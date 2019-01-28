import luigi
import luigi_orm_targets as lorm
import subprocess
import sys
import os
import unicodecsv as csv
import datetime
import ujson as json
import gzip
import fcntl
import redis
import itertools
import traceback
import random
import re
import twitter_url_utils as tuu


class globalconfig(luigi.Task):
    gardenhose_dir = luigi.Parameter()
    input_base_dir = luigi.Parameter()
    mysql_db = luigi.Parameter()
    redis_host = luigi.Parameter()
glbl = globalconfig()


class FilterURLs(RemoveOnFailureMixin, luigi.Task):
    date = luigi.DateParameter(default=datetime.date.today())
    input_dir = luigi.Parameter(default=glbl.input_base_dir)
    output_dir = luigi.Parameter(default=os.path.join(
        glbl.input_base_dir, 'with_urls'))

    def requires(self):
        return FilterFriendsTweets(self.date)

    def output(self):
        target = luigi.LocalTarget(os.path.join(
            self.output_dir, "{}.tsv.gz".format(self.date)))
        target.makedirs()
        return target

    def run(self):
        input_path = os.path.join(
            glbl.input_base_dir, "{}.json.gz".format(self.date))
        with gzip.open(input_path, 'r') as fin, gzip.open(self.output().path, 'wb') as fout:
            wrtr = csv.DictWriter(fout, tuu.URL_FIELDS, delimiter='\t',
                                  quoting=csv.QUOTE_NONE, escapechar='\\')
            wrtr.writeheader()

            for line in fin:
                tweet = json.loads(line.decode("utf8"))
                map(wrtr.writerow, tuu.extract_urls_from_tweet(tweet))


class FilterPoliticalURLs(RemoveOnFailureMixin, luigi.Task):
    date = luigi.DateParameter(default=datetime.date.today())

    def requires(self):
        return FilterURLs(self.date)

    def output(self):
        target = luigi.LocalTarget(os.path.join(os.path.dirname(
            self.input().path), 'political', '{}.tsv.gz'.format(self.date)))
        target.makedirs()
        return target

    # def program_args(self):
    def run(self):
        script_path = '../political_classifier/politicalFilterURLData.R'
        whitelist_path = '../political_classifier/whitelist.politics3.txt'
        model_path = self.output().path[:-7]
        cmd = [
            'Rscript', '-e', 'source("{}", chdir = TRUE)'.format(script_path), '-e',
            'filterURLDataUsingClassifier("{}", "{}", "{}", modelFileOutStem="{}")'.format(
                self.input().path, self.output().path[:-3], whitelist_path, model_path)
        ]
        print 'Running command: ' + ' '.join(cmd)
        # try:
        proc = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        for line in iter(proc.stdout.readline, ''):
            sys.stdout.write(line)

        print 'Subprocess finished with return code ' + str(proc.wait())



class FetchHTMLs(luigi.Task):
    date = luigi.DateParameter(default=datetime.date.today())
    db_conn = luigi.Parameter(default=glbl.mysql_db)
    redis_host = luigi.Parameter(default=glbl.redis_host)

    def requires(self):
        return FilterPoliticalURLs(self.date)

    def output(self):
        db_trgt = lorm.HTMLTarget(self.db_conn, ds=str(self.date))
        f_trgt = luigi.LocalTarget(os.path.join(
            os.path.dirname(self.input().path), 'fetched_urls', '{}.complete'.format(self.date)))
        f_trgt.makedirs()
        return db_trgt, f_trgt

    def run(self):
        db_trgt, f_trgt = self.output()
        r = redis.StrictRedis(host=self.redis_host, port=6379)
        with gzip.open(self.input().path, 'r') as fin, db_trgt as db:
            rdr = csv.DictReader(fin, delimiter='\t', quoting=csv.QUOTE_NONE, escapechar='\\')
            db.create_table()
            proccessed_cnt, exc_cnt, err_cnt, dup_cnt, fetch_cnt = 0, 0, 0, 0, 0
            for url in rdr:
                if proccessed_cnt % 1000 == 0:
                    msg = "ds = %s: processed %d, excluded %d, in db %d, error %d, fetched %d" % \
                        (str(self.date), proccessed_cnt, exc_cnt, dup_cnt, err_cnt, fetch_cnt)
                    self.set_status_message(msg)
                    print msg
                proccessed_cnt += 1
                expanded_url = url.get('canonical_url', None)
                if url['link_type'] != 'external' or expanded_url is None or len(expanded_url) < 7:
                    exc_cnt += 1
                    continue

                try:
                    expanded_url_enc = expanded_url.encode('utf-8')
                    # if expanded_url_enc in db:
                    key_is_new = r.setnx(expanded_url_enc, "0")
                    if not key_is_new:
                        dup_cnt += 1
                        continue
                    # print 'Fetching url started at', datetime.datetime.now()
                    scrape_res = tuu.fetch_html(expanded_url)
                    # print 'Fetching finished at', datetime.datetime.now()
                    # print 'HTML length', len(scrape_res.get('html', ''))
                    db.add(lorm.HTML(expanded_url=expanded_url_enc,
                                     ds=str(self.date), **scrape_res))
                    # print 'Inserting to DB finished at', datetime.datetime.now()
                    fetch_cnt += 1
                except Exception as e:
                    err_cnt += 1
                    print 'Fetching HTML error: ' + e.__class__.__name__ + ': ' + str(e.message)

        with f_trgt.open('wb'):
            pass


class ParseHTMLs(luigi.Task):
    date = luigi.DateParameter(default=datetime.date.today())
    db_conn = luigi.Parameter(default=glbl.mysql_db)

    def requires(self):
        return FetchHTMLs(self.date)

    def output(self):
        db_trgt = lorm.ParsedHTMLTarget(self.db_conn, ds=str(self.date))
        f_trgt = luigi.LocalTarget(os.path.join(
            os.path.dirname(self.input()[1].path), 'parsed_urls', '{}.complete'.format(self.date)))
        f_trgt.makedirs()
        return db_trgt, f_trgt

    def run(self):
        db_trgt, f_trgt = self.output()
        with self.input()[0] as in_db, db_trgt as out_db:
            out_db.create_table()
            parse_err_cnt = 0
            print 'Querying DB at', datetime.datetime.now()
            for i, row in enumerate(in_db.iter_not_parsed(yield_per=100)):
                if (i + 1) % 1000 == 0:
                    self.set_status_message("Processed %d urls, %d parsing errors." %
                                            (i, parse_err_cnt))
                print 'Parsing url at', datetime.datetime.now()
                try:
                    metadata = {}
                    if row.html is not None:
                        metadata = tuu.parse_page(row.html)
                    final_url = metadata.get('canonical_url', None)
                    if final_url is None:
                        final_url = row.landing_url
                    if final_url is None:
                        final_url = row.expanded_url
                    domain_m = tuu.URL_PATTERN.match(final_url)
                    domain = None if domain_m is None else domain_m.group(1)
                    out_db.add(lorm.ParsedHTML(
                        expanded_url=row.expanded_url,
                        landing_url=row.landing_url,
                        website=domain,
                        ds=row.ds,
                        ts=row.ts,
                        **metadata
                    ))
                except Exception as e:
                    parse_err_cnt += 1
                    print 'error #' + str(parse_err_cnt) + ' ' + \
                        e.__class__.__name__ + ': ' + str(e.message)
        with f_trgt.open('wb'):
            pass


class MergeTweetsWithParsedPoliticalURLs(RemoveOnFailureMixin, luigi.Task):
    date = luigi.DateParameter(default=datetime.date.today())

    def requires(self):
        return FilterPoliticalURLs(self.date), ParseHTMLs(self.date)

    def output(self):
        target = luigi.LocalTarget(os.path.join(os.path.dirname(
            self.input()[0].path), 'fetched', '{}.tsv.gz'.format(self.date)))
        target.makedirs()
        return target

    def run(self):
        in_trgt, (db_trgt, _) = self.input()
        with gzip.open(in_trgt.path, 'r') as fin, gzip.open(self.output().path, 'wb') as fout,\
                db_trgt as db:
            rdr = csv.DictReader(fin, delimiter='\t', quoting=csv.QUOTE_NONE, escapechar='\\')
            wrtr = csv.DictWriter(fout, rdr.fieldnames,
                                  delimiter='\t', quoting=csv.QUOTE_NONE, escapechar='\\')
            wrtr.writeheader()

            for url in rdr:
                expanded_url = url.get('canonical_url', None)
                if url['link_type'] != 'external' or expanded_url is None or len(expanded_url) < 7:
                    continue
                expanded_url_enc = expanded_url.encode('utf-8')
                parsed = db.get_row_by_key(expanded_url_enc)
                if parsed is None:
                    continue

                if parsed.canonical_url is not None:
                    url['canonical_url'] = parsed.canonical_url
                elif parsed.landing_url is not None:
                    url['canonical_url'] = parsed.landing_url
                url['website'] = parsed.website
                url['title'] = parsed.title
                url['date_published'] = parsed.date_published
                url['description'] = parsed.description
                url['author'] = parsed.author

                wrtr.writerow(url)


