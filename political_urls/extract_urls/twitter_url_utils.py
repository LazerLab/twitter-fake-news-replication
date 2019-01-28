import re
from tweetData import extract_tweet_info
import requests
import lxml.html
from lxml.cssselect import CSSSelector

TWEET_URL_PATTERN = re.compile('.*twitter\.com/(.*)/status/(\d+)($|/.*)$')
URL_PATTERN = re.compile('https?://([^/]+)')


def get_ext_status_ents(status):
    if status is None:
        return {}
    return status['extended_tweet']['entities'] if 'extended_tweet' in status \
        else status['entities']


URL_FIELDS = ['tweet_id', 'user_id', 'tweet_date', 'retweet_prefix', 'tweet_text',
              'quoted_text', 'retweet_of_tweet_id', 'retweet_of_user_id',
              'quote_of_tweet_id', 'quote_of_user_name', 'reply_to_tweet_id',
              'candidate_interaction', 'where_url_found', 'who_url_from',
              'link_type', 'shortened_url', 'canonical_url', 'website', 'title',
              'date_published', 'description', 'author']


def extract_urls_from_tweet(t, skip_internal_twitter=False, expand_links=3):

    # # constructs a dictionary containing the fields we'll print
    # tweet_level_info = tweetData.extract_tweet_info(t)

    ##############################################
    res = []

    quote = t.get('quoted_status', None)
    rt = t.get('retweeted_status', None)

    tid = t.get('id_str', '')
    tweet_info = None
    # has_quote = False   # a RT of a quote has is_quote_status=True, but
    # needs to be handled as a RT

    orig_ents = get_ext_status_ents(t)
    rt_ents = get_ext_status_ents(rt)
    quoted_ents = get_ext_status_ents(quote)

    self_and_quoted_ids = [tid, quote['id_str']
                           ] if quote is not None else [tid]
    if rt is None:
        urls = [('orig', False, url) for url in orig_ents.get('urls', [])] + \
            [('orig', True, url) for url in orig_ents.get('media', [])]
    else:
        urls = [('retweeted', False, url) for url in rt_ents.get('urls', [])] + \
            [('retweeted', True, url) for url in rt_ents.get('media', [])]
    urls = urls + \
        [('quoted', False, url) for url in quoted_ents.get('urls', [])] + \
        [('quoted', True, url) for url in quoted_ents.get('media', [])]

    for url_type, is_media, url_struct in urls:
        expanded_url = url_struct.get('expanded_url', None)
        if expanded_url is None or not isinstance(expanded_url, basestring):
            continue

        # twitter url parts
        m = TWEET_URL_PATTERN.match(expanded_url)
        if m is not None:  # this is a twitter url
            tweeted_status_owner, tweeted_status_id, tweeted_status_ext = m.groups()
            is_tweeted_status = tweeted_status_ext == '' or tweeted_status_ext == '/'
            # skip if skip_internal_twitter or it's a self-referential tweet
            # url and links to quoted status outside the quoted_status
            if skip_internal_twitter or (is_tweeted_status and not is_media and
                                         tweeted_status_id in self_and_quoted_ids and
                                         (url_type == 'orig' or url_type == 'retweeted')
                                         ):
                continue
            rec = extract_tweet_info(
                t) if tweet_info is None else tweet_info.copy()
            if 'reply_to_user_id' in rec:
                del rec['reply_to_user_id']
            url_orig = next((x['user']['screen_name']
                             for x in [rt, quote] if x is not None), '')
            rec.update({
                'where_url_found': url_type,
                'who_url_from': url_orig,
                'link_type': 'twitter_status' if is_tweeted_status else 'twitter_media',
                'shortened_url': url_struct['url'],
                'canonical_url': expanded_url,
                'website': 'twitter.com',
                'author': tweeted_status_owner if tweeted_status_owner != 'i/web' else ''
            })
            res.append(rec)
        else:
            link_type = 'external'
            rec = extract_tweet_info(
                t) if tweet_info is None else tweet_info.copy()
            if 'reply_to_user_id' in rec:
                del rec['reply_to_user_id']
            url_orig = next((x['user']['screen_name']
                             for x in [rt, quote] if x is not None), '')
            domain_match = URL_PATTERN.match(expanded_url)
            rec.update({
                'where_url_found': url_type,
                'who_url_from': url_orig,
                'link_type': link_type,
                'shortened_url': url_struct['url'],
                'canonical_url': expanded_url,
                'website': domain_match.group(1) if domain_match is not None else None
            })
            res.append(rec)

    return res


class TimeoutError(Exception):
    """ Timeout function modified from:
        http://stackoverflow.com/questions/492519/timeout-on-a-function-call
        and http://stackoverflow.com/questions/11901328/how-to-timeout-function-in-python-timeout-less-than-a-second
    """
    pass


def timeout(func, args=(), kwargs=None, timeout_duration=1, default=None, throw_exception=False):
    kwargs = kwargs or {}

    import signal

    def handler(signum, frame):
        raise TimeoutError()

    # set the timeout handler
    old_handler = signal.signal(signal.SIGALRM, handler)
    # signal.alarm(timeout_duration)    # this one only allows integer timeout
    # values
    signal.setitimer(signal.ITIMER_REAL, timeout_duration)
    try:
        result = func(*args, **kwargs)
    except TimeoutError as exc:
        result = default
        if throw_exception:
            raise exc
    finally:
        signal.alarm(0)
        signal.signal(signal.SIGALRM, old_handler)

    return result


def fetch_html(expanded_url, turnOffSSL=False, fastOnlyExpandURL=False):
    """ Fetches the html of a url, following redirects and all.
        Handles three types of errors:
        1. Error reaching final page (malformed website; HTTP errors in any of the redirects or
           at the final page)
        2. Error downloading page content or determining encoding (certain large non-html files
           take up ridiculous amounts of time and memory when r.text is called)
        3. Errors reading html (e.g., malformed Unicode or more encoding errors)
        Returns a dictionary that contains:
        'landing_url' - the url after following all redirects
        'err' - if something went wrong
        'is_html' - 1/0 if content type is html
        'content_length' - when too large (>1000000)
        'html' - the html content! """
    res = {}
    try:
        # Just use head for now: it's faster
        rh = requests.head(expanded_url, timeout=5,
                           allow_redirects=True, verify=not turnOffSSL)
        # special case for 405, Method not allowed -- it might simply not
        # support "head." Try the full thing.
        if rh.status_code == 405:
            rh = requests.get(expanded_url, timeout=5, verify=not turnOffSSL)
        if rh.status_code != requests.codes.ok:
            res['err'] = "HTTP status " + str(rh.status_code)
        if rh.url is not None:
            res['landing_url'] = rh.url
    except requests.exceptions.SSLError as e:
        if not turnOffSSL:
            return fetch_html(expanded_url, turnOffSSL=True, fastOnlyExpandURL=fastOnlyExpandURL)
        # still gives an error, even with the flag
        res['err'] = e.__class__.__name__ + ": " + str(e.message)
    except requests.exceptions.RequestException as e:
        # These are commonly ReadTimeout or ConnectionError (for invalid URLs)
        res['err'] = e.__class__.__name__ + ": " + str(e.message)

    if 'err' in res:
        return res

    # Will only download the page if it's html and not too large (1MB or more - probably not HTML)
    res['is_html'] = int('html' in rh.headers.get('content-type', 'html').lower())
    res['content_length'] = int(rh.headers.get('content-length', '-1'))
    if res['is_html'] == 0 or res['content_length'] >= 1000000 or fastOnlyExpandURL:
        return res

    # Go for more fields!
    try:
        # hopefully can still hit the page we hit above
        r = requests.get(res.get('landing_url', expanded_url), timeout=5, verify=not turnOffSSL)
        if r.status_code != requests.codes.ok:
            res['err'] = "HTTP status " + str(r.status_code)
            return res

        # This is where URLs may take a long time & memory. Time out after 10 sec.
        # Switched to calling r.content instead of r.text ==> html parser will handle encoding.
        res['html'] = timeout(requests.Response.content.__get__, args=[r],
                              timeout_duration=10, throw_exception=True)
        if len(res['html']) < 1:
            res['err'] = 'empty page'
            del res['html']
        # else:
        #     res['html'] = unicode(res['html'].encode('utf8'))
    except TimeoutError:
        res['err'] = 'Timed out while reading page content'
    except Exception as e:
        # Encoding errors will show up here.
        res['err'] = e.__class__.__name__ + ": " + str(e.message)

    return res


meta_selectors = {
    'description': ['head > meta[name="description"]', 'head > meta[property="og:description"]', 'head > meta[name="twitter:description"]', 'head > meta[itemprop="description"]'],
    'author': ['head > meta[name="author"]', 'head > meta[property="article:author"]', 'head > meta[name="twitter:creator"]'],
    'title': ['head > meta[name="title"]', 'head > meta[property="og:title"]', 'head > meta[name="twitter:title"]', 'head > meta[itemprop="name"]', 'head > title'],
    'canonical_url': ['head > link[rel="canonical"]', 'head > meta[property="og:url"]', 'head > meta[name="twitter:url"]'],
    'date_published': ['head > meta[property="article:published_time"]']
}

prop2selectors = {}
sel2value_attr = {}
bad_chars = re.compile('[\r\n\t]+')


def parse_page(html, meta_selectors=meta_selectors):
    global prop2selectors
    global sel2value_attr

    res = {}
    # initialize CSS selector objects on first call
    if len(prop2selectors) < 1:
        for prop, sels in meta_selectors.iteritems():
            objs = []
            for sel in sels:
                sel_obj = CSSSelector(sel)
                objs.append(sel_obj)
                if 'link' in sel:
                    sel2value_attr[sel_obj] = 'href'
                elif 'meta' in sel:
                    sel2value_attr[sel_obj] = 'content'
                else:
                    sel2value_attr[sel_obj] = None
            prop2selectors[prop] = objs

    doc = lxml.html.fromstring(html)
    # for each key in meta_selectors, try to get a value for it by trying all its css selectors in order
    for prop, sels in prop2selectors.iteritems():
        val = u''
        for sel in sels:
            elems = sel(doc)
            if len(elems) < 1:
                continue
            if prop == 'pagination' and sel2value_attr[sel] is None:
                val = 'pagination_elem_found'
            elif prop == 'title' and sel2value_attr[sel] is None:
                # changes bad characters to spaces; assumes input is unicode(?), and changes it to utf-8
                val = unicode(','.join([bad_chars.sub(' ', lxml.html.tostring(elem, method='text', encoding='unicode')).strip() for elem in elems])).encode("utf-8")
            else:
                val_attr = sel2value_attr[sel]
                tmp_array = []
                for elem in elems:
                    if val_attr in elem.attrib:
                        tmp_array.append(bad_chars.sub(' ', elem.attrib[val_attr]).strip())
                val = unicode(','.join(tmp_array)).encode("utf-8")
            break
        if len(val) > 0:
            res[prop] = val

    # If canonical url we extracted is blank or malformed, fall back to the version we know worked
    if 'canonical_url' in res and \
       (len(res['canonical_url']) == 0 or URL_PATTERN.match(res['canonical_url']) is None):
        del res['canonical_url']

    return res
