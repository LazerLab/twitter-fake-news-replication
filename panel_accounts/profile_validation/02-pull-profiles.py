import time
import tweepy
import sys
import twitterAuthLDF
import unicodecsv as csv


# Quick program that uses Twitter API to grab current profiles for a list of users.
# To use: you must replace 'twitterAuthLDF.getAPIobj()' with a call that authenticates your account.

def main():
    infile = 'private_data/panel_sample.csv'
    outfile = 'private_data/panel_sample_profiles.csv'

    APIobj = twitterAuthLDF.getAPIobj()

    with open(infile) as csv_file:
        csv_reader = csv.reader(csv_file, delimiter=',')
        header = csv_reader.next() # gobble
        grabProfile(csv_reader, APIobj, outfile)


def grabProfile(csv_reader, api, outfile):

    with open(outfile, 'w') as outfilefile:
        writer = csv.writer(outfilefile)
        writer.writerow(['user_id', 'small_pic', 'profile_pic','screen_name','name','location'])

        while True:
            try:
                row = csv_reader.next(None)  # throws Exception to end the program :(
                if (row is None):
                    break
                user = api.get_user(user_id=row[0])
                big_pic = ''.join(user.profile_image_url.split('_normal'))
                writer.writerow([row[0], user.profile_image_url, big_pic, user.screen_name, user.name, user.location])
            except tweepy.RateLimitError:
                sys.stderr.write("(Rate limit -- waiting 15 min)\n")
                time.sleep(15 * 60)
            except tweepy.TweepError:
                sys.stderr.write("Got Twitter error, skipping user " + row[0] + "\n")



if __name__ == "__main__":
    main()
