
import re

TRUMP_UID = 25073877
CLINTON_UID = 1339835893
trump_search = re.compile(r"\b(donald|trump|#trump)\b", re.U | re.I)
clinton_search = re.compile(r"\b(hillary|hilary|clinton|#hillary)\b", re.U | re.I)

# Pull out non-URL data about tweet.
# Note: assumes json is in the tweet_mode=extended (or whatever the exact setting is) that gives it the field 'full_text',
# not 'text'.
def extract_tweet_info(json):
    data = {'user_id': json['user']['id_str'],
            'tweet_id': json['id_str'],
            'tweet_date': json['created_at'],
            'tweet_text': '',
            'retweet_prefix': '',
            'quoted_text': '',
            'retweet_of_tweet_id': '',
            'retweet_of_user_id': '',
            'quote_of_tweet_id': '',
            'quote_of_user_id': '',
            'quote_of_user_name': '',
            'reply_to_tweet_id': '',
            'reply_to_user_id': ''
            }

    is_retweet = json.has_key('retweeted_status')
    is_reply = (json.has_key('in_reply_to_status_id_str') and json['in_reply_to_status_id_str'] is not None) or \
               (json.has_key('in_reply_to_user_id_str') and json['in_reply_to_user_id_str'] is not None)
    is_quote = json['is_quote_status']

    data['tweet_text'] = get_text_field(json)

    if is_retweet:   # grab original (non-truncated) text, plus save the "RT @username: " prefix
        data['retweet_prefix'] = 'RT @' + json['retweeted_status']['user']['screen_name'] + ": "
        data['tweet_text'] = get_text_field(json['retweeted_status'])
        data['retweet_of_tweet_id'] = json['retweeted_status']['id_str']
        data['retweet_of_user_id'] = json['retweeted_status']['user']['id_str']

    if is_reply:
        if json.has_key('in_reply_to_status_id_str') and json['in_reply_to_status_id_str'] is not None:
            data['reply_to_tweet_id'] = json['in_reply_to_status_id_str']
        if json.has_key('in_reply_to_user_id_str') and json['in_reply_to_user_id_str'] is not None:
            data['reply_to_user_id'] = json['in_reply_to_user_id_str']

    if is_quote:
        # quoted_status stuff may be missing, e.g. if initial status has been deleted
        if json.has_key('quoted_status_id_str'):
            data['quote_of_tweet_id'] = json['quoted_status_id_str']

        # go get the text being quoted and user

        if is_retweet and json['retweeted_status'].has_key('quoted_status'):
            data['quote_of_user_id'] = json['retweeted_status']['quoted_status']['user']['id_str']
            data['quote_of_user_name'] = json['retweeted_status']['quoted_status']['user']['screen_name']
            data['quoted_text'] = get_text_field(json['retweeted_status']['quoted_status'])
        else:
            if json.has_key('quoted_status'):
                data['quote_of_user_id'] = json['quoted_status']['user']['id_str']
                data['quote_of_user_name'] = json['quoted_status']['user']['screen_name']
                data['quoted_text'] = get_text_field(json['quoted_status'])

    data['candidate_interaction'] = "||".join(tweet_about_clinton_or_trump(json, data))
    del data['quote_of_user_id']   # not currently wanted in output (but needed it to check for interaction)

    return data

# gets top-level text field, either full_text (optionally dipping into extended_tweet) or text
bad_chars = re.compile('[\r\n\t]+')
def get_text_field(json):
    txt = ''
    if 'full_text' in json:
        txt = json['full_text']
    elif 'extended_tweet' in json and 'full_text' in json['extended_tweet']:
        txt = json['extended_tweet']['full_text']
    else:
        txt = json.get('text', '')
    return bad_chars.sub(' ', txt)

# Borrowed from Kenny & modified
def tweet_about_clinton_or_trump(t, data):
    res = set()
    check_for_interaction_type(t['in_reply_to_user_id_str'], "ReplyTo", res)
    if data.has_key('quote_of_user_id'):
        check_for_interaction_type(data['quote_of_user_id'], "Quote", res)  # using data b/c it catches RT of quote
    if data.has_key('retweet_of_user_id'):
        check_for_interaction_type(data['retweet_of_user_id'], "RT", res)

    t_mentions = get_mentions(t)

    if not contains(res, "Trump") and TRUMP_UID in t_mentions:
        res.add("@Mention_Trump")
    if not contains(res, "Clinton") and CLINTON_UID in t_mentions:
        res.add("@Mention_Clinton")

    # new: to search for text mentions, look in full_text + quoted_text.
    # no need to look in RT prefix--can't be there.
    if trump_search.search(data['tweet_text'] + " " + data['quoted_text']):
        res.add("TextMention_Trump")
    if clinton_search.search(data['tweet_text'] + " " + data['quoted_text']):
        res.add("TextMention_Clinton")

    return res


def check_for_interaction_type(interaction_id, interaction_type, res):
    if not interaction_id:
        return
    if int(interaction_id) == TRUMP_UID:
        res.add(interaction_type + "_Trump")
    if int(interaction_id) == CLINTON_UID:
        res.add(interaction_type + "_Clinton")


def contains(res, term):
    return len(res) and reduce(lambda x, y: x | y, [term in x for x in res])

def get_mentions(tweet_json):
    if tweet_json.has_key('extended_tweet'):
        orig_entities = tweet_json['extended_tweet']['entities']
    else:
        orig_entities = tweet_json['entities']

    return [entity['id'] for entity in orig_entities['user_mentions'] if 'id' in entity]
