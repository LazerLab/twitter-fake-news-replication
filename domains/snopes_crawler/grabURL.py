import requests
import lxml.html
from lxml.cssselect import CSSSelector
import re




# This version traps and handles the three types of errors seen (empirically):
# 1. Error reaching final page (malformed website; HTTP errors in any of the redirects or at the final page)
# 2. Error downloading page content or determining encoding (certain large non-html files take up ridiculous amounts
# of time and memory when r.text is called)
# 3. Errors reading html (e.g., malformed Unicode or more encoding errors)
# Return value is a dictionary. Contains keys:
# 'initial_url' -- always
# 'canonical_url' -- if it successfully navigated the redirects
# 'error' -- if something went wrong
# The additional fields will be present (possibly blank) if no 'error'.
def safeGetURLAndMetadata(url, turnOffSSL = False, skipFirstCanonical = False, fastOnlyExpandURL = False):
    row = {'initial_url': url}

    # Find out what page to go to
    try:
        # Just use head for now: it's faster
        rh = requests.head(url, timeout=5, allow_redirects=True, verify=not turnOffSSL)
        # special case for 405, Method not allowed -- it might simply not support "head." Try the full thing.
        if rh.status_code == 405:
            rh = requests.get(url, timeout=5, verify=not turnOffSSL)
        # elif rh.status_code ==
        row['canonical_url'] = rh.url
        if rh.status_code != requests.codes.ok:
            row['error'] = "HTTP status " + str(rh.status_code)

    except requests.exceptions.SSLError as e:
        if turnOffSSL:
            # still gives an error, even with the flag
            row['error'] = e.__class__.__name__ + ": " + str(e.message)
        else:
            return safeGetURLAndMetadata(url, turnOffSSL = True, skipFirstCanonical = skipFirstCanonical)

    except requests.exceptions.RequestException as e:
        # These are commonly ReadTimeout or ConnectionError (for invalid URLs)
        row['error'] = e.__class__.__name__ + ": " + str(e.message)

    if row.has_key('error'):
        return row

    # Should we download the page? Only if it's html and not too large
    isHTML = True
    if rh.headers.get('content-type') and ("html" not in rh.headers['content-type'].lower()):
        # they explicitly state content-type, and it's not html
        isHTML = False
    isTooLong = False
    if rh.headers.get('content-length') and (int(rh.headers['content-length']) >= 1000000):
        # they say it's going to be 1MB or more (= probably not HTML)
        isTooLong = True
    if (not isHTML) or isTooLong or fastOnlyExpandURL:
        return row

    # Go for more fields!
    try:
        r = requests.get(url, timeout=5, verify = not turnOffSSL)    # hopefully can still hit the page we hit above
        if r.status_code != requests.codes.ok:
            row['error'] = "HTTP status " + str(r.status_code)
            return row

        # This is the moment some URLs provoke large time & memory use.  Kill it after 10 sec.
        # Call r.content instead of r.text. This lets lxml be the one to take on the encoding challenge.
        page_content = timeout(requests.Response.content.__get__, args=[r], timeout_duration=10, throw_exception=True)

        if len(page_content) == 0:
            row['error'] = "empty page"
            return row

        # XML-parsing errors may be thrown
        doc = lxml.html.fromstring(page_content)

    except TimeoutError:
        row['error'] = "Timed out while reading page content"
    except Exception as e:
        # Encoding errors will show up here.
        row['error'] = e.__class__.__name__ + ": " + str(e.message)

    if row.has_key('error'):
        return row

    # If that all worked, we're almost home free

    # for each key in meta_selectors, try to get a value for it by trying all its css selectors in order
    for prop, sels in prop2selectors.iteritems():
        val = u''
        # hack for linkis.com URLs
        if skipFirstCanonical and prop == "canonical_url":
            sels = sels[1:]

        for sel in sels:
            elems = sel(doc)
            if len(elems) < 1:
                continue
            if prop=='pagination' and sel2value_attr[sel] is None:
                val = 'pagination_elem_found'
            elif prop=='title' and sel2value_attr[sel] is None:
                # changes bad characters to spaces; assumes input is unicode(?), and changes it to utf-8
                val = unicode(','.join([bad_chars.sub(' ', lxml.html.tostring(elem, method='text', encoding='unicode')).strip() for elem in elems])).encode("utf-8")
            else:
                val_attr = sel2value_attr[sel]
                tmp_array = []
                for elem in elems:
                    if elem.attrib.has_key(val_attr):
                        tmp_array.append(bad_chars.sub(' ', elem.attrib[val_attr]).strip())
                val = unicode(','.join(tmp_array)).encode("utf-8")
            break
        row[prop] = val

    # If canonical url we extracted is blank or malformed, fall back to the version we know worked
    if len(row['canonical_url']) == 0 or (not website_pattern.match(row['canonical_url'])):
        row['canonical_url'] = r.url

    return row



# Timeout function modified from: http://stackoverflow.com/questions/492519/timeout-on-a-function-call
# and http://stackoverflow.com/questions/11901328/how-to-timeout-function-in-python-timeout-less-than-a-second
class TimeoutError(Exception):  # moved this part outside timeout() so I can use it later
    pass

def timeout(func, args=(), kwargs=None, timeout_duration=1, default=None, throw_exception=False):
    kwargs = kwargs or {}

    import signal

    def handler(signum, frame):
        raise TimeoutError()

    # set the timeout handler
    old_handler = signal.signal(signal.SIGALRM, handler)
    # signal.alarm(timeout_duration)    # this one only allows integer timeout values
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


meta_selectors = {
    'keywords': ['head > meta[name="news_keywords"]', 'head > meta[name="keywords"]'],
    'description': ['head > meta[name="description"]', 'head > meta[property="og:description"]', 'head > meta[name="twitter:description"]', 'head > meta[itemprop="description"]'],
    'author': ['head > meta[name="author"]', 'head > meta[property="article:author"]', 'head > meta[name="twitter:creator"]'],
    'title': ['head > meta[name="title"]', 'head > meta[property="og:title"]', 'head > meta[name="twitter:title"]', 'head > meta[itemprop="name"]', 'head > title'],
    'canonical_url': ['head > link[rel="canonical"]', 'head > meta[property="og:url"]', 'head > meta[name="twitter:url"]'],
    'type': ['head > meta[property="og:type"]', 'head > meta[name="twitter:card"]'],
    'pub_time': ['head > meta[property="article:published_time"]'],
    #'section': ['head > meta[property="article:section"]', 'head > meta[property="vr:category"]'],
    #'tag': ['head > meta[property="article:tag"]'],
    #'pagination': ['head > link[rel="next"]', 'head > link[rel="prev"]', 'div#pagination', 'div.tool-pages', '.pagination-widget']
}

prop2selectors = {}
sel2value_attr = {}
bad_chars = re.compile('[\r\n\t]+')
website_pattern = re.compile('https?://([^/]+)')
def init():
    global meta_selectors
    global prop2selectors
    global sel2value_attr
    global bad_chars
    global website_pattern
    for prop,sels in meta_selectors.iteritems():
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



if __name__ == "__main__":
    init()
    # print safeGetURLAndMetadata("https://t.co/lfRIoA8V7U")
    # print safeGetURLAndMetadata("https://t.co/eqfBHQpHZC")
    # print safeGetURLAndMetadata("https://t.co/W0LwCLZxyo")
    # print safeGetURLAndMetadata("https://t.co/969OuG8efu")
    # print safeGetURLAndMetadata("https://t.co/14I0iH1c3t")
    # print safeGetURLAndMetadata("https://t.co/Xk27aFm2fv")
    # print safeGetURLAndMetadata("https://t.co/wFcqbtq8Pd")
    # print safeGetURLAndMetadata("https://t.co/ZmjQaM4ehO")
    print safeGetURLAndMetadata("https://t.co/MkGU2eE56T")


