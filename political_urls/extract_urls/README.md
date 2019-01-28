# extract_urls

Main steps for extracting political URLs from raw tweets (JSON files):

  * Convert JSON files to tab-separated format, with one row per URL in each tweet.
  * Train and apply classifier for political tweets. Save as a tab-separated file containing one row per political URL.
  * Visit each URL to see what domain it resolves to (after following any redirects from link shorteners, etc.) and save the HTML.
  
The code in `luigi_pipeline.py` was used to perform these steps. It relies on the following infrastructure:

  * The [Luigi module](https://github.com/spotify/luigi) is used to run the whole pipeline on data for each day. Whole pipeline = the `.run()` methods, in turn, of `FilterURLs`, `FilterPoliticalURLs`, `FetchHTMLs`, `ParseHTMLs`, and `MergeTweetsWithParsedPoliticalURLs`.
  * As input, each day's raw tweets are stored in a separate file.
  * For efficiency regarding repeated URLs, we expand each URL only once, storing the results (HTML, domain, etc.) in a database, and then merge them back in.
  
Even without this infrastructure, the main functionality can be inspected and recreated by putting together code from the `.run()` methods above.

A standalone repository with some of the same code and functionality, and additional documentation, can be accessed at [github.com/lfriedl/tweet-topic-filter]("https://github.com/lfriedl/tweet-topic-filter").
