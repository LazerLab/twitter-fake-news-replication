source("classifyTweets.R")

# Wrapper for callin classifyTweets on URL data.
# Builds 1 model per input file. Assumes input file fits in memory.
# Work of the wrapper: 
# -Changes data rows from tweet-URLs to tweets (for classification) and back again.
# -Constructs tweet text "complete_raw_text" from input columns (includes RT, quoted handles and expanded URLs).


# inFile: a tsv.gz (or similar) containing columns tweet_id, tweet_text (and retweet_prefix, quote_of_user_name and quoted_text) and canonical_url.
#   Repeats same tweet on multiple rows (assumed to be sequential!), once per URL. (Doesn't matter if it has only "external" URLs or also twitter_* ones.)
# outFile: a tsv file with same fields as inFile + political_classifier_score and white_terms. See classifierThreshold for which lines.
#   If classifierThreshold is NULL (implied if saveDebuggingColumnsRows is TRUE), keeps all rows.
# keywordFile: used to define the training set, one term (word or phrase) per line. If a tweet contains one of these terms, it's ALWAYS kept (so, 
#   be sure to put only high-precision terms in this file.)
# modelFileOutStem: if non-NULL, we'll save two .rds files beginning with this path + prefix, to allow using same model (including doc->term mapping) on future tweets
#   (plus a 3rd file == a plot of training set scores)
# classifierThreshold: Normally, keep rows with scores above this value. If it's NULL or if saveDebuggingColumnsRows=T, then keep all rows.
# saveDebuggingColumnsRows: flag for printing larger outfile to inspect scores.
filterURLDataUsingClassifier = function(inFile, outFile, keywordFile, modelFileOutStem=NULL, classifierThreshold=.8, saveDebuggingColumnsRows=FALSE,
                                        goFastUseNaiveBayes=F, noTraining=F, loadVectorizer=F) {
    
    startTime = Sys.time()
    print(paste("Run started at", startTime))
    
    # 1. Read data, change to tweet level (from tweet-url level), match against whitelist, tokenize, and build DocumentTermMatrix.
    inData = fread(paste("zcat", inFile), sep = "\t", quote="")
    tweetData = constructTweets(inData)    # contains tweet_id, complete_raw_text
    print(paste("Done constructing tweets at", Sys.time() - startTime))
    
    # 2. All the classification work! Construct training set, tokenize text, train classifier, get predictions from classifier.
    tweetData = classifyTweets(keywordFile, tweetData = tweetData[, .(tweet_id, complete_raw_text)],  modelFileOutStem, startTime, goFastUseNaiveBayes, 
                                noTraining=noTraining, loadVectorizer=loadVectorizer)
    
    # 3. Change back to tweet-url level, apply threshold for filtering, save
    dataWithScores = merge(inData, tweetData, by="tweet_id")
    
    colsWanted = colnames(dataWithScores)
    if (saveDebuggingColumnsRows) {
        classifierThreshold = NULL
    } else {
        # all we add to original is white_terms and political_classifier_score
        colsWanted = setdiff(colsWanted, c("classifier_label", "complete_raw_text", "rest_of_text", "raw_classifier_score", "white_terms"))
    }
    if (! is.null(classifierThreshold)) {
        print(paste("Keeping the", sum(dataWithScores$political_classifier_score >= classifierThreshold), "of", nrow(dataWithScores), 
                    "lines with a classifier score >=", classifierThreshold))
        dataWithScores = dataWithScores[political_classifier_score >= classifierThreshold,]
    }
    fwrite(dataWithScores[, colsWanted, with=F], file=outFile, sep="\t", quote=F)
    system(paste("gzip", outFile))
    
}


# Gathers text + URLs for each tweet
# inData: a data.table (straight from the inFile)
# returns: a data.table with columns tweet_id and tokenized_text 
constructTweets = function(inData) {
    
    # text: Paste back together the RT prefix, the tweet_text, and the quoted_text.
    firstRowEachTweet = !duplicated(inData$tweet_id)
    inData$quoting = ifelse(inData$quote_of_user_name == '', '', paste0("[QTG @", inData$quote_of_user_name, "]"))
    tweetData = inData[firstRowEachTweet, .(tweet_id, raw_text = paste(retweet_prefix, tweet_text, quoting, quoted_text))]
    # urls: 
    tweetURLs = inData[, .(urls = paste(canonical_url, collapse=" ")), by = tweet_id]
    # The whole reason we expanded URLs is because the t.co versions are meaningless. Delete those.
    tweetData$raw_text2 = gsub("(?<!\\w)https?\\://t\\.co/\\S+", "", tweetData$raw_text, perl=T)
    
    tweetData = merge(tweetData, tweetURLs)  # has columns tweet_id, raw_text, and urls
    tweetData[, complete_raw_text := paste(raw_text2, urls)]
    
    return(tweetData[, .(tweet_id, complete_raw_text)])
    
}
