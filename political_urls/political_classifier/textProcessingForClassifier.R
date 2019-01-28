
# The tokenizing and grepping functions used for classifying tweets.

# Basically a fancy call to grep (fancy in that it knows about hashtags, handles and urls). Always case-insensitive.
# Returns a logical vector.
# Every term is searched for (a) within hashtags, (b) as a handle, and (c) as a standalone word (possibly with -s or -es appended).
# Whitelist terms: once lowercased, they become a regexp pattern. Special characters could cause problems. 
# Do NOT use '#' or '@' characters in the whitelist; in the current implementation, they won't match at all.
# Escaped regex components like \\. do work within text and urls.
# 
# N.B. This can get really slow if the keyword list gets long. 
# N.B. Keeps non-matched parts in their original case so we can recognize camelCase later.
# [During development, checking 100k lines of input against 42 whitelist terms was taking 12 sec, which was 4x faster than before optimizing.]
checkForWhitelistTerms = function(whitelistTerms, tweets, removeWLTerms=FALSE) {
    
    whitelistLower = unique(tolower(whitelistTerms))
    print(paste("Checking", length(tweets), "tweets against", length(whitelistLower), "whitelist terms"))
    tweets = paste("", tweets)  # prepend a space. So I can use a regex that's faster for 3a/3b.
    tweetsLower = tolower(tweets)
    
    # Hashtags need not follow word boundaries
    # For the sake of removeWLTerms, grab the entire hashtag
    whiteBoundaries1 = paste0("#[[:alnum:]_]*", whitelistLower, "[[:alnum:]_]*")  # i.e., if it's inside a hashtag, we like it regardless of what's before or after
    # If in a handle, must be an exact match 
    whiteBoundaries2 = paste0("@", whitelistLower, "(?!\\w)")
    # In free text and urls (no need to distinguish), must follow word boundaries. Can also add -s or -es.
    whitelistLower = gsub("\\b\\ \\b", "[ -]", whitelistLower)   # space-separated terms can also be hyphen-separated
    endsInPunct = grepl("\\W$", whitelistLower)
    whiteBoundaries3a = paste0("[^@]\\b", whitelistLower[!endsInPunct], "(e?s)?\\b")   # far faster than expression below
    if (sum(endsInPunct)) {
        whiteBoundaries3b = paste0("[^@]\\b", whitelistLower[endsInPunct])
    } else {
        whiteBoundaries3b = c()
    }
    whiteBoundaries3 = c(whiteBoundaries3a, whiteBoundaries3b)
    
    # Timing-wise, long regexprs take forever to run. Much faster to run each type individually.
    # (useBytes makes it mildly faster w/o apparent changes to results)
    whitePatStr = paste(whiteBoundaries1, collapse="|")
    hasM1 = grepl(whitePatStr, tweetsLower, perl=T, useBytes=T)
    whitePatStr = paste(whiteBoundaries2, collapse="|")
    hasM2 = grepl(whitePatStr, tweetsLower, perl=T, useBytes=T)
    whitePatStr = paste(whiteBoundaries3, collapse="|")
    hasM3 = grepl(whitePatStr, tweetsLower, perl=T, useBytes=T)
    hasMatch = hasM1 | hasM2 | hasM3
    
    if (removeWLTerms) {
        # Go back and look at where the matches take place. Runs fast b/c only looks at tweets that matched.
        posTweets = tweetsLower[hasMatch]
        posTweetsOrigCase = tweets[hasMatch]
        
        # Extract URLs independently of the rest (couldn't get it to work using only 1 pattern)
        whiteBoundaries4 = paste0("\\bhttps?\\:\\S*\\b", whitelistLower[!endsInPunct], "(e?s)?\\b\\S*")   # special for full URLs
        whitePatStr = paste(whiteBoundaries4, collapse="|")
        matchInfo = gregexpr(whitePatStr, posTweets, perl=T)   # turn off useBytes to avoid errors below
        matchedParts = regmatches(posTweets, matchInfo)   # returns a list. Each element is a vector of 0 or more strings.
        unmatchedParts = regmatches(posTweetsOrigCase, matchInfo, invert=T)   # returns a list. Each element is a vector of 0 or more strings.
        matchedCol = sapply(matchedParts, function(x) { paste(unique(x), collapse=", ") })
        unmatchedCol = sapply(unmatchedParts, paste, collapse=" ")
        
        # Now, with URLs removed, extract the other matches from unmatchedCol
        bigWhitePatStr = paste(c(whiteBoundaries1, whiteBoundaries2, whiteBoundaries3), collapse="|")
        matchInfo = gregexpr(bigWhitePatStr, tolower(unmatchedCol), perl=T)   # turn off useBytes to avoid errors below
        matchedParts = regmatches(tolower(unmatchedCol), matchInfo)   # returns a list. Each element is a vector of 0 or more strings.
        unmatchedParts = regmatches(unmatchedCol, matchInfo, invert=T)   # returns a list. Each element is a vector of 0 or more strings.
        # Collapse the vectors to 1 string per tweet. E.g., list entry with "Clinton" "Trump" changes to "Clinton Trump"
        matchedCol2 = sapply(matchedParts, function(x) { paste(unique(x), collapse=", ") })
        unmatchedCol2 = sapply(unmatchedParts, paste, collapse=" ")
        
        # Paste URL matches and other matches together
        matchedCol3 = paste(matchedCol, matchedCol2, sep=", ")
        # But remove leading, trailing ", "
        matchedCol3 = sub("^, ", replacement = "", matchedCol3, perl=T)
        matchedCol3 = sub(", $", replacement = "", matchedCol3, perl=T)
        
        # Map back to original tweets
        fullMatchedCol = rep("", length(tweets))
        fullUnmatchedCol = tweets
        fullMatchedCol[hasMatch] = matchedCol3
        fullUnmatchedCol[hasMatch] = unmatchedCol2
        return(data.table(classifier_label=hasMatch, white_terms=fullMatchedCol, rest_of_text=fullUnmatchedCol))
    } else {
        # Short and simple
        return(hasMatch)
    }
    
}


# Mostly just tokenizes by breaking at punctuation and spaces, but some special handling for Twitter entities:
# -URLs: save entire URL in addition to tokenizing it (useful when a particular URL is shared a lot)
# -handles and hashtags: save original word (stripping punctuation), and (optionally) also split at camel case or numbers
# In order to handle camelCase, tweets must be in original case. Will be lowercased here.
tokenizeTweet = function(tweets, 
                         # some tweaks I thought might help
                         breakCamelCase=T, removeApostS=T, keepUSA=T,
                         doStemming=T) {  
    
    # extract URLs (must not be preceded by alphanumeric char)
    urlMatches = gregexpr("\\bhttps?\\:\\S+", tweets, perl=T)  # starts with http: or https:, goes until a space
    urls = regmatches(tolower(tweets), urlMatches)   # left in list form; now lowercased
    # collapsed back into vectors with one string per tweet
    noturls = sapply(regmatches(tweets, urlMatches, invert=T), paste, collapse=" ")
    
    # extract handles and hashtags (even if mid-word)
    handlesHashesMatches = gregexpr("(@|#)[\\w_]+", noturls, perl=T)
    handlesHashesList = regmatches(noturls, handlesHashesMatches)
    handlesHashesListNoPunct = sapply(handlesHashesList, gsub, pattern = "[@#]+", replacement="")   # used below
    handlesHashesVector = sapply(handlesHashesListNoPunct, paste, collapse=" ")                     # used below
    justWords = sapply(regmatches(tolower(noturls), handlesHashesMatches, invert=T), paste, collapse=" ")  # now lowercased
    
    
    # camel case extraction, then make lowercase
    if (breakCamelCase) {
        # adds component words from handles and hashtags
        
        matchesPerEntry = gregexpr("([A-Z][a-z]+|[A-Z]+|[a-z]+|[0-9]+)", handlesHashesVector, perl=T)
        wordPieces = regmatches(handlesHashesVector, matchesPerEntry)        
        
        # see which of new are not %in% old
        retrieveNewBits = function(a, b) { a[! (a %in% b)] }
        newBits = mapply(retrieveNewBits, wordPieces, handlesHashesListNoPunct)
        # change from list to vector
        newBitsVector = sapply(newBits, paste, collapse=" ")
        # paste the old and new vector entries together
        newPlusOld = paste(handlesHashesVector, newBitsVector)
        handlesHashesProcessed = tolower(newPlusOld)
        
    } else {
        handlesHashesProcessed = tolower(handlesHashes)
    }
    
    # for words: apostrophe and usa handling
    if (removeApostS) {
        # this version turns "obama's" into "obama". Other one turns it into "obamas".
        words1a = gsub("'s", " ", justWords, fixed=T)
        justWords = gsub("'", "", words1a, fixed=T)
    } else {
        # delete apostrophes
        justWords = gsub("'", "", justWords, fixed=T)
    }
    
    if (keepUSA) {
        # change "u.s." and "u.s.a." to the word "usa"; otherwise, they'll evaporate into single letters
        justWords = gsub("(?<![\\w\\.])u\\.s\\.(a\\.)?(?!\\w)", "usa", justWords, perl=T)  # trying to ensure it's a stand-alone word
    }
    
    
    # For words and handles and hashes and URLs (excluding the full url, saved separately): 
    # split on punctuation and spaces, remove punctuation, and do stemming if asked.
    # (Relying on tokenizers package.)
    # Internally, the tokenizer is using stringi's stri_split_boundaries(), which is reasonably smart.
    # It keeps periods inside web addresses, e.g., "twitter.com", and it keeps underscores.
    allButFullURLs = paste(justWords, handlesHashesProcessed, urls)
    if (doStemming) {
        tokenizedList = tokenize_word_stems(allButFullURLs)
    } else {
        tokenizedList = tokenize_words(allButFullURLs)
    }
    
    # put urls back in
    boundCols = cbind(tokenizedList, urls)   # a 2-column matrix, which doesn't mind that each cell holds a vector
    dimnames(boundCols) = NULL
    allTogether = apply(boundCols, 1, unlist)
    
    return(allTogether)
    
}


# Returns a DocumentTermMatrix
tokenizeToDocTermMatrix = function(tweets, modelFileOutStem=NULL, loadVectorizer=F) {
    
    # Create an iterator over this set of tweets. Args:
    # preprocessor: can be user-defined -- any function that maps one string to another.
    # tokenizer: is called after preprocessor, splits one string into many. 
    iterator_tweets = itoken(tweets, tokenizer=tokenizeTweet)  # my own function, above
    
    if (!loadVectorizer) {
        print("Computing corpus term freqs")
        vocab = create_vocabulary(iterator_tweets)      # essentially a data.table of term & doc freqs
        
        # If it doesn't appear at least once per 10000 docs, can't be too helpful in a classifier.
        # --> changed to 5000 to get model to converge with politics3 whitelist.
        # (Seems better to use this option than term_count_min, which would need to be adjusted depending on corpus size.)
        vocab2 = prune_vocabulary(vocab, doc_proportion_min=1/5000, doc_proportion_max = .9)
        
        vectorizer = vocab_vectorizer(vocab2)            # maps terms to indices
        if (!is.null(modelFileOutStem)) {
            print("Saving vectorizer")
            saveRDS(vectorizer, file = paste0(modelFileOutStem, "-vectorizer.rds"))
        }
    } else {
        vectorizer <- readRDS(paste0(modelFileOutStem, "-vectorizer.rds"))
    }
    
    print("Building doc-term matrix")
    dtMatrix = create_dtm(iterator_tweets, vectorizer)  # needs tweets as input, since earlier we just used them to get word stats 
    
    return(dtMatrix)
}
