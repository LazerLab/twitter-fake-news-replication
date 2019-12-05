
if (!require("pacman")) install.packages("pacman")
pacman::p_load(stringi)

# In voterfile, names contain: hyphen ("HEDIN-JONES"), period ("st. john"), comma ("mccall, jr."),
# space ("VAN GIESEN"), apostrophe ("O'CONNOR"; "FORTE'").

# We also have accent grave ("ANN`"), backslash (OUELLETTE\\), 
# underscore ("CJ_"), slash ("FIELD/TORREY") and weirdness ("WHITTEN (82)")
# Also name alternatives (or clarifications) within parentheses: 
# "C V (CHARLES VINCENT)", "JAMES (SON)", "EUGENE (GENE)"
# percent signs ("ZE%"), bracket ("[HILLIP"), semicolon ("DO;;EU")
# And a lot of numbers accidentally in the mix. Substitutions ("BARTOL0MEI", "WHITTALL 1V"), 
# typos ("JANIC3E"), and random nonsense ("WILLIAM03041997", "KENNEALLY6").

# Call this on first name and last name separately.
getNameWordsFromVoter = function(voterName) {
    
    # delete apostrophes; change periods, commas and hyphens to spaces
    name = gsub("\\'", "", voterName, perl=T)
    name = gsub("[\\-\\.\\,]", " ", name, perl=T)
    # other new things to change to spaces: accent grave, underscore, back & forward slashes, 
    # percent sign, bracket, semicolon
    name = gsub("[\\`_/\\\\;%\\[]", " ", name, perl=T)
    # delete anything within parens
    name = gsub("\\(.*\\)", "", name, perl=T)
    
    # substitute 1 --> I and 0 --> 0
    name = gsub("1", "I", name, perl=T)
    name = gsub("0", "O", name, perl=T)
    # delete other numbers
    name = gsub("\\d", "", name, perl=T)
    
    # warn about any new weird punctuation we meet
    if (grepl("[^A-Za-z\\s]", name, perl=T)) {
        warning(paste("odd new punctuation removed from this voter name: ", name))
        name = gsub("[^A-Za-z\\s]", "", name, perl=T)
    }
    
    # fix special cases that are inconsistent in voter file:
    # "O CONNOR" --> "O'CONNOR" --> "OCONNOR" [after deleting apostrophe]
    # "MC GEE" --> "MCGEE" ("MC" or "MAC" is not a full word in a last name)
    name = gsub("\\bO\\s+", "O", name, perl=T)
    name = gsub("\\bMC\\s+", "MC", name, perl=T)
    name = gsub("\\bMAC\\s+", "MAC", name, perl=T)
    
    # trim leading whitespace before splitting
    name = sub("^\\s+", "", name, perl=T)
    words = strsplit(name, "\\s+", perl=T)[[1]]
    
    return(tolower(words))
}

# helper function handles encoding, punctuation & case, returns lower-case string
processNameText = function(twitterName) {
    # convert to latin, then latin-ascii (removing diacriticals); then remove symbols
    # (non-latin alphabets will be rare among US-based profiles, but common elsewhere)
    name = stri_trans_nfkc(twitterName)
    name = stri_trans_general(name, "latin; latin-ascii")
    name = stri_replace_all(name, " ", charclass="[\\p{S}]")
    
    # handle punctuation exactly as in getNameWordsFromVoter(): apostrophes are deleted.
    # Other punctuation (and in twitter data we'll also see digits, underscores and anything else)
    # gets replaced by spaces.
    name = gsub("\\'", "", name, perl=T)
    name = gsub("[^a-zA-Z]", " ", name, perl=T)
    
    # trim leading whitespace (in preparation for splitting on whitespace)
    name = sub("^\\s+", "", name, perl=T)
    
    return(name)
}

getWordsFromTwitterName = function(twitterName) {
    name = processNameText(twitterName)
    words = strsplit(name, "\\s+", perl=T)[[1]]
    
    # Look for CamelCase (defined as any capital letters in the middle) 
    # within any word, and split these into separate words
    matchesPerWord = gregexpr("([A-Z][a-z]*|[a-z]+)", words, perl=T)
    moreWords = unlist(regmatches(words, matchesPerWord))
    # Use both the original and the camel-split words
    words = union(words, moreWords)     # some of moreWords will be in words already
    
    return(tolower(words))
}

# Stores & returns more info than getWordsFromTwitterName(), so we can later vary whether or not we split on caps.
getWordsFromTwitterNameForHandleMatching = function(twitterName) {
    
    name = processNameText(twitterName)
    words = strsplit(name, "\\s+", perl=T)[[1]]
    
    # Look for CamelCase (capital letters in the middle) 
    # within any word, and split these into separate words
    # But when it's 2 caps in a row, don't actually split them; just store them as "splittable."
    matchesPerWord = gregexpr("([A-Z][a-z]+|[a-z]+)", words, perl=T)
    camelWords = unlist(regmatches(words, matchesPerWord))
    allCapsWords = unlist(regmatches(words, matchesPerWord, invert=T))
    allCapsWords = allCapsWords[nchar(allCapsWords) > 0]
    
    allWords = tolower(c(allCapsWords, camelWords))
    splittable = rep(F, length(allWords))
    splittable[which(nchar(allCapsWords)>1)] = T
    
    return(list(words=allWords, splittable=splittable))
}

splitHandleNative = function(twitterHandle, useCase=T) {
    # first, split the username on punctuation and digits 
    name = gsub("[^a-zA-Z]", " ", twitterHandle, perl=T)
    name = sub("^\\s+", "", name, perl=T)
    words = strsplit(name, "\\s+", perl=T)[[1]]
    
    if (useCase) {
        # also split on any CamelCase
        matchesPerWord = gregexpr("([A-Z][a-z]*|[a-z]+)", words, perl=T)
        camelWords = unlist(regmatches(words, matchesPerWord))
        words = camelWords
    }
    return(tolower(words))
}
# Use the words from the name to help chop up the twitter handle
# Returns two sets of (new) words to index, and "goodMatch" indicating whether the handle matched 
# the name.
getWordsFromTwitterHandle = function(twitterHandle, nameWords, useCase=T) {
    
    # split the handle using info inside it
    handleWords = splitHandleNative(twitterHandle, useCase=useCase)
    if (!length(handleWords)) {
        return(list(handleWords=c(), alsoToIndex=c(), goodMatch=F))
    }
    
    if (length(nameWords)) {
        # Try picking it apart using words from the full name.
        # "chunks" will be what's left after we try to match things to name words.
        processed = getHandleChunksUsingNameWords(handleWords, nameWords)
        
        # Count it as a good match if over half the length of the handle is accounted 
        # for by the name
        goodMatch = (sum(nchar(processed$untouched)) < .5 * sum(nchar(handleWords))) 
        
        return(list(handleWords=processed$untouched, alsoToIndex=processed$toIndex, goodMatch=goodMatch))
    } else {
        return(list(handleWords=handleWords, alsoToIndex=c(), goodMatch=F))
    }
    
}


# Todo / items to improve:
# -strings of all-caps. In name, we treat them differently between the two functions. In handle, we make them into single letters.
#  Want to do something more consistent and coherent.
#  Really want (for handle) to take whole string of caps, possibly minus the last letter -- i.e., work right for both "PRcomMedical"
#  and "TDAInsurance". At the end of a handle, no such decisions to make.
putThemTogether = function(twitterName, twitterHandle, printSummary=T) {
    
    # First, get the name words to index
    nameWords0 = getWordsFromTwitterName(twitterName)
    
    # Now, get the name words to use for picking apart the handle (differs from above in that these are disjoint)
    nameWordsForHandle = getWordsFromTwitterNameForHandleMatching(twitterName)
    nameWords = nameWordsForHandle$words
    splittable = nameWordsForHandle$splittable
    
    # 4 options for handling capitalization. 
    # name words: either split all caps words or don't; handle: either split on caps or don't
    # It's looking like what we should do is: split neither on caps, or split both on caps.
    handleMoreWordsData = getWordsFromTwitterHandle(twitterHandle, nameWords, useCase=F)
    #handleMoreWordsData2 = getWordsFromTwitterHandle(twitterHandle, nameWords, useCase=T)
    
    nameWordsLetters =  c(nameWords[!splittable], unlist(strsplit(nameWords[splittable], "")))
    #handleMoreWordsData3 = getWordsFromTwitterHandle(twitterHandle, nameWordsLetters, useCase=F)
    handleMoreWordsData4 = getWordsFromTwitterHandle(twitterHandle, nameWordsLetters, useCase=T)
    
    # Switch to handleMoreWordsData4 iff...
    # Pick the result that gives fewer unmatched chars; if tied, then more unmatched words (since handle 
    # doesn't contain spaces, I'd rather bias towards paying attention to caps--except, don't count 1-letter words); 
    # if tied, then higher(?) char count of extra words to index; if tied, then higher number of extra words to index
    # (excluding 1-letter words)
    
    capsFewerHandleLetters = (sum(nchar(handleMoreWordsData4$handleWords)) < sum(nchar(handleMoreWordsData$handleWords)))
    capsMoreHandleWords = (length(handleMoreWordsData4$handleWords[nchar(handleMoreWordsData4$handleWords) > 1]) > 
                                  length(handleMoreWordsData$handleWords[nchar(handleMoreWordsData$handleWords) > 1]))
    capsMoreExtraChars = (sum(nchar(handleMoreWordsData4$alsoToIndex)) > sum(nchar(handleMoreWordsData$alsoToIndex)))
    capsMoreExtraWords = (length(handleMoreWordsData4$alsoToIndex[nchar(handleMoreWordsData4$alsoToIndex) > 1]) > 
                              length(handleMoreWordsData$alsoToIndex[nchar(handleMoreWordsData$alsoToIndex) > 1]))
    if (handleMoreWordsData4$goodMatch && !handleMoreWordsData$goodMatch) {
        handleMoreWordsData = handleMoreWordsData4
    } else if (capsFewerHandleLetters) {
        handleMoreWordsData = handleMoreWordsData4
    } else if (capsMoreHandleWords) {
        handleMoreWordsData = handleMoreWordsData4
    } else if (capsMoreExtraChars) {
        handleMoreWordsData = handleMoreWordsData4
    } else if (capsMoreExtraWords) {
        handleMoreWordsData = handleMoreWordsData4
    }
        

    if (printSummary) {
        cat(paste("name ->", paste(nameWords0, collapse=" "), "\n"))
        cat(paste("handle ->", paste(handleMoreWordsData$handleWords, collapse=" "), ";", paste(handleMoreWordsData$alsoToIndex, collapse=" "), "\n"))
        if (handleMoreWordsData$goodMatch) {
            cat("\tsuccess!\n")
        }
    }
    
    # Before returning this data to another function, take union of the two handle words sets, then setdiff of that with nameWords0.
    allHandleWords = union(handleMoreWordsData$handleWords, handleMoreWordsData$alsoToIndex)
    allHandleWords = setdiff(allHandleWords, nameWords0)
    # also, come on now; I don't want to index any one-letter words...
    allHandleWords = allHandleWords[nchar(allHandleWords) > 1]
    nameWords0 = nameWords0[nchar(nameWords0) > 1]
    
    return(list(nameWords=nameWords0, handleWords=allHandleWords, goodMatch=handleMoreWordsData$goodMatch))
}


# Returns list with two components: untouched (for handle chunks we can't process), and toIndex (for chunks from either end
# that we accounted for, but want in this form too)
# This revision tries to be clever by storing the alignment of name words to the handle. Then,
# once we've done all the matching we can of the handle string, can come back and, for any unmatched 
# handle bits, create chunks that attach them to the nearest matching nameWord. E.g., if name=John and handle=Johnny, 
# we want to return handle chunks "ny" and "johnny". (Because both kinds of cases come up.) 
getHandleChunksUsingNameWords = function(handleWords, nameWords) {
    # I don't quite understand what happens when nameWords is a multiset, but it causes 
    # a warning in the bookkeeping for step 2)
    nameWords = unique(nameWords)
    
    # order each set of words by longest first, so we don't have one-word strings preventing better matches
    nameWordList = nameWords[order(nchar(nameWords), decreasing=T)]
    # put aside 1-letter words; initials should get special handling
    nameWordsAvail = (nchar(nameWordList) > 1)
    
    # ditto for handle words
    handleWordList = handleWords[order(nchar(handleWords), decreasing=T)]
    
    # Data structure to store index (in nameWordList) of what nameWord we've matched to this handle position
    # matchesForHandleChars will always mimic the structure of handle words: same number of words, and nchar of each.
    # matchesForHandleChars always knows which chunks of the handle are still available.  The function 
    # getContinguousChunksFromDataStructure grabs those chunks for a given handle word.
    matchesForHandleChars = vector(mode="list", length=length(handleWordList))
    for (i in 1:length(handleWordList)) {
        matchesForHandleChars[[i]] = vector(mode="numeric", length=nchar(handleWordList[i]))
    }
    
    # Step 1: Look for (full) nameWords within handle
    # For each nameWord: 
    # -try to "use" it by matching against the handle words
    # -if you use it, mark it as used, and mark the corresponding handle chars with the nameWord index
    
    for (i in 1:length(nameWordList)) {
        if (!nameWordsAvail[i]) {
            next
        }
        nameW = nameWordList[i]
        # starting with the shortest handleWords, see if any contain nameW
        for (j in length(handleWordList):1) {
            handleW = handleWordList[j]
            listOfContiguousChunks = getContinguousChunksFromDataStructure(matchesForHandleChars[[j]])
            if (! length(listOfContiguousChunks)) {
                next
            }
            for (k in 1:length(listOfContiguousChunks)) {
                handleSubstr = substr(handleW, listOfContiguousChunks[[k]]$startChar, 
                                      listOfContiguousChunks[[k]]$endChar)
                
                matchesInHandle = regexpr(nameW, handleSubstr) 
            
                if (matchesInHandle != -1) {
                    # found a match!
                    # mark these chars as taken in handle
                    startChar = matchesInHandle + (listOfContiguousChunks[[k]]$startChar - 1)
                    endChar = startChar + attr(matchesInHandle, "match.length") - 1 
                    matchesForHandleChars[[j]][startChar:endChar] = rep(i, length(startChar:endChar))
                    # mark this nameW as taken (and move on to the next)
                    nameWordsAvail[i] = F
                    break
                }
            }
            if (!nameWordsAvail[i]) {  # if we matched the name word, break from this the handle loop too
                break
            }
        }
    }
    
    # Step 2: 
    # For each remaining handle chunk,
    # -see if it matches within an available name word. (We know it doesn't match a whole one.) 
    #  If chunk is only 1 letter, must match at the start of the name word.
    # -if so, mark the nameWord used, mark the section of handle as used. Later, we'll make sure
    # this chunk of handle is put on the list to index.
    # Go from longest to shortest handle chunk again. Do include one-letter handle words and one-letter name words.
    
    # todo: consider repeating this section until there are no changes
    
    oneLetterNameWords = (nchar(nameWordList) == 1)
    nameWordsAvail[which(oneLetterNameWords)] = T
    
    # get all handle chunks, and sort by length
    currentAvailableChunks = itemizeAvailableChunks(matchesForHandleChars)
    if (is.null(currentAvailableChunks$coords)) {
        return(assembleAlignment(matchesForHandleChars, handleWordList, nameWordList, c()))
    }
    
    chunksToCheck = currentAvailableChunks$coords[rev(order(currentAvailableChunks$lengths))]
    # to interpret chunksToCheck: parse the "j:k", telling us to grab availableChunks[[j]][k],
    # which gives the index for the substring within handleWordList[j]
    for (chunkCoords in chunksToCheck) {
        jkvector = strsplit(chunkCoords, ":")[[1]]
        j = as.numeric(jkvector[1])
        k = as.numeric(jkvector[2])
        availableChElement = currentAvailableChunks$availableChunks[[j]][[k]]  # has startChar and endChar
        handleSubstr = substr(handleWordList[j], availableChElement$startChar, availableChElement$endChar)
        
        # see if it's inside a nameWord. (If it's one letter long, require it to be at start of the nameWord.)
        if (nchar(handleSubstr) > 1) {
            # is handle chunk present anywhere inside?
            matchesInNames = regexpr(handleSubstr, nameWordList[nameWordsAvail])
        } else {
            # is handle chunk a prefix?
            matchesInNames = regexpr(paste("^", handleSubstr, sep=""), nameWordList[nameWordsAvail])
        }
        
        if (sum(matchesInNames > -1)) {   # if there are any matches
            # kill this name word
            # If handleW is a substring of multiple nameWords, match it to the shortest name word.
            nameWordToUse = nameWordList[nameWordsAvail][max(which(matchesInNames > -1))]
            nameWordIndex = which(nameWordList == nameWordToUse)
            nameWordsAvail[nameWordIndex] = F
            
            # mark this section of handle as belonging to this nameWord
            matchesForHandleChars[[j]][availableChElement$startChar:availableChElement$endChar] = 
                rep(nameWordIndex, length(availableChElement$startChar:availableChElement$endChar))
        }
    }
    
    # Step 3: 1-letter name words: look for them at the start of remaining handle chunks
    # Also, if we make a change, re-itemize and check all the nameWords again (in case the changes allow a new match),
    # until there are no further changes.
    
    wordsToAddToIndex = c()    # our return value. We're going to start putting things in it during this loop.
    
    repeat {   # kind of like a do-while loop, but you have to explicitly decide to break out
        
        # determine remaining chunks 
        currentAvailableChunks = itemizeAvailableChunks(matchesForHandleChars)
        if (is.null(currentAvailableChunks$coords)) {
            return(assembleAlignment(matchesForHandleChars, handleWordList, nameWordList, wordsToAddToIndex))
        }
    
        # this time, start with the shortest handleWords, because we might find exact matches among them.
        chunksToCheck = currentAvailableChunks$coords[order(currentAvailableChunks$lengths)]
        
        madeChangesToHandleMatches = F
        for (i in 1:length(nameWordList)) {
            if (!nameWordsAvail[i] || !oneLetterNameWords[i]) {
                next
            }
            nameW = nameWordList[i]
            
            for (chunkCoords in chunksToCheck) {
                jkvector = strsplit(chunkCoords, ":")[[1]]
                j = as.numeric(jkvector[1])
                k = as.numeric(jkvector[2])
                availableChElement = currentAvailableChunks$availableChunks[[j]][[k]]  # has startChar and endChar
                handleSubstr = substr(handleWordList[j], availableChElement$startChar, availableChElement$endChar)
                    
                matchesInHandle = regexpr(paste("^", nameW, sep=""), handleSubstr) 
                    
                if (matchesInHandle != -1) {
                    # found a match!
                    
                    # mark this whole substring of handle as belonging to this nameWord
                    # and also add this substring (an expansion of a 1-letter nameWord) to the wordsToAddToIndex we'll return
                    #matchesForHandleChars[[j]][availableChElement$startChar:availableChElement$endChar] = 
                    #    rep(nameWordIndex, length(availableChElement$startChar:availableChElement$endChar))
                    
                    # Treat it like we did in step 1: 
                    # -mark this handle 1st letter (only) as used by this word
                    # -add the full handle substring (an expansion of a 1-letter nameWord) to the index [not necessary -- will be done at the end]
                    # -but this time, don't mark the rest of the handle substring as used. In case it contains more initials.
                    # -mark the nameword as used
                    # (We'll do exactly the same handling in step 4)
                    matchesForHandleChars[[j]][availableChElement$startChar] = i
                    madeChangesToHandleMatches = T
                    
                    nameWordsAvail[i] = F
                    break
                }
            }
            if (!nameWordsAvail[i]) {  # if we matched the name word, break from this the handle loop too
                break
            }
        }
    
        if (!madeChangesToHandleMatches) {
            break
        }
    }
    
    
    
    # Step 4: take remaining name words, and look for their initials at start and end of remaining handle chunks
    # (Looks much like step 3, except using initials)
    
    repeat {   
        
        # determine remaining chunks 
        currentAvailableChunks = itemizeAvailableChunks(matchesForHandleChars)
        if (is.null(currentAvailableChunks$coords)) {
            return(assembleAlignment(matchesForHandleChars, handleWordList, nameWordList, wordsToAddToIndex))
        }
        
        # start with the shortest handleWords, because we might find exact matches among them.
        chunksToCheck = currentAvailableChunks$coords[order(currentAvailableChunks$lengths)]
        
        madeChangesToHandleMatches = F
        for (i in 1:length(nameWordList)) {
            if (!nameWordsAvail[i]) {
                next
            }
            nameInitial = substr(nameWordList[i], 1, 1)
            
            for (chunkCoords in chunksToCheck) {
                jkvector = strsplit(chunkCoords, ":")[[1]]
                j = as.numeric(jkvector[1])
                k = as.numeric(jkvector[2])
                availableChElement = currentAvailableChunks$availableChunks[[j]][[k]]  # has startChar and endChar
                handleSubstr = substr(handleWordList[j], availableChElement$startChar, availableChElement$endChar)
                
                initialInHandle = regexpr(paste("^", nameInitial, sep=""), handleSubstr)                 
                
                if (initialInHandle != -1) {
                    # found a match!
                    # Treat it like we did in steps 1 & 3: 
                    # -mark this handle 1st letter (only) as used by this word
                    # -add the full handle substring to the index [not necessary, will be done at the end]
                    # -but this time, don't mark the rest of the handle substring as used. In case it contains more initials.
                    # -mark the nameword as used
                    
                    matchesForHandleChars[[j]][availableChElement$startChar] = i
                    madeChangesToHandleMatches = T
                    
                    nameWordsAvail[i] = F
                    break
                }
            }
            if (!nameWordsAvail[i]) {  # if we matched the name word, break from this the handle loop too
                break
            }
        }
        
        if (!madeChangesToHandleMatches) {
            break
        }
    }
        

    
    # Step 5: assemble alignment into a set of chunks to return
    # Chunks of handle we'll return:
    # -any already manually put on the return list
    # -those we have matched to name words
    # -those we haven't matched to any name word
    # -bonus: for unmatched sections, if they're preceded by a matched section, concatenate unmatched + matched into a word
    retVal = assembleAlignment(matchesForHandleChars, handleWordList, nameWordList, wordsToAddToIndex)
    
    return(retVal)
}

assembleAlignment = function(matchesForHandleChars, handleWordList, nameWordList, wordsToAddToIndex) {  
    currentAvailableChunks = itemizeAvailableChunks(matchesForHandleChars)
    unmatchedChunks = c()
    
    for (chunkCoords in currentAvailableChunks$coords) {
        jkvector = strsplit(chunkCoords, ":")[[1]]
        j = as.numeric(jkvector[1])
        k = as.numeric(jkvector[2])
        availableChElement = currentAvailableChunks$availableChunks[[j]][[k]]  # has startChar and endChar
        handleSubstr = substr(handleWordList[j], availableChElement$startChar, availableChElement$endChar)
        
        # add unmatched chunks
        unmatchedChunks = c(unmatchedChunks, handleSubstr)    
        
        # also, if there's a matched portion just before it
        if (availableChElement$startChar > 1) {
            # which word did that part belong to?
            precedingMatchedWordIndex = matchesForHandleChars[[j]][availableChElement$startChar - 1] 
            #newWordToAdd = paste0(nameWordList[precedingMatchedWordIndex], handleSubstr)
            # don't necessarily take the nameWord verbatim. Rather, take the matched segment of handle.
            precedingMatchedPart = substr(handleWordList[[j]], start=min(which(matchesForHandleChars[[j]]==precedingMatchedWordIndex)),
                                          stop=max(which(matchesForHandleChars[[j]]==precedingMatchedWordIndex)))
            newWordToAdd = paste0(precedingMatchedPart, handleSubstr)
                                                                               
            wordsToAddToIndex = union(wordsToAddToIndex, newWordToAdd)
        }
    }
    
    # now, repeat to get the happily matched chunks too
    happilyMatchedChunks = itemizeAvailableChunks(matchesForHandleChars, invert=T)
    matchedChunks = c()
    for (chunkCoords in happilyMatchedChunks$coords) {
        jkvector = strsplit(chunkCoords, ":")[[1]]
        j = as.numeric(jkvector[1])
        k = as.numeric(jkvector[2])
        availableChElement = happilyMatchedChunks$availableChunks[[j]][[k]]  # has startChar and endChar
        handleSubstr = substr(handleWordList[j], availableChElement$startChar, availableChElement$endChar)
        
        # no need to store initials of nameWords, which is what these probably are
        if (nchar(handleSubstr) > 1) {
            matchedChunks = c(matchedChunks, handleSubstr)    
        }
    }
    # most matchedChunks will just be nameWords, but sometimes they'll differ
    matchedChunks = setdiff(matchedChunks, nameWordList)
    wordsToAddToIndex = union(wordsToAddToIndex, matchedChunks)
        
    wordsToAddToIndex = setdiff(wordsToAddToIndex, unmatchedChunks)
    return(list(untouched=unmatchedChunks, toIndex=wordsToAddToIndex))
}
    
# takes one element of matchesForHandleChars list, returns list of continguous substrings available in it
# (information is all about string indices, not actual strings)
# parameter invert: if T, return non-available substrings instead
getContinguousChunksFromDataStructure = function(matchesForHandleChars, invert=F) {
    if (invert) {  # actually written as a separate fn
        return(getMatchedChunksFromDataStructure(matchesForHandleChars))
    }
        
    openIndices = which(matchesForHandleChars == 0)
    contiguousChunks = list()
    startK = -1
    prevK = -1
    numChunksSeen = 0
    for (k in openIndices) {
        if (k == prevK + 1) {  # extend the previous word
            prevK = k
        } else {
            if (startK != -1) {   # skip if it was just the initialization value
                # we have a substring to examine
                newSubstring = list(startChar=startK, endChar=prevK)
                numChunksSeen = numChunksSeen + 1
                contiguousChunks[[numChunksSeen]] = newSubstring
            }
            startK = prevK = k
        }
    }
    # finish the open word
    if (startK != -1) {   # skip if it was just the initialization value
        newSubstring = list(startChar=startK, endChar=prevK)
        numChunksSeen = numChunksSeen + 1
        contiguousChunks[[numChunksSeen]] = newSubstring
    }    
    
    return(contiguousChunks)
}

getMatchedChunksFromDataStructure = function(matchesForHandleChars) {
    #openIndices = which(matchesForHandleChars == 0)
    
    contiguousChunks = list()
    startK = -1
    prevVal = -1
    numChunksSeen = 0
    for (k in 1:length(matchesForHandleChars)) {
        valAtK = matchesForHandleChars[k]
        #if (valAtK == prevVal) {  # extend the previous word
        # to extend, do nothing
        if (valAtK != prevVal) {
            if (startK != -1) {   # skip if it was just the initialization value
                # we have a substring to examine
                newSubstring = list(startChar=startK, endChar=(k-1))
                numChunksSeen = numChunksSeen + 1
                contiguousChunks[[numChunksSeen]] = newSubstring
            }
            startK = k
            prevVal = valAtK
        }
    }
    # finish the open word
    if (startK != -1) {   # skip if it was just the initialization value
        newSubstring = list(startChar=startK, endChar=k)
        numChunksSeen = numChunksSeen + 1
        contiguousChunks[[numChunksSeen]] = newSubstring
    }    
    
    return(contiguousChunks)
}

itemizeAvailableChunks = function(matchesForHandleChars, invert=F) {
    availableChunks = list()
    lengthOfAvailableChunks = c()
    whereToFindAvailableChunks = c()
    for (j in 1:length(matchesForHandleChars)) {
        availableChunks[[j]] = getContinguousChunksFromDataStructure(matchesForHandleChars[[j]], invert=invert)
        if (!length(availableChunks[[j]])) {
            next
        }
        for (k in 1:length(availableChunks[[j]])) {
            thisChunk = (availableChunks[[j]])[[k]]
            lengthOfAvailableChunks = c(lengthOfAvailableChunks, thisChunk$endChar - thisChunk$startChar + 1)
            whereToFindAvailableChunks = c(whereToFindAvailableChunks, paste(j, k, sep=":"))
        }
    }
    return(list(availableChunks=availableChunks, lengths=lengthOfAvailableChunks, coords=whereToFindAvailableChunks))
}


# tricky cases:
#> putThemTogether2("Millie Rosario",  "Mrs_Stunts")
#name -> millie rosario 
#handle -> stunts s ; rs 
# might prefer to also get "mrs", but function took off the both the M and R as initials (which might be correct anyway)

#putThemTogether2("Brianna Fleming", "Bria_Ming")
#name -> brianna fleming 
#handle ->  ;  
#success!
# might prefer to also get "ming"
    

#> putThemTogether("JKayeLove",       "JessicaKayee")
#[1] "name -> jkayelove j kaye love"
#[1] "handle -> essica e"
#<--fixed (to "handle -> jessica e")
# might prefer also getting "kayee"
# now, happy

#> putThemTogether("Food&BeverageERP.com",    "FoodBevERP")
#[1] "name -> food beverageerp com beverage e r p"
#[1] "handle -> v"
#<--fixed (to empty handle)
# now, happy using no-camel or both-camel splitting
#
#> putThemTogether("ben.",    "bduff")
#[1] "name -> ben"
#[1] "handle -> bduff"
# (could it get: handle->b duff?)
# now, happy
#
#> putThemTogether("Bri ⚓️\u2728",   "Bittybriii_")
#[1] "name -> bri"
#[1] "handle -> bitty ii"
# (might prefer: handle->bitty ii briii)
# now, happy

#> putThemTogether("T.j. Dunn",       "imtjdunn")
#[1] "name -> t j dunn"
#[1] "handle -> imtj"
# (might prefer: "im", as an earlier version returned)
# now: won't work unless we decide that in step 3, it's ok to match the *end* of a word.

#> putThemTogether("Sam Solace",      "SamuelSolace")
#name -> sam solace 
#handle -> uel 
# might prefer: samuel
# now, happy

# > putThemTogether("Escola Pierre Vigne",     "epvigne")
#name -> escola pierre vigne 
#handle -> ep 
# might prefer: empty handle
# now, happy

#> putThemTogether("NessaLeeCarter",  "imNESSAsary")
#name -> nessaleecarter nessa lee carter 
#handle -> asary im e s 
#> putThemTogether("CoachCob",        "COB2123")
#name -> coachcob coach cob 
#handle -> o b 
# prefer to handle the all-caps better: match as one word first
# <--fixed!
# now: for both, matches the all-caps part in no-camel mode 


#> putThemTogether("anahylares", "Anahy980")
#name -> anahylares 
#handle ->  ; anahy 
# prefer to also index "lares"

# putThemTogether("SHINEBLOCK", "ShineBlock")
#name -> shineblock s h i n e b l o c k 
#handle ->  ;  
# prefer to extract "shine" and "block"
