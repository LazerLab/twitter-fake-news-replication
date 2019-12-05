
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table)

source("processLocations.R")

# File to take preliminary matching results (from querying voters against the twitter DB) and convert them to 
# full-fledged matches, where each row = 1 voter + 1 twitter account.

# matchFileIn: Fields available may vary, but that doesn't matter, because we'll keep only what's needed, then 
#   join to the full voter file at the end. Fields expected: 
#   voter_id, matchesThisState, matchesPlainUS, matchesUnknownLoc, matchesOtherState, matchesForeign;
#   then, for fileFormat>=3: twProfileID, twProfileName, twProfileHandle, twProfileLoc, loc2 (or newInferredLoc for fileFormat==4), [voter's] state, city
# voterFileIn: assumed to be one of the csv files, and it has to contain "voter_id".
# fullMatchFileOut: will be a .csv file.
matchTheMatches = function(voterfileIn, matchFileIn, fullMatchFileOut,
                           matchFileFormat = 1,   
                           # File formats: 1: counts summary (outdated); 
                           #               2: all potential matches (with now-obsolete location coding); 
                           #               3: all potential matches with intermediate newlocs column "loc2" already added using purely isLocationInUSA()
                           #               4: all potential matches with final newInferredLoc column already added using addNewLocToFile()
                           placeListDir=NULL    # placeListDir is needed when format = 3
                        ) {
    matchRulesVersion = 3   # no need to support others here

    # 1. Decide which matches are reliable (depending on the matching rules to use).
    
    # (note: specifically not using the flag colClasses = "character", to avoid excessive quotes in the output file)
    matchCountsData = read.table(file=matchFileIn, header=T, #stringsAsFactors=F, 
                                 comment.char="", sep="\t", fill=T, quote="", na.strings=NULL) 
    
    if (matchFileFormat==3) {
        # convert format 3 to 4, by cross-checking loc2 field against voter cities, storing as newInferredLoc
        matchCountsData = updateFormat3To4(matchCountsData, placeListDir)
        matchFileFormat = 4
    }
    
    # Do the matching! (Exciting part.)
    if (matchRulesVersion == 1) {
        # conservative / original: only allow the match when there's exactly 1 match on twitter and it's to the right state
        #twMatches = conservativeMatching1(matchCountsData, matchFileFormat)
    } else if (matchRulesVersion == 2) {
        # Ignore any candidate matches for which we know the location and it's a different US state
        #twMatches = conservativeMatching2(matchCountsData, matchFileFormat)
    } else if (matchRulesVersion == 3) {
        # Additionally exclude "foreign" locs (requires more recent loc computation).
        
        # Requires input to have newInferredLoc column, either computed above or offline in advance.
        if (matchFileFormat == 4 && "newInferredLoc" %in% colnames(matchCountsData)) {
            twMatches = matching3ExcludeForeign(matchCountsData)
        } else {
            print("error in matchTheMatches rule 3: wrong input file format")
            return()
        }
    }
    
    # 2. Ensure uniqueness: if >= 2 voters match to the same twitter account, drop these rows.
    twitterAcctFreqs = table(twMatches$twProfileID)
    rows_twitterOccurringJustOnce = (twMatches$twProfileID %in% names(twitterAcctFreqs)[twitterAcctFreqs==1])
    twMatches = twMatches[rows_twitterOccurringJustOnce,]
    
    # 3. Merge with the original voter data
    voterData = read.csv(file=voterfileIn, header=T, sep="\t") 
    allDataMatched = merge(twMatches, voterData, by.x="voter_id", by.y="voter_id", all.x=T)
    
    # sanity check: specifying all.x shouldn't have created any blank fields
    if (sum(is.na(allDataMatched$last_name))) {
        warning("Hrmm. Some records were not properly matched to the voter data.")
    }
    
    # 4. Write out
    print(paste("Used matching rules", matchRulesVersion, "and got", nrow(allDataMatched), "matches."))
    fwrite(allDataMatched, file=fullMatchFileOut, row.names=F, na="", sep="\t")
    print(paste("Output saved as", fullMatchFileOut))
}



# Exclude potential matches that are foreign or in a different state
matching3ExcludeForeign = function(matchCountsData) {
    # for returning
    fieldNamesWanted = c("voter_id", "twProfileID", "twProfileName", "twProfileHandle", "twProfileLoc")
    
    # make sure some columns are characters
    matchCountsData$newInferredLoc = as.character(matchCountsData$newInferredLoc)
    matchCountsData$state = as.character(matchCountsData$state)
    
    # code modified from rule2, with just one addition:
    # remove rows with a newInferredLoc of "foreign"
    matchCountsData = matchCountsData[matchCountsData$newInferredLoc != "foreign",]
    
    # remove matchesOtherState rows! Find these using the newly inferred loc, since it might have changed.
    newLocLength = nchar(matchCountsData$newInferredLoc)   # only states have length 2
    differentState = (newLocLength == 2 & matchCountsData$newInferredLoc != matchCountsData$state)
    matchCountsData = matchCountsData[!differentState,]
        
    # now, see which voters have exactly 1 matching row
    nameFreqs = table(matchCountsData$voter_id)
    rows_votersOccurringJustOnce = (matchCountsData$voter_id %in% names(nameFreqs)[nameFreqs==1])
    votersWithOneMatch = matchCountsData[rows_votersOccurringJustOnce,]
    # accept the match only if it's to the right state!
    acceptThisMatch = which(votersWithOneMatch$state == votersWithOneMatch$newInferredLoc)
    
    fieldsWanted = (colnames(matchCountsData) %in% fieldNamesWanted)
    matches = votersWithOneMatch[acceptThisMatch, fieldsWanted]
    return(matches)
    
}


# Wrapper for updateFormat3To4: reads file (in format 3), converts "loc2" to "newInferredLoc", saves file (now format 4).
addNewLocToFile = function(matchFileIn, matchFileOut, placeListDir) {
    # we expect input file to contain loc2
    matchCountsData = read.table(file=matchFileIn, header=T, #stringsAsFactors=F, 
                                 comment.char="", sep="\t", fill=T, quote="", na.strings=NULL)
    
    matchCountsData = updateFormat3To4(matchCountsData, placeListDir)
    
    # note wrt quotes: help(write.table) says: "in both cases, row and column names are quoted if they are written."
    write.table(matchCountsData, file=matchFileOut, row.names=F, na="", sep="\t", quote=F)
    return(T)
}

# This function adds a new column "newInferredLoc" and keeps the old "loc2"
updateFormat3To4 = function(matchCountsData, placeListDir) {
        if (! ("loc2" %in% colnames(matchCountsData))) {
            print("error in matchTheMatches: expected a column named loc2")
            return()    
        }
        # caution: loc2 can be (the string) "NA", but this needs to be treated as "" here.
        matchCountsData$loc2[matchCountsData$loc2 == "NA"] = rep("", sum(matchCountsData$loc2=="NA"))
        
        # cross-check locs against voter cities
        places = readKnownPlaceLists(placeListDir)
        matchCountsData$newInferredLoc = updateLocInference(matchCountsData, matchCountsData$loc2, places)
        return(matchCountsData)
}

# Take table of candidate matches with loc2 
# and update the loc by seeing if the twitter loc contains the voter's city (or state?).
updateLocInference = function(matchCountsData, loc2, places) {
    # make sure certain columns are characters
    loc2 = as.character(loc2)
    matchCountsData$state = as.character(matchCountsData$state)
    matchCountsData$twProfileLoc = as.character(matchCountsData$twProfileLoc)
    matchCountsData$city = as.character(matchCountsData$city)

    # See if the twitter loc contains the voter's city. 
    # (Yes! This gets us about 5% more "matchesThisState" rows, in one data file.)
    locStrings = matchCountsData$twProfileLoc
    citiesAsPatterns = paste0("\\b", matchCountsData$city, "\\b")
    locStringContainsCity = mapply(grepl, citiesAsPatterns, locStrings, MoreArgs=list(ignore.case=T, perl=T))
    # note: sometimes registered city is blank. Don't count those as matches:
    locStringContainsCity = locStringContainsCity & (mapply(nchar, matchCountsData$city) > 0)
    
    # See if the twitter loc contains the voter's state. (More lenient pattern-matching now that it's a candidate match.)
    alsoLookForStateMatch = T
    if (alsoLookForStateMatch) {
        # each state code can expand to either itself, itself with periods in between, the full state, or any abbrevs 
        # listed in places$stateAbbrevs.
        statesAsPatterns = matchCountsData$state
        # easiest to code is as a loop over 50 states
        for (state in unique(places$stateAbbrevs$stateAbbrev)) {
            # grab all the long words
            stateRE1 = paste(places$stateAbbrevs$stateLong[places$stateAbbrevs$stateAbbrev==state], collapse="|")
            # append the two-letter abbrev (with or without periods)
            letter1 = substr(state, 1, 1)
            letter2 = substr(state, 2, 2)
            stateRE2 = paste0("\\b(", letter1, "\\.?", letter2, "|", stateRE1, ")\\b")
            if (state == "IN" || state == "OR" || state == "OH" || state == "DE") {
                # the two-letter abbreviations could be words, so don't match on those
                stateRE2 = paste0("\\b(", stateRE1, ")\\b")
            } else if (state == "WA") {
                # don't allow a match on just "Washington," or we'll get DC places
                stateRE2 = sub("washington", "washington state", stateRE2)
            } else if (state == "VA") {
                # don't allow a match on just "Virginia," or we'll get West Virginia places too. Match is ok at the start of 
                # a string though.
                stateRE2 = sub("virginia", "^virginia", stateRE2)
            }
            
            statesAsPatterns[statesAsPatterns==state] = rep(stateRE2, sum(statesAsPatterns==state))
        }
        locStringContainsState = mapply(grepl, statesAsPatterns, locStrings, MoreArgs=list(ignore.case=T, perl=T))
    }
    
    # Having locStringContainsCity simply override loc2 gave mistakes in both directions.
    # Let's try a more fine-grained approach: (motivated from the examples in front of me)
    # (If inferredLoc already matched the right state, don't change it.)
    # 0. if matchesCity and matchesState, it's a real match.
    # 1. if inferredLoc is "unparsed" or "USA", accept the city-match or the state-match (as helping us parse)
    # 2. if locString equals the city (with no other info), use the city-match. 
    #    (This means "Dublin" alone will match the city "Dublin, NH"; and in general if you live in a city that sounds like a bigger place,
    #     we'll match you to people who live in that bigger place.)
    #    [Alternative possibility: mark this case as "unparsed", so as not to make mistakes in either direction.]
    # 3. if inferredLoc is a different state, we should be more careful: use the city-match UNLESS it parses very clearly 
    #    as the other state--i.e., use the other state if twitter location is "city, other-state".
    # 4. if inferredLoc is NY because locString contains "NYC", then allow matchesCity or matchesState as sufficient evidence
    #    (because if we'd parsed it from scratch without the NYC, state alone would have been enough; and they may well
    #     have listed multiple locations.)
    # 5. Any other cases: if it was "foreign" but matched on city (or state?), downgrade the loc to "unparsed." 
    #    (We should no longer count it as a definitively foreign non-match; now there's some uncertainty.)
    #    (These foreign/matches on city definitely look dubious. Matches on state...can go either way, but ok, let's 
    #     consider them now uncertain too.)
    
    finalLocs = loc2
    
    # case 0
    matchesCityAndState = locStringContainsCity & locStringContainsState & (loc2 != matchCountsData$state)
    
    # cases 1 and 2 (= by far the biggest source of new matches)
    useCityOrStateMatch = (loc2 != matchCountsData$state) & (locStringContainsCity | locStringContainsState) & 
                    (loc2 == "unparsed" | loc2 == "USA" |                 # case 1
                        tolower(matchCountsData$twProfileLoc) == tolower(matchCountsData$city))    # case 2

    # case 3: inferredState is a different state
    case3 = locStringContainsCity & loc2 != matchCountsData$state & (nchar(loc2) == 2)  
    cityGuess = mapply(getInferredCity, locStrings[case3], inferredStateSeen=loc2[case3], 
                       MoreArgs=list(stateAbbrevs=places$stateAbbrevs, cityStateAbbrevs=places$cityStateAbbrevs))
    # ignore the cityMatch if it clearly parsed as a city in the alternative state
    case3UseCityMatchRowIDs = (which(case3))[tolower(cityGuess)!=tolower(matchCountsData$city[case3])]
    # change the short vector of row ids into a long vector of bools
    case3UseCityMatch = (1:nrow(matchCountsData)) %in% case3UseCityMatchRowIDs
    
    # case 4: inferredState is NY and twProfileLoc contains "NYC"
    case4 = (loc2 == "NY") & grepl("nyc", matchCountsData$twProfileLoc, ignore.case=T) & 
        (loc2 != matchCountsData$state) & (locStringContainsState | locStringContainsCity)

    # Do it!
    itsAMatch = matchesCityAndState | useCityOrStateMatch | case3UseCityMatch | case4
    finalLocs[itsAMatch] =  matchCountsData$state[itsAMatch]
    
    # case 5 = other cases: potential match, previously "foreign", and didn't satisfy match criteria 
    case5 = (locStringContainsCity | locStringContainsState) & (loc2 == "foreign") & !itsAMatch
        
    finalLocs[case5] = rep("unparsed", sum(case5))
    
    return(finalLocs)
    
}

# carefully walk through preprocessing code that can get us to the city/state call, just as was done in isLocationInUSA.
# We can assume we got some state out of this string last time.
getInferredCity = function(locString, inferredStateSeen, stateAbbrevs, cityStateAbbrevs) {
    locString = stri_trans_nfkc(locString)
    locString = stri_replace_all(locString, " ", charclass="[\\p{S}\\p{C}]")
    locString = stri_replace(locString, "", regex="\\p{Space}+$")
    locString = stri_replace(locString, "", regex="[\\p{P}\\p{Z}\\p{M}]+$")
    
    stub = checkForUSA(locString)
    stub = checkForUSRegions(stub$remainingLocString) 
    locString = sub("\\bmetro\\b", "", stub$remainingLocString, ignore.case=T, perl=T)
    locString = sub("\\barea\\b", "", locString, ignore.case=T, perl=T)
    
    locString = sub("^\\s+", "", locString, perl=T)
    locString = sub("\\s+$", "", locString, perl=T)
    locString = stri_replace(locString, "", regex="[\\p{P}\\p{Z}\\p{M}]+$")
    
    stub = isExactlyCityOrState(locString, cityStateAbbrevs, stateAbbrevs)  # if stub$hasLoc, we're back in case 2
    if (!stub$hasLoc && nchar(locString) > 0) {
        stub = looksLikeCityState(locString, stateAbbrevs) 
        if (stub$hasLoc && inferredStateSeen == stub$inferredLoc) {   # if that part doesn't match, we haven't parsed it like the real function does
            return(stub$cityGuess)
        }
    }
    return(NULL)
}

