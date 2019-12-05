source("matchPeople3.R")
source("matchFinish.R")
source("processLocations.R")
placeListDir = "wordLists"
voterfileIn = "sample-data/10-voter-data.txt"
voterfileOut = "sample-data/11-match-counts.txt"         
candMatchFile = "sample-data/11-candidate-matches.txt"     
candMatchFileWithLocs = "sample-data/12-cand-matches-locs.txt"
matchFileOut = "sample-data/13-matches.txt"

doEverything = function() {

    # 1. match voter file against SQL database, saving up to 10 candidate matches per voter
    countTwitterVoterMatches(voterfileIn, candMatchFile, matchCountsFile=voterfileOut, uniquenessFilterField="state_count")
        
    # 2. infer locations for candidate matches 
	writeInferredLocations(candMatchFile, candMatchFileWithLocs, placeListDir)
	# update them after checking voter's city for candidate matches
	addNewLocToFile(candMatchFileWithLocs, candMatchFileWithLocs, placeListDir)
    
    # 3. decide which candidate matches are certain enough 
	# Updates the inferred location, along the way, after checking voter's city
    matchTheMatches(voterfileIn=voterfileIn, matchFileIn=candMatchFileWithLocs,
                   fullMatchFileOut=matchFileOut, matchFileFormat=4, placeListDir=placeListDir)

}

doEverything()
