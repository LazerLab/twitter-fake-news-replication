

# Function to pull out Twitter profiles whose names match each voter. 
#
# Iterates through voter file. 
# For each voter, clean the name, query the DB for matches, and write out all matches. 
# Maximum of 10 matches printed per voter (unless specified otherwise). 
# Caches name + results so that if voter file is sorted by name, we can reuse results from previous line.

if (!require("pacman")) install.packages("pacman")
pacman::p_load(RMySQL)

options(warn=1)             # print warnings as they occur
source("processNames.R")   	# for getNameWordsFromVoter()

# Very important for speed: These tables' indexes need to be pre-loaded into the key cache (takes a few (3-4) minutes).
alreadyInKeyCache = FALSE      # TRUE if it's been done manually; otherwise, will do it after opening db connection.
# table(s) with profile data: 
DBtables = c("exampleProfiles")

# All-purpose function we want.
# Input: voter file, a tab-delimited file containing at least the columns: voter_id, first & last names, city, state.
# Outputs:
#   -outfileForCandMatches is .tsv file of all good-looking matches, to use in next step of matching.
#   -voterfileOut [optional] shows number of matches per voter
countTwitterVoterMatches = function(voterfileIn, outfileForCandMatches, 
				startWithRecord=1, stopAfterRecord=NULL, matchCountsFile = NULL, maxMatchesToSave=10,
				uniquenessFilterField=NULL, uniquenessFilterCount=1) {
    
    print(paste("Args to main function: voterfileIn=", voterfileIn, ", outfileForCandMatches=", outfileForCandMatches, 
        ", startWithRecord=", startWithRecord, ", stopAfterRecord=", stopAfterRecord, ", matchCountsFile=", matchCountsFile,
	", uniquenessFilterField=", uniquenessFilterField, ", uniquenessFilterCount=", uniquenessFilterCount))
    
	# (keeping "_count" cols to make it easier to keep track of which filter has been applied)
	inputColnamesToKeep = c("voter_id", "first_name", "middle_name", "last_name", "city", "state", "county_count", "city_count", "state_count", "zipcode_count")
	inputSep = "\t"

    # db connection
    dbCon = initializeDBCon()
    
    # in/out files
    con = file(voterfileIn, open="rt")
	twOut2 = file(outfileForCandMatches, open="wt")
    if (!is.null(matchCountsFile)) {
		conCnts = file(matchCountsFile, open="wt")
	}
    
    # read and write headers

	# Try speeding up the skipping of many lines using readLines:
	lines = readLines(con=con, n = startWithRecord)		# gets header, but stops before the data record we want.
	numRecordsRead = length(lines) - 1 			# "lines" contains header
	# save the header
	# Must use colClasses = "character" to avoid initials of F or T being expanded to FALSE or TRUE
	inputColnamesFixed = read.csv(text=lines[1], header=F, stringsAsFactors=F, colClasses = "character", sep=inputSep)
	# get our starting data record 
	lineFound = readLines(con, n=1)
	if (length(lineFound)) {
		fields = read.csv(text=lineFound, header=F, nrows=1, stringsAsFactors=F, colClasses = "character", sep=inputSep)
	}
	numRecordsRead = numRecordsRead + length(lineFound)	
	rm(lines)	# cleanup
	gc()		# (playing around with how to keep memory usage low)

	# Now the header's stored in inputColnamesFixed, we've set numRecordsRead, and fields contains the first line

	if (startWithRecord > numRecordsRead) {
		print(paste("Supposed to start with row", startWithRecord, ", but file only contains", numRecordsRead, "rows; exiting countTwitterVoterMatches()"))
		close(con)
		close(twOut2)
		dbDisconnect(dbCon)
		if (!is.null(matchCountsFile)) {
			close(conCnts)
		}
		return()
	}


	inputColsToKeep = which(inputColnamesFixed %in% inputColnamesToKeep)	# a vector of indices
	colsToQuote = length(inputColsToKeep) + 2:4   # from DB's Twitter profiles, quote fields 2-4: name, handle, loc

	# Identify cols we need later
	firstNameColNum = which(inputColnamesFixed == "first_name")
	lastNameColNum = which(inputColnamesFixed == "last_name")
	filterColNum = which(inputColnamesFixed == uniquenessFilterField)

	writeLines(paste(c(inputColnamesFixed[inputColsToKeep], "twProfileID", "twProfileName", "twProfileHandle", "twProfileLoc", "nameHandleWords"),
				 collapse="\t"), con=twOut2)

    if (!is.null(matchCountsFile)) {
		writeLines(paste(c(inputColnamesFixed[inputColsToKeep], "numCandidateMatches"), collapse="\t"), con=conCnts)
	}

	lineCount = startWithRecord - 1
	queryCountTimes = c()
	querySelectTimes = c()

	# for caching
	prevFullName = ""
	prevNumMatches = ""
	prevProfiles = twProfiles = NULL

    while (length(fields)) {

		# Get name
		# fields is a data frame with 1 row
		fields[is.na(fields)] = ""
		firstName = fields[1,firstNameColNum]
		lastNames = fields[1,lastNameColNum]
		currFullName = paste0(firstName, "|", lastNames)
			
		# Preprocess into nameWords
		# a vector of lower-case words with punctuation removed
		firstNameWords = getNameWordsFromVoter(firstName)
		lastNameWords = getNameWordsFromVoter(lastNames)
		nameWords = c(firstNameWords, lastNameWords)

        # Search for this record only if there's a >1-letter word in both firstName and lastName fields.
        # (MySQL has been started with special flags so that all words of 2 or more characters will be active in queries)
        if (!( sum(nchar(firstNameWords) > 1) && sum(nchar(lastNameWords) > 1))) {
            # this will print in the matchCounts file
            totNumMatches = -99		# special code to mean "skipped this record"
            
        } else if (!is.null(uniquenessFilterField) && fields[1,filterColNum] > uniquenessFilterCount) {
			totNumMatches = -98		# special code to mean "not unique enough to search for"

		} else if (currFullName == prevFullName) {
			# reuse cached results
			totNumMatches = prevNumMatches
			twProfiles = prevProfiles

        } else {
            startTime = Sys.time()
			totNumMatches = countTwitterProfiles(dbCon, nameWords, maxOk=maxMatchesToSave)	# save time by only retrieving records when the number is small
			endTime = Sys.time()
			queryCountTimes = c(queryCountTimes, as.numeric(endTime - startTime, units="secs"))

			if (totNumMatches > 0 && totNumMatches <= maxMatchesToSave) {
				startTime = Sys.time()
				twProfiles = getAllTwitterProfiles(dbCon, nameWords)	# data.frame with columns: id, name, handle, locationString, inferredLoc
				endTime = Sys.time()
				querySelectTimes = c(querySelectTimes, as.numeric(endTime - startTime, units="secs"))
			}
			
			# caching only applies to names that actually got queried
			prevFullName = currFullName
			prevProfiles = twProfiles
			prevNumMatches = totNumMatches
		}
            
		# Write out candidate matches, as input to further matching rules 
		if (totNumMatches > 0 && totNumMatches <= maxMatchesToSave) {
			write.table(cbind(fields[inputColsToKeep], twProfiles),
				file=twOut2, quote=colsToQuote, sep="\t", row.names=F, col.names=F, qmethod="d")
		}

		if (!is.null(matchCountsFile)) {
				writeLines(paste(c(fields[inputColsToKeep], totNumMatches), collapse="\t"), con=conCnts)
		}

		lineCount = lineCount + 1
		if (lineCount %% 100 == 0) {
			print(paste("finished line", lineCount))
			flush(twOut2)
			if (!is.null(matchCountsFile)) {
				flush(conCnts)
			}
			if (length(queryCountTimes)) {
				print(paste("Timing for count(*) calls: mean=", round(mean(queryCountTimes), 3), "sec, var=", var(queryCountTimes)))
				#print(paste(queryCountTimes, collapse=" "))
			} else {
				print("No count(*) calls")
			}
			if (length(querySelectTimes)) {
				print(paste("Timing for select(*) calls: num=", length(querySelectTimes), ", mean=", round(mean(querySelectTimes), 3), 
					    "sec, var=", var(querySelectTimes)))
			} else {
				print("No select(*) calls")
			}
			queryCountTimes = c()
			querySelectTimes = c()
		}
		if (!is.null(stopAfterRecord) && stopAfterRecord == lineCount) {
		    break
		}
        
        lineFound = readLines(con, n=1)
        fields = c()
        if (length(lineFound)) {
			fields = read.csv(text=lineFound, header=F, nrows=1, stringsAsFactors=F, colClasses = "character", sep=inputSep)
        }
    }
	
    close(con)
	close(twOut2)
    dbDisconnect(dbCon)
    if (!is.null(matchCountsFile)) {
		close(conCnts)
    }
}



# Returns a number.
# maxOk further short-circuits counting by:
#  -not looking at additional tables after maxOk is surpassed
#  -using "limit maxOk+1" to shorten subquery too
countTwitterProfiles = function(dbCon, nameWords, maxOk=-1) {
    matchString = paste0("match(nameHandleWords) against('", paste0("+", nameWords, collapse=" "), "' in boolean mode)")
    subqLimit = ifelse(maxOk > 0, maxOk + 1, 100)
    
    currentCnt = 0
    for (dbT in DBtables) {
    	# avoid long queries using this cap:
    	query = paste("select count(id) as cnt from (select id from", dbT, "where", matchString, "limit", subqLimit, ") as a")
        res1 = dbGetQuery(dbCon, query)
        currentCnt = currentCnt + res1$cnt
        if (maxOk > 0 && currentCnt > maxOk) {
            return(subqLimit)   # better than currentCnt b/c gives everyone the same number for "past limit"
        }
    }
	return(currentCnt)
}

# Returns a data.frame of results. If no hits, result contains 0 rows.
getAllTwitterProfiles = function(dbCon, nameWords) {
    matchString = paste0("match(nameHandleWords) against('", paste0("+", nameWords, collapse=" "), "' in boolean mode)")
    
    for (dbT in DBtables) {
        query = paste("select * from", dbT, "where", matchString)
        res1 = dbGetQuery(dbCon, query)
        if (!exists("profiles")) {
            profiles = res1
        } else {
            profiles = rbind(profiles, res1)
        }
    }
	
	return(profiles)
}


initializeDBCon = function() {
    # db connection
    # will only work from the achtung cluster
    dbCon = dbConnect(dbDriver("MySQL"), user="yourDetailsHere", password="yourDetailsHere", 
                      host="yourDetailsHere", dbname="yourDBHere")
    # right away, increase tmp table size, so it doesn't spend eons writing stuff to disk
    query = "SET tmp_table_size = 1024 * 1024 * 64" # 64M
    queryRes = dbSendQuery(dbCon, query)
    dbClearResult(queryRes)
    query = "SHOW VARIABLES LIKE 'tmp_table_size'"
    queryRes  = dbSendQuery(dbCon, query)
    answer = dbFetch(queryRes)
    print(answer)
    dbClearResult(queryRes)
    
    # magic potion to make SQL actually speak to us in Unicode 
    # (via http://stackoverflow.com/questions/12869778/fetching-utf-8-text-from-mysql-in-r-returns)
    queryRes = dbSendQuery(dbCon, "SET NAMES utf8")
    
    if (!alreadyInKeyCache) {
        query = paste("LOAD INDEX INTO CACHE", paste(DBtables, collapse=", "))
        queryRes  = dbSendQuery(dbCon, query)
        answer = dbFetch(queryRes)
        print(answer)
        dbClearResult(queryRes)
    }
        
    
    return(dbCon)
}


