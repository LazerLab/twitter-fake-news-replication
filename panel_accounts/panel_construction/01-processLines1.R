

# Function to read next line, with a lookahead so that we can fix stray newline characters.
# That is, always read and store one extra line, and if it's not a whole line, paste it into the current one.
getNextLineWithLookahead = function(con, nextFieldsAlreadyRead, maxFieldsPerLine=5, separator="\t") {
    line2 = readLines(con, n=1, skipNul=T)
    if (length(line2) == 0) {       # i.e., if no lines were read
        return(list(nextFieldsAlreadyRead, NULL))
    }
	fields2 = strsplit(line2, split=separator)[[1]]
    
    # If line2 ended with a tab, we've now lost it. (strstring behavior: 2 possible inputs --> 1 output) 
    # Sometimes will happen in tandem with those mid-line linebreaks. 
    # Can also happen when:
    # -there's an extra tab after the last field
    # -that was the tab before the last field, but the last field was blank
    # Append it as a new field iff the line has room for more fields.
    # (Check later to make sure it's gone lest we paste it to other "\t"s at printing time.)
    if (substr(line2, nchar(line2), nchar(line2)) == separator && length(fields2) < maxFieldsPerLine) {
        fields2 = c(fields2, separator)
    }
    
    # check whether line2 looks right: does it start with a numeric field?
    line2TwitterID = tryCatch(as.numeric(fields2[1]), warning = function(w) { NA })
    # or even if it is a number, is the id (lexicographically) less than the previous one 
    # (which doesn't happen when all is well), or do they start with different digits? (Real digit changes 
    # only happen 8 times, none of which are broken line breaks.)
    if (is.na(line2TwitterID) || 
            fields2[1] < nextFieldsAlreadyRead[1] ||
            # can't disallow the digit to ever change, so check that it only changes by going up 1 
            (substr(fields2[1], 1, 1) != substr(nextFieldsAlreadyRead[1], 1, 1) &&
                 as.numeric(substr(fields2[1], 1, 1)) != as.numeric(substr(nextFieldsAlreadyRead[1], 1, 1)) + 1)) {
        # try to fix it
             
        # presumably the two sets of fields should get pasted together
        #fixedFields = c(nextFieldsAlreadyRead[1:(length(nextFieldsAlreadyRead)-1)],
        #                paste(nextFieldsAlreadyRead[length(nextFieldsAlreadyRead)], fields2[1], sep=""),
        #                fields2[2:length(fields2)])
        
        # note: sometimes one line has multiple newlines in it. To handle that, make this call recursive.
        
        if (nextFieldsAlreadyRead[length(nextFieldsAlreadyRead)] == separator) {
            # special handling if final field is a "\t"
            prevLine = paste(nextFieldsAlreadyRead[1:(length(nextFieldsAlreadyRead)-1)], collapse=separator)
            prevLine = paste(prevLine, separator, sep="")
        } else {  # the normal case
            prevLine = paste(nextFieldsAlreadyRead, collapse=separator)
        }
        fixedLine = paste(prevLine, line2, sep="")
        fixedFields = strsplit(fixedLine, split=separator)[[1]]
        # handle final "\t" again:
        if (substr(fixedLine, nchar(fixedLine), nchar(fixedLine)) == "\t" && length(fixedFields) < maxFieldsPerLine) {
            fixedFields = c(fixedFields, separator)
        }
        
        retVal = getNextLineWithLookahead(con, fixedFields)
        
        # before returning it, print out what the fixed line looks like
        print(paste("Fixed stray newlines to produce this: ", paste(retVal[[1]], collapse=" | "), sep=""))
        return(retVal)
    }        
        
    return(list(nextFieldsAlreadyRead, fields2))
}

# function to ONLY fix embedded newlines
reprintFileFixingNewlines = function(inFile, outFile, separator="\t", numFields=5) {
    con = file(inFile, open="rt")
    conOut = file(outFile, open="wt")
    
    linesRead = 0
    lineFound = readLines(con, n=1, skipNul=T)
	fields = strsplit(lineFound, split=separator)[[1]]
    
    # buffering
    nextLine = readLines(con, n=1, skipNul=T)  
	nextFields = strsplit(nextLine, split=separator)[[1]]
    
    while (length(fields)) {
        if (fields[length(fields)] == separator && length(fields) > numFields) {
            warning("Line ended with excess fields:", paste(fields, collapse=" | "))
        }
        
        writeLines(paste(fields, collapse=separator), con=conOut)
        linesRead = linesRead + 1
        
        next2Lines = getNextLineWithLookahead(con, nextFields, maxFieldsPerLine=numFields, 
                                              separator=separator)
        fields = next2Lines[[1]]
        nextFields = next2Lines[[2]]
    }
    close(con)
    close(conOut)
    print(paste("Processed data contains", linesRead, "lines"))
}

inFile = "sample-data/01-raw-profiles.txt"
outFile = "sample-data/02-cleaner-profiles.txt"
reprintFileFixingNewlines(inFile, outFile)

