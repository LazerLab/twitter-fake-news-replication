
if (!require("pacman")) install.packages("pacman")
pacman::p_load(compiler) # for speed
enableJIT(3)  

source("processNames.R")

writeIndexTerms = function(inFile, outFile, numFieldsInOrigFile, appendToOutputFile=F, inputIsCompressed=F,
                            startWhenID=NULL) {

    if (inputIsCompressed) {
        con = gzfile(inFile, open="rt", encoding="UTF-8")
    } else {
        con = file(inFile, open="rt", encoding="UTF-8")
    }

    if (!is.null(startWhenID)) {
        okToStart = F
    } else { 
        okToStart = T
    }

    if (appendToOutputFile) {
        conOut = file(outFile, open="at")
    } else {
        conOut = file(outFile, open="wt")
    }
    
    firstLine = T
    # automatically strips enclosing quotes, reads all fields as class character
    fields = scan(con, what=as.list(""), nmax=1, sep="\t", quiet=T, na.strings=NULL, skipNul=T, quote="\"")[[1]]

    while (length(fields)) {

        if (firstLine) {
            firstLine = F
            if (grepl("date_created", fields[2])) {
                if (okToStart) {
                    writeLines( paste(c(fields, "nameHandleWords"), collapse="\t"), con=conOut)
                }
                fields = scan(con, what=as.list(""), nmax=1, sep="\t", quiet=T, na.strings=NULL, skipNul=T, quote="\"")[[1]]
                next
            }
        }

        if (!okToStart) {
            id = fields[1]
            if (as.numeric(id) == startWhenID) {
                okToStart = T
            } else {
                fields = scan(con, what=as.list(""), nmax=1, sep="\t", quiet=T, na.strings=NULL, skipNul=T, quote="\"")[[1]]
                next
            }
        }
        
        name = fields[3]
        handle = fields[4]
        
        # list with components nameWords, handleWords, goodMatch
        indexResults = putThemTogether(name, handle, printSummary=F)
        
        fieldsNoNAs = fields[1:numFieldsInOrigFile]
        fieldsNoNAs[is.na(fieldsNoNAs)] = ""
        writeLines(paste0(paste(fieldsNoNAs, collapse="\t"), "\t", paste(indexResults$nameWords, collapse=" "), " ", 
                          paste(indexResults$handleWords, collapse=" ")), con=conOut)
        
        fields = scan(con, what=as.list(""), nmax=1, sep="\t", quiet=T, na.strings=NULL, skipNul=T, quote="\"")[[1]]
    }
    close(con)
    close(conOut)
}


numFieldsInOrigFile = 5     # fields 3 & 4 must contain name and handle; this var says how many cols to reprint
infile = "sample-data/02-cleaner-profiles.txt"
outfile = "sample-data/03-profiles-name-words.txt"
writeIndexTerms(infile, outfile, numFieldsInOrigFile)
