
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, stringi)


readKnownPlaceLists = function(placeListDir) {
    
    statesFile = paste0(placeListDir, "/statesTable.txt")
    stateAbbrevs = read.table(statesFile, sep="\t", as.is=T)
    stateAbbrevs[,1] = tolower(stateAbbrevs[,1])
    colnames(stateAbbrevs) = c("stateLong", "stateAbbrev")
    
    cityStatesFile = paste0(placeListDir, "/cityStateAbbrev.txt")
    cityStateAbbrevs = read.table(cityStatesFile, sep="\t", header=T, as.is=T)
    cityStateAbbrevs[,1] = tolower(cityStateAbbrevs[,1])
    cityStateAbbrevs[,2] = tolower(cityStateAbbrevs[,2])
    cityStateAbbrevs = cityStateAbbrevs[,c(2,1,3)]   # SQL used a funny column order
    # keep state abbrevs capitalized, but everything else in lower case
    
    countryFile1 = paste0(placeListDir, "/countries.txt")  # shorter list in which I added a handful of abbreviations manually
    # full list of countries + capitals in english + native language
    countryFile2 = paste0(placeListDir, "/wikipedia_countries_and_capitals_in_native_languages.txt")  
    countryFile3 = paste0(placeListDir, "/foreignEnglishSpeaking.txt")  
    countries1 = scan(countryFile1, what=character(), sep="\t", quote="")
    countries2 = scan(countryFile2, what=character(), sep="\t", quote="")
    countries3 = scan(countryFile3, what=character(), sep="\t", quote="")
    countryList = unique(tolower(c(countries1, countries2, countries3)))
    
    # fix formatting irregularities: remove blank entries that creep in
    countryList = countryList[countryList != ""]   
    # trim blanks from end of string
    countryList = sub("\\s+$", "", countryList, perl=T)
    
    # handle apostrophes: make one copy with them, another where they're deleted
    countriesWithApostrophes = grepl("'", countryList, fixed=T)
    versionWOApos = stri_replace_all_fixed(countryList[which(countriesWithApostrophes)], "'", "")
    countryList = unique(c(countryList, versionWOApos))
    
    return(list(stateAbbrevs=stateAbbrevs, cityStateAbbrevs=cityStateAbbrevs, countryList=countryList))
}

# Function looks at location field and decides if it's foreign, us, or (if possible) a specific state.
# Adds a column to the data, in output file. Prints 'NA' if location field was non-empty but was only (e.g.) symbols.
# This function does reprint lines that have no location.
writeInferredLocations = function(inFile, outFile, placeListDir, origFieldName="twProfileLoc", newFieldName="loc2") {
	inData = fread(inFile)
    places = readKnownPlaceLists(placeListDir)
    # experimental: remove diacritical marks here and now from latin chars
    places$countryList = unique(stri_trans_general(places$countryList, "latin-ascii"))

    locStrings = inData[[origFieldName]]
    inData[[newFieldName]] = mapply(isLocationInUSA, locStrings, MoreArgs=list(stateAbbrevs=places$stateAbbrevs,
                              cityStateAbbrevs=places$cityStateAbbrevs, countryList=places$countryList, breakOutKnownForeign = 1))

	fwrite(inData, file=outFile, sep="\t")

}

    
# Returns inferredState, which can be any of: 51 state abbrevs; "USA" for other usa; blank for foreign;
# NA for when there was no real text there.
# If we set breakOutKnownForeign = 1, then in place of blank, returns either "foreign" or "unparsed".
isLocationInUSA = function(locString, stateAbbrevs, cityStateAbbrevs, countryList, breakOutKnownForeign = 0) {
    # how to modify existing functionality wrt breakOutKnownForeign: always call it "foreign"
    # inside this function. When returning, remap to get either (breakOutKnownForeign == 0) "foreign" and "" ==> "", 
    # or (breakOutKnownForeign==1) "foreign" ==> "foreign", "" ==> "unparsed".

    # standardize unicode characters to the extent possible
    # -the general rule seems to be to always use NFC normalization -- that means if two chars are printed 
    # identically, they're stored identically
    # -here, I also want the "compatibility" forms -- it merges together characters that mean the same thing,
    # even if printed differently (e.g., ligature in "fi", varying character widths)
    locString = stri_trans_nfkc(locString)
    
    # stri_trans_general(locString, "latin") will transliterate everything--regardless of original alphabet--
    # into latin characters. Rather nicely. 
    # stri_trans_general(locString, "latin-ascii") will remove diacritical marks from latin characters.
    # Do we want to use these to standardize text for locations? <-- no, I guess.
    # -It could bring new false positives, by making more strings match our patterns.
    # -It could also allow new correct matches to occur.
    # -I was wondering if this would fix those times people write in "funny-looking" fonts.
    
    # Do we want to use these to test whether the text looks foreign? 
    # -Foreign strings will mostly fail our pattern matching anyway.
    # -On the other hand, we could ensure non-latin characters make a string fail.
    
    # Actually, the R base package regexes are erratic wrt non-ascii chars. The better move
    # would be to rewrite the matching using stringi. (But that syntax is different, so I haven't changed most calls.)
    
    # Replace all characters of type "symbol" or "control code" with spaces (an attempt to remove hearts and other pictograms)
    locString = stri_replace_all(locString, " ", charclass="[\\p{S}\\p{C}]")
    
    # trim any leading and trailing whitespace
    locString = stri_replace(locString, "", regex="\\p{Space}+$")
    # Remove trailing punctuation (and spaces)
    locString = stri_replace(locString, "", regex="[\\p{P}\\p{Z}\\p{M}]+$")
    
    if (is.null(locString) || nchar(locString) == 0 || is.na(locString)) {
        return(NA)
    }
 
    inferredStateAbbrev = ""
    # additional variables to keep track of what we learn
    metaInUSA = metaKnownUSLoc = metaCityState = metaForeign = F
    
    # if USA, set inferredState at the end
    stub = checkForUSA(locString)
    locString = stub$remainingLocString
    metaInUSA = metaInUSA || stub$hasUSA

    stub = checkForUSRegions(locString) 
    locString = stub$remainingLocString
    metaInUSA = metaInUSA || stub$hasUSA
   
    # More flexible patterns for NY and SF than when we look for city names later. 
    # Call this before we remove "area", so we can look for "Bay Area" specifically.
    stub = containsNY_SF(locString)
    metaKnownUSLoc = metaKnownUSLoc || stub$hasLoc
    inferredStateAbbrev = stub$inferredLoc
    
    
    # From here on out, ignore the words "area" and "metro"
    locString = sub("\\bmetro\\b", "", locString, ignore.case=T, perl=T)
    locString = sub("\\barea\\b", "", locString, ignore.case=T, perl=T)
    
    # again trim leading and trailing whitespace
    locString = sub("^\\s+", "", locString, perl=T)
    locString = sub("\\s+$", "", locString, perl=T)
    # and trailing punctuation
    locString = stri_replace(locString, "", regex="[\\p{P}\\p{Z}\\p{M}]+$")
    # caution: sometimes the trailing punctuation was part of, e.g., "d.c." or "n.y.". Later we check for such.

    
    stub = isExactlyCityOrState(locString, cityStateAbbrevs, stateAbbrevs)
    # special case we can just handle out here: "LA" should only count as "USA"
    if (locString == "LA") {
        stub$hasLoc = F
        stub$inferredLoc = "USA"
        metaInUSA = T
    }
    metaKnownUSLoc = metaKnownUSLoc || stub$hasLoc
    if (stub$hasLoc) {
        inferredStateAbbrev = stub$inferredLoc
    }
        
    # Have we killed it or found the state already?
    # Note: this version makes us return NY if we ever see NYC (more than earlier versions did).
    if (nchar(locString) == 0 || metaKnownUSLoc) {
        if (nchar(inferredStateAbbrev) == 0 && metaInUSA) {
            return("USA")
        } else {
            return(remapCoding(inferredStateAbbrev, breakOutKnownForeign))
        }
    }
    
    # Rough/approximate match for things like "city, state": see if the final word is a state.
    stub = looksLikeCityState(locString, stateAbbrevs) 
    # note: not returning any changes to locString 
    # (currently those are: delete apostrophes, and "delete period if preceded and followed by a single letter",
    # to make up for trailing punctuation deletion earlier)
    if (stub$hasLoc) {       
        # "on second thought": sometimes it looks like a "city, state" but should be reclassified otherwise
        stub = handleSpecialCases(stub$inferredLoc, locString)
        inferredStateAbbrev = stub$inferredLoc
        
        # get meta with results
        metaCityState = metaCityState || stub$hasOldLoc
        if (nchar(inferredStateAbbrev) > 0 && inferredStateAbbrev == "foreign") {
            metaForeign = T
        } else if (nchar(inferredStateAbbrev) > 0 && inferredStateAbbrev == "USA") {
            metaInUSA = T
        }
    }
    
    # Is it a known foreign country?
    foreignLoc = hasForeignLoc(locString, countryList)
    if (foreignLoc) {
        metaForeign = T
    }
    
    # Only definitely mark it as foreign if we didn't already find evidence of the US. (There could always be both; 
    # want to err on the side of calling things US, to be conservative.)
    if (metaForeign && !metaCityState && !metaKnownUSLoc && !metaInUSA) {
        inferredStateAbbrev = "foreign"
    }
  
    # Finally: if it's in USA but nothing more specific, use that.
    if (nchar(inferredStateAbbrev) == 0 && metaInUSA) {
        inferredStateAbbrev = "USA"
    }
    
    return(remapCoding(inferredStateAbbrev, breakOutKnownForeign))
    
}    

checkForUSA = function (locString) {
    # does it contain "USA" or "U.S.A." as a word (case insensitive)?
    # Or "United States".
    # \b means "word boundary is here"
    # if we find any of those expressions, remove them, so we can keep looking for a state
    hasUSA = grepl("\\bUSA\\b", locString, ignore.case=T, perl=T, fixed=F)  
    if (hasUSA) {
        locString = sub("\\bUSA\\b", "", locString, ignore.case=T, perl=T)
    }
    # \W means "non-word character" (letter or digit)
    hasUSA2 = grepl("\\bU\\.S(\\.A\\.?)?", locString, ignore.case=T, perl=T, fixed=F)  
    if (hasUSA2) {
        locString = sub("\\bU\\.S(\\.A\\.?)?", "", locString, ignore.case=T, perl=T)
    }
    hasUS = grepl("\\bunited\\ states\\b", locString, ignore.case=T, perl=T, fixed=F)
    if (hasUS) {
        locString = sub("\\bunited\\ states\\b", "", locString, ignore.case=T, perl=T)
    }
    isUS = (tolower(locString) == "us" || tolower(locString) == "u.s.")
    if (isUS) {
        locString = ""  # we've eaten it all
    }
    return(list(remainingLocString = locString, hasUSA = (hasUSA || hasUSA2 || hasUS || isUS)))
}

checkForUSRegions = function(locString) {
    # does it contain any of the terms "west coast", "east coast", "midwest", "new england"?
    # again, if we find any of those expressions, remove them, so we can keep looking for a state
    hasWCoast = grepl("\\bWest\\ Coast\\b", locString, ignore.case=T, perl=T, fixed=F) 
    hasECoast = grepl("\\bEast\\ Coast\\b", locString, ignore.case=T, perl=T, fixed=F) 
    if (hasWCoast || hasECoast) {
        locString = sub("\\b(West|East)\\ Coast\\b", "", locString, ignore.case=T, perl=T)
    }
    hasMidWest = grepl("\\bMidwest\\b", locString, ignore.case=T, perl=T, fixed=F) 
    if (hasMidWest) {
        locString = sub("\\bMidwest\\b", "", locString, ignore.case=T, perl=T)
    }
    hasNEngland = grepl("\\bNew\\ England\\b", locString, ignore.case=T, perl=T, fixed=F) 
    if (hasNEngland) {
        locString = sub("\\bNew\\ England\\b", "", locString, ignore.case=T, perl=T)
    }
    return(list(remainingLocString = locString, hasUSA = (hasWCoast || hasECoast || hasMidWest || hasNEngland)))
    
} 

containsNY_SF = function(locString) {
    inferredStateAbbrev = ""
    # special cases for NYC and San Fran
    hasNYC = grepl("\\bNYC\\b", locString, ignore.case=T, perl=T, fixed=F)
    if (hasNYC) {
        inferredStateAbbrev = "NY"
    }
    # matching on "san francisco" anywhere will give false positives, because there are international SF's
    # here, allow it at the end of the string. (The international ones usually have a suffix.)
    hasSFBay1 = grepl("\\bSan\\s*Fran(cisco)?\\b($|\\s+Bay(\\ Area)?\\b)", locString, ignore.case=T, perl=T, fixed=F)
    # match on "Bay Area" anywhere, either alone or prefixed by "San Fran", "SF", etc.
    hasSFBay2 = grepl("((SF|S\\.F\\.|San\\ Fran(cisco)?)\\ )?\\bBay\\ Area\\b", locString, 
                      ignore.case=T, perl=T, fixed=F)
    hasSoCal = grepl("\\bSoCal\\b", locString, ignore.case=T, perl=T, fixed=F)
    if (hasSFBay1 || hasSFBay2 || hasSoCal) {
        inferredStateAbbrev = "CA"
    }
    return(list(hasLoc = (hasNYC || hasSFBay1 || hasSFBay2 || hasSoCal),
                inferredLoc=inferredStateAbbrev))
}

isExactlyCityOrState = function(locString, cityStateAbbrevs, stateAbbrevs) {
    inferredStateAbbrev = ""
    # is it a major US city? (And did we delete the ending period in "Washington, D.C."?)
    isACity = tolower(locString) %in% cityStateAbbrevs$city || tolower(paste0(locString, ".")) %in% cityStateAbbrevs$city
    if (isACity) {
        indices = match(c(tolower(locString), tolower(paste0(locString, "."))), cityStateAbbrevs$city)
        index = indices[!is.na(indices)]
        inferredStateAbbrev = cityStateAbbrevs$stateAbbrev[index]
    }
    
    # is it exactly a US state or standard abbrev or a (*capitalized*) 2-letter abbreviation?
    isAState = tolower(locString) %in% stateAbbrevs$stateLong
    if (isAState) {
        inferredStateAbbrev = stateAbbrevs$stateAbbrev[match(tolower(locString), stateAbbrevs$stateLong)]
    }
    isAStateAbbrev = locString %in% stateAbbrevs$stateAbbrev
    # todo: see if we should also accept cases where the person put "Mi" or even "mi" instead of "MI".
    if (isAStateAbbrev) {
        inferredStateAbbrev = locString
    }
    return(list(hasLoc=(isACity || isAState || isAStateAbbrev), inferredLoc=inferredStateAbbrev))
}


# Is the final word a state or state abbreviation (plus optional 5-digit zip code)? (case insensitive)
looksLikeCityState = function(locString, stateAbbrevs) {

    # delete apostrophes 
    locString = gsub("\\'", "", locString, perl=T)
    # delete periods if preceded and followed by a single letter
    locString = gsub("\\b(\\w)\\.(\\w)\\b", "\\1\\2", locString, perl=T)
	# divide the string at non-word characters
    tokens = stri_split_regex(locString, "[^\\p{L}\\p{N}]+", perl=T, omit_empty=T)[[1]] 

    isCityState = F
    possibleCityName = inferredStateAbbrev = ""
    if (length(tokens)) {
        # ignore zip code (or other numbers) or "US" if that's at the end
        if (tolower(tokens[length(tokens)]) == "us") {
            tokens = tokens[1:(length(tokens) - 1)]
        }
        if (grepl("^\\d+$", tokens[length(tokens)], perl=T, fixed=F)) {
            tokens = tokens[1:(length(tokens) - 1)]
        }
        finalWord = tolower(tokens[length(tokens)])
        # states can be up to 3 words long ("district of columbia")
        final2Words = final3Words = ""
        if (length(tokens) >= 2) {
            final2Words = tolower(paste(tokens[(length(tokens)-1) : length(tokens)], collapse=" "))
        }
        if (length(tokens) >= 3) {
            final3Words = tolower(paste(tokens[(length(tokens)-2) : length(tokens)], collapse=" "))
        }
        
        if (finalWord %in% tolower(stateAbbrevs$stateAbbrev)) {
            isCityState = T
            inferredStateAbbrev = toupper(finalWord)
            if (length(tokens)-1 >= 1) {
                possibleCityName = paste(tokens[1:(length(tokens)-1)], collapse=" ")
            }
        } else if (sum(c(finalWord, final2Words, final3Words) %in% stateAbbrevs$stateLong)) {
            isCityState = T
            # expect 2 NA's here
            # precedence to longest match (else it thinks "West Virginia" means "Virginia").
            indices = match(c(final3Words, final2Words, finalWord), stateAbbrevs$stateLong)  
            # just in case it's possible to get more than one, take the first
            index = (indices[!is.na(indices)])[1] 
            inferredStateAbbrev = stateAbbrevs$stateAbbrev[index]
            
            numTokensForState = 4 - (which(!is.na(indices)))[1]
            if (length(tokens) - numTokensForState >= 1) {
                possibleCityName = paste(tokens[1:(length(tokens) - numTokensForState)], collapse=" ")
            }
        }
        
    } else {
        isCityState = F
        inferredStateAbbrev = ""
    }
    return(list(hasLoc=isCityState, inferredLoc=inferredStateAbbrev, cityGuess=possibleCityName))
}

# Special cases = where it looked like it ended in a state or state abbrev, but it might mean something different
handleSpecialCases = function(inferredStateAbbrev, locString) {
    stillGood = T
    isKnownForeign = F   # (new!)
    # special case for Maine: must end in ", me" (plus optional zip); otherwise we get phrases ending with the word "me"
    if (inferredStateAbbrev == "ME") {
        # bugfix done for Maine
        stillGood = grepl("(\\bMaine\\b)|,\\ me(\\ \\d{5})?$|^ME$", locString, ignore.case=T, perl=T, fixed=F)
    }
    # special case for Indiana: must not have numbers after it, such as "found in 101". Unless it's a 5-digit zip.
    # Also, in word list, important not to allow "Ind." for indiana; otherwise we get all kinds of Indonesia 
    # entries there.
    if (inferredStateAbbrev == "IN") {
        looksLikeZip = grepl("\\bin\\s+\\d{5}$", locString, ignore.case=T, perl=T, fixed=F)
        if (!looksLikeZip) {
            stillGood = ! grepl("\\bin\\s+\\d+", locString, ignore.case=T, perl=T, fixed=F)
        }
    }
    
    
    # special case for DC: if prefaced by a city, city must be Washington
    if (inferredStateAbbrev == "DC" && grepl("\\w+.*d\\.?c\\.?", locString, ignore.case=T, perl=T, fixed=F)) {
        # fixed: allow "D.C." also
        isWashington = grepl("washington(,|\\s)+d\\.?c\\.?(\\ \\d{5})?$", locString, ignore.case=T, perl=T, fixed=F)
        stillGood = isWashington

        # based on match results I see, I don't want to be marking these as foreign. When it's not Washington, 
        # often it's a different US place. (In the raw twitter data, I saw DC that stood for "district capital" in 
        # various countries, but these seem to be rare among the candidate matches.)
        #isKnownForeign = !isWashington
    }
    
    # special case for CA: if we see the word "Baja", it's foreign
    if (inferredStateAbbrev == "CA") {
        isBaja = grepl("\\bBaja\\b", locString, ignore.case=T, perl=T, fixed=F)
        stillGood = !isBaja
        isKnownForeign = isBaja
    }
    
    # special case for ID: exclude cities in Indonesia
    if (inferredStateAbbrev == "ID") {
        indonesia = grepl("\\b(jakarta|bandung|Yogyakarta|Surabaya|Malang|Semarang|Bogor|bali|java)\\b",
                          locString, ignore.case=T, perl=T, fixed=F)
        stillGood = !indonesia
        isKnownForeign = indonesia
    }
    
    # watch for the country of georgia
    if (inferredStateAbbrev == "GA") {
        isCountry = grepl("\\b(Tbilisi|Kutaisi)\\b", locString, ignore.case=T, perl=T, fixed=F)
        stillGood = !isCountry
        isKnownForeign = isCountry
    }
    
    # watch for Michoacán, Mexico
    if (inferredStateAbbrev == "MI") {
        isCountry = stri_detect_regex(locString, 
                        paste0("\\b(Apatzing.n|Churumuco|Hidalgo|Coalcom.n|Jacona|Jiquilpan|Jungapeo|La\\ Huacana\\ Cavadas|L.zaro|",
                            "C.rdenas|Los\\ Reyes|Morelia|Nueva\\ Italia|Pátzcuaro|",
                           "Puruandiro|Sahuayo\\ de\\ Morelos|Santa\\ Ana\\ Maya|Tac.mbaro|Tangancícuaro|",
                           "Tepalcatepec|Uruapan|Venustiano|Carranza|Yur.cuaro|Zacapu|Zamora|Zit.cuaro)\\b"),
                        case_insensitive=TRUE)
        stillGood = !isCountry
        isKnownForeign = isCountry
    }
        
    
    # states in brazil
    brazilStates = c("SC", "MA", "AL", "MS", "MT", "PA")
    if (inferredStateAbbrev %in% brazilStates) {
        isBrazil = grepl("brasil", locString, ignore.case=T)
        if (!isBrazil) {
            if (inferredStateAbbrev == "SC") {
                # I'm letting '.' stand in for letters with diacritical marks
                isBrazil = stri_detect_regex(locString, "\\b(Florian.polis|Joinville|Blumenau|S.o Jos.|Crici.ma|Chapec.|Itaja.|Jaragu.\\ do\\ Sul|Lages|Palho.a|Balne.rio\\ Cambori.|Brusque|Tubar.o)\\b", case_insensitive=TRUE)
            }
            if (inferredStateAbbrev == "MA") {
                # fixed: make "Cape Cod, MA" still come out in Massachusetts
                isBrazil = stri_detect_regex(locString, "\\b(S.o\\ Lu.s|Imperatriz|S.o\\ Jos.\\ de\\ Ribamar|Caxias|Timon|Cod\\w|Pa.o\\ do\\ Lumiar|A.ail.ndia|Bacabal)\\b", case_insensitive=TRUE)
            }
            if (inferredStateAbbrev == "AL") {
                isBrazil = stri_detect_regex(locString, "\\b(Macei.|Arapiraca|Rio\\ Largo|Palmeira\\ dos\\ .ndios)\\b", case_insensitive=TRUE)
            }
            if (inferredStateAbbrev == "MS") {
                isBrazil = stri_detect_regex(locString, "\\b(Campo\\ Grande|Dourados|Tr.s Lagoas|corumb.)\\b", case_insensitive=TRUE)
            }
            if (inferredStateAbbrev == "MT") {
                # no one's in the real montana!
                isBrazil = stri_detect_regex(locString, "\\b(Cuiab.|Rondon.polis|Tangar.\\ da\\ Serra|barra\\ do\\ gar.as|V.rzea\\ Grande|Sinop|C.ceres|Sorriso|Primavera\\ do\\ Leste|lucas do rio verde|Alta Floresta|Juina|Marcel.ndia)\\b", case_insensitive=TRUE)
            }
            if (inferredStateAbbrev == "PA") {
                isBrazil = stri_detect_regex(locString, "\\b(Bel.m|Ananindeua|Santar.m|Marab.|Castanhal|Parauapebas|Abaetetuba)\\b", case_insensitive=TRUE)
            }
        }
        stillGood = !isBrazil
        isKnownForeign = isBrazil
    }
            
    # Filipino phrase commonly used as location
    if (inferredStateAbbrev == "MO") {
        isFilipino = grepl("\\bsa\\ puso\\ mo\\b", locString, ignore.case=T, perl=T, fixed=F)
        stillGood = !isFilipino
        isKnownForeign = isFilipino   # making a judgment call that it puts them outside the US
    }
    
    # make sure it's colorado, not columbia or argentina
    # (though it could always still be county or company)
    if (inferredStateAbbrev == "CO") {
        isColumOrArg = stri_detect_regex(locString, "\\b(Bogot.|Medell.n|Cali|Barranquilla|Cartagena|C.cuta|Bucaramanga|Ibagu.|Soledad|Pereira|Santa Marta|Soacha|cutral)\\b", case_insensitive=TRUE)
        stillGood = !isColumOrArg
        isKnownForeign = isColumOrArg
    }
    
    # [cases more or less handled above]
    # Are there really that many Americans from AR, ID, IN, MA, and LA? Some manual checking...
    # AR: some argentina, some indonesia??, philippines IN: some indiana, one from turkey?  uh, hard to tell, it seems.
    # ID: mostly indonesia. But one Idaho.
    # (ah, but it comes out different when I stop looking at ids from the top of the list.)
    # (I'd guess ID is indonesia, IN might often be india, and AR might often be argentina.)
    
    # LA: los angeles or louisiana? I think it's more often the city, but let's generalize to "USA"
    if (inferredStateAbbrev == "LA") {
        # but allow it to be louisiana if it ends in ", LA" (plus optional zip), case-insensitive
        hasLouisiana = grepl("(\\bLouisiana\\b)|,\\ la(\\ \\d{5})?$", locString, ignore.case=T, perl=T, fixed=F)
        if (! hasLouisiana) {
            # look for major Louisiana cities: New Orleans, Baton Rouge, Shreveport, Metairie, Lafayette
            hasLouisiana = grepl("\\b(New\\ Orleans|Baton\\ Rouge|Shreveport|Metairie|Lafayette)\\b", locString, ignore.case=T, perl=T, fixed=F)
        }
        if (! hasLouisiana) {
            inferredStateAbbrev = "USA"
            stillGood = F
        }
    }
    
    # check for full country names
    if (stillGood) {
        isArgenIndon = grepl("\\b(indonesia|argentina)\\b", locString, ignore.case=T, perl=T, fixed=F)
        stillGood = !isArgenIndon
        isKnownForeign = isArgenIndon
    }
    
    # downgrade these look-like states to foreign locations or blank
    if (!stillGood) {
        if (isKnownForeign) {
            inferredStateAbbrev = "foreign"
        } else if (inferredStateAbbrev != "USA") {  # if "USA", don't change it
            inferredStateAbbrev = ""
        }
    }
    
    return(list(hasOldLoc=stillGood, inferredLoc=inferredStateAbbrev))
}

hasForeignLoc = function(locString, countryList) {
    
    # case 1: it's foreign if all the letters are in a non-latin script
    numLetters = stri_count_charclass(locString, "\\p{Letter}")
    numLatinLetters = stri_count_charclass(locString, "\\p{script=latin}")
    if (numLetters > 0 && numLatinLetters == 0) {
        return(T)
    }
    
    # put it in ascii to match what writeInferredLocations did 
    locSimpler = stri_trans_general(tolower(locString), "latin-ascii")

    # does it start or end with a country or capital from my master list?
    # (make sure the country words are at word boundaries)
    countriesAsPatterns = paste0("\\b", countryList, "$|^", countryList, "\\b")
    t2 = mapply(stri_detect_regex, pattern=countriesAsPatterns, MoreArgs=list(str=locSimpler))

    return(sum(t2) > 0)   # say "foreign" using the most lenient criteria
}


remapCoding = function(inferredStateAbbrev, breakOutKnownForeign) {
    if (!breakOutKnownForeign) {
        if (nchar(inferredStateAbbrev) > 0 && inferredStateAbbrev == "foreign") {
            inferredStateAbbrev = ""
        }
    } else {
        if (inferredStateAbbrev == "") {
            inferredStateAbbrev = "unparsed"
        }
    }
    return(inferredStateAbbrev)
}

    
# special cases:
# -removed PR and other territories
# -added united states (as standalone), but not america (since occurs in latin america, sudamerica)
# -added terms midwest, east coast, west coast
# -"me" as a word, vs. state abbrev
# -pittsburg, PA 02134
# -nyc anywhere; sf or bay area for san francisco
# -unicode handling / normalization
# -metro <xx>, <xx> area
# -bogotá, DC [= Distrito Capital] <-- fixed
# -mexicali, baja california; Jakarta, ID; Tbilisi,Georgia; "Sa Puso Mo" = Filipino phrase
# -those pesky brazilian states that have the same abbrevs as ours; also Italian
# -some brazilian states: MA, SC, AL, MS, MT, PA
# -Calif.; Philly
# -Co can mean company, county or columbia
# -Minn. for Minnesota; Mich; Mass; Miss; etc. --> Added list from http://www.stateabbreviations.us/ (though wish I'd made it a separate column)
# -Oklahoma City fell out of the city list 
# -cities: Shreveport, New Haven, Vegas
# -places like PR: put them back, to identify as foreign

