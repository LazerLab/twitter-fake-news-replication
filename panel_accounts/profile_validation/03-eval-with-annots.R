# 1. Prep for annotations. 
# --> I copied/pasted the first 200 lines of panel_sample_profiles.csv into shared doc, keeping only the user_id and profile_pic columns

# 2. After annotations, I copied/pasted each person's answers back to the main sheet, standardized coding (caps, etc.), then saved it as annotations.tsv locally.
# Join back with true values, to compute various stats of agreement.

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, bit64, irr)	# libraries needed

privateDataDir = "private_data"
voterD = fread(file.path(privateDataDir, "panel_sample.csv"))
twitterD = fread(file.path(privateDataDir, "panel_sample_profiles.csv"))
annotD = fread(file.path(privateDataDir, "annotations.tsv"))
colnames(annotD) = c("user_id", "profile_pic", "annotator1", "hasPerson1", "gender1", "age1", "ethnicity1", "V8", "annotator2", "hasPerson2", "gender2", "age2", "ethnicity2")
setkey(voterD, "user_id")
setkey(twitterD, "user_id")
setkey(annotD, "user_id")


# A. Profile fields: do they (still) contain the voter's first name, last name, state, city?
allProfileD = voterD[twitterD,]

# (forgot to take name field, so grab that back out of voter data)
source("util/panel_util.R")
panel = getPanelData(privateDataDir, withTrolls = F, withBots = F, useOrigBotScores = F) 
setkey(panel, user_id)
allProfileD = panel[, .(user_id, first_name, last_name)][allProfileD,]

# load full names (and nicknames) of states. Join them in before any of the greps.
statesFile = "panel_accounts/profile_validation/data/statesTable.txt"
stateAbbrevs = read.table(statesFile, sep="\t", as.is=T)
stateAbbrevs[,1] = tolower(stateAbbrevs[,1])
colnames(stateAbbrevs) = c("stateLong", "stateAbbrev")
stateAbbrevs = setDT(stateAbbrevs)
onePerState = stateAbbrevs[, .(longState = paste(stateLong, collapse="|")), by=.(stateAbbrev)]
setkey(onePerState, "stateAbbrev")
setkey(allProfileD, "tsmart_state")
allProfileD = onePerState[allProfileD,]


print(paste("Have Twitter profile data for", nrow(allProfileD), "panel members"))
hasFirstName = (mapply(grepl, allProfileD[, first_name], allProfileD[, name], ignore.case=T)) # not enforcing word boundaries
hasLastName = (mapply(grepl, allProfileD[, last_name], allProfileD[, name], ignore.case=T)) 
hasCity = (mapply(grepl, allProfileD[, tsmart_city], allProfileD[, location], ignore.case=T)) 
hasStateAbbrev = (mapply(grepl, paste0("\\b", allProfileD[, stateAbbrev], "\\b"), allProfileD[, location], ignore.case=T)) # yes word boundary 
hasState = (mapply(grepl, paste0("\\b(", onePerState[allProfileD,longState], ")\\b"), onePerState[allProfileD, location], ignore.case=T))

print(paste("In profile data, num where name field contains voter's first name:", sum(hasFirstName), "or", sum(hasFirstName)/nrow(allProfileD)))
print(paste("Num where name field contains voter's last name:", sum(hasLastName), "or", sum(hasLastName)/nrow(allProfileD)))
print(paste("Num with both first & last:", sum(hasFirstName & hasLastName), "or", sum(hasFirstName & hasLastName)/nrow(allProfileD)))
print(paste("Num where location contains voter's city:", sum(hasCity), "or", sum(hasCity)/nrow(allProfileD)))
print(paste("Num where location contains voter's state abbrev:", sum(hasStateAbbrev), "or", sum(hasStateAbbrev)/nrow(allProfileD)))
print(paste("Num where location contains voter's state:", sum(hasState), "or", sum(hasState)/nrow(allProfileD)))

print("putting that together...")
print(paste("Num with first and/OR last name:", sum(hasFirstName | hasLastName), "or", sum(hasFirstName | hasLastName)/nrow(allProfileD)))
print(paste("Num with city, state and/OR state abbrev:", sum(hasCity | hasStateAbbrev | hasState), "or", sum(hasCity | hasStateAbbrev | hasState)/nrow(allProfileD)))

# save as columns for finding outliers later
allProfileD[, matchesFirstName := hasFirstName]
allProfileD[, matchesLastName := hasLastName]
allProfileD[, matchesState := hasState | hasStateAbbrev]
allProfileD[, matchesCity := hasCity]
setkey(allProfileD, "user_id")



# B. Human-coded labels (age, gender, ethnicity): how close are they to each other and to the presumed match?
allD = allProfileD[annotD,]  # all that we have annotations for. (Keep extra columns computed in part A.)

# How often did annotators agree on whether to look at the photo?
allD[, hasPerson1 := as.logical(chartr("YN", "TF", hasPerson1))]
allD[, hasPerson2 := as.logical(chartr("YN", "TF", hasPerson2))]
print(paste("We annotated", nrow(allD), "profiles"))
print(paste("Both annotators agreed there was a valid photo in", nrow(allD[hasPerson1 & hasPerson2,]), "cases and there wasn't in", nrow(allD[!hasPerson1 & !hasPerson2,]), "cases"))
print(paste("(They disagreed in", nrow(allD[!(hasPerson1 & hasPerson2) & (hasPerson1 | hasPerson2),]), "cases, which we ignore)"))

# Now, subset on those where both looked.
allD = allD[hasPerson1 & hasPerson2,]
print(paste("Among those", nrow(allD), "annotated profiles..."))

# Gender. 
# Compare annotators: how far are we between chance agreement and perfect agreement?
allD[, sex := substr(sex, 1, 1)]
print(paste("Gender: both annotators agreed", nrow(allD[gender1 == gender2]), "times."))
# --> ah, good, annotators agreed 100% of the time. 

# Gender: Annotators vs. voter data 
# [where we are so far: we have two annotators for 131 profiles. they agree on sex, but that differs from the voter data 4 of the 131 times.]
allD[, matchesSex := gender1 == sex]


# Age.
# compare annotators
# Krippendorff's alpha is a generalization of several other methods. It handles arbitrary assignments of coders to items. also, both categorical & continuous data.
# (though I'm not bothering to show exactly which two annotators coded each item. Looks like it shouldn't affect the stat.)
print(paste("Age: Krippendorff's alpha between annotators"))
print(kripp.alpha(t(as.matrix(allD[, .(age1, age2)])), method="ratio"))  # gives only .74. (using "interval" gives .79, since it uses squared loss, but I don't know that that's warranted here.)


# annotator mean vs. voter data
allD[, averageAge := apply(allD[, .(age1, age2)], 1, mean)]
print(paste("Age: annotator avg vs. voter data. Pearson correlation is:"))
print(cor(allD[, .(averageAge, age_start_2018)]))
print(paste("Age: annotator avg vs. voter data. Krippendorff's alpha:"))
print(kripp.alpha(t(as.matrix(allD[, .(averageAge, age_start_2018)])), method="ratio"))

# Are there outliers? Plot:
 pdf("img/age_vs_annotation.pdf")
 plot(allD[, age_start_2018], allD[,averageAge], xlab="Voter age at start of 2018", ylab="Estimated age of person in Twitter profile photo")
 lines(allD[, age_start_2018], allD[, age_start_2018], type='l')		# the line x = y for reference.
 dev.off()
# What's to see? Annotators usually underestimate age. Which is both (a) polite and (b) consistent with photos showing the person at <= their current age (never older!).

print(paste("Here's the distribution of the diffs between annotation and truth:"))
print(summary((allD[, age_start_2018] - allD[,averageAge])))
print(paste("Number of times we were off by > 10 years:", sum(abs(allD[, age_start_2018] - allD[,averageAge]) > 10), "; by > 15 years:", sum(abs(allD[, age_start_2018] - allD[,averageAge]) > 15),
	"; by > 20 years:", sum(abs(allD[, age_start_2018] - allD[,averageAge]) > 20)))
print(paste("or as fractions: we were off by <= 5 years:", sum(abs(allD[, age_start_2018] - allD[,averageAge]) <= 5) / nrow(allD), 
	"; by <= 10 years:", sum(abs(allD[, age_start_2018] - allD[,averageAge]) <= 10) / nrow(allD), 
	"; by <= 15 years:", sum(abs(allD[, age_start_2018] - allD[,averageAge]) <= 15) / nrow(allD),
    "; by <= 20 years:", sum(abs(allD[, age_start_2018] - allD[,averageAge]) <= 20) / nrow(allD)))
allDyoung = allD[age_start_2018 <= 50,]
allDold = allD[age_start_2018 > 50,]
print(paste("People younger than 50 ==", nrow(allDyoung) / nrow(allD), "of the sample"))
print(paste("for people aged <=50: we were off by <= 5 years:", sum(abs(allDyoung[, age_start_2018] - allDyoung[,averageAge]) <= 5) / nrow(allDyoung),  
	"; by <= 10 years:", sum(abs(allDyoung[, age_start_2018] - allDyoung[,averageAge]) <= 10) / nrow(allDyoung), 
	"; by <= 15 years:", sum(abs(allDyoung[, age_start_2018] - allDyoung[,averageAge]) <= 15) / nrow(allDyoung),
    "; by <= 20 years:", sum(abs(allDyoung[, age_start_2018] - allDyoung[,averageAge]) <= 20) / nrow(allDyoung)))
print(paste("for people aged >50: we were off by <= 5 years:", sum(abs(allDold[, age_start_2018] - allDold[,averageAge]) <= 5) / nrow(allDold),  
	"; by <= 10 years:", sum(abs(allDold[, age_start_2018] - allDold[,averageAge]) <= 10) / nrow(allDold), 
	"; by <= 15 years:", sum(abs(allDold[, age_start_2018] - allDold[,averageAge]) <= 15) / nrow(allDold),
    "; by <= 20 years:", sum(abs(allDold[, age_start_2018] - allDold[,averageAge]) <= 20) / nrow(allDold)))



allD[, matchesAgeUpTo10 := abs(age_start_2018 - averageAge) <= 10]
matchColnames = c("matchesFirstName", "matchesLastName", "matchesState", "matchesCity", "matchesSex", "matchesAgeUpTo10")
allD[, numMatches := apply(allD[, ..matchColnames], 1, sum)]	 # (gives a warning message, but works)




# Ethnicity.
# Annotators vs. each other.
print(paste("Ethnicity: both annotators agreed", nrow(allD[ethnicity1 == ethnicity2,]), "times. Krippendorff's alpha is:"))
print(kripp.alpha(t(as.matrix(allD[, .(ethnicity1, ethnicity2)])), method="nominal"))
# ok to ignore warning message about NAs -- see https://stackoverflow.com/questions/21907540/irr-krippendorfs-alpha-with-non-numeric-classifications-warning-message
# Note: in the (11) disagreements, it's always one 'W', one something else.

# How to change annotator results into an agreed value? Let's take the 120 cases where they do agree.
agreedEth = allD[ethnicity1 == ethnicity2,]
print(paste("Ethnicity: looking only at the", nrow(agreedEth), "cases where annotators agree, compare their judgements to the voter data.",
			nrow(agreedEth[ethnicity1==ethnicity_code,]), "match. Krippendorff's alpha:"))
print(kripp.alpha(t(as.matrix(agreedEth[, .(ethnicity1, ethnicity_code)])), method="nominal"))


# Noticed: in no case do two data sources ever label the same person 'H'; the others always say 'W'. What if we just change all H --> W?
allD[, `:=`(newEth1 = ethnicity1, newEth2 = ethnicity2, newEth_code = ethnicity_code)]	# gives warning message but works
allD[ethnicity1 == 'H', newEth1 := 'W']	 
allD[ethnicity2 == 'H', newEth2 := 'W']
allD[ethnicity_code == 'H', newEth_code := 'W']
print("If we collapse the categories Hispanic and White... repeating the above")
print(paste("Ethnicity: both annotators agreed", nrow(allD[newEth1 == newEth2,]), "times. Krippendorff's alpha is:"))
print(kripp.alpha(t(as.matrix(allD[, .(newEth1, newEth2)])), method="nominal"))
table(allD[newEth1 == newEth2, newEth1])

agreedEth = allD[newEth1 == newEth2,]
print(paste("Ethnicity: looking only at the", nrow(agreedEth), "cases where annotators agree, compare their judgements to the voter data.",
			nrow(agreedEth[newEth1==newEth_code,]), "match. Krippendorff's alpha:"))
print(kripp.alpha(t(as.matrix(agreedEth[, .(newEth1, newEth_code)])), method="nominal"))

print(paste("plain fraction that match:", nrow(agreedEth[newEth1==newEth_code,]) / nrow(agreedEth)))



