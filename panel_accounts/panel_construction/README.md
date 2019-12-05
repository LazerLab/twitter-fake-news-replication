# Replication package for Section S.1, "Linking Voting Records to Twitter Accounts"

##Data
* The directory `wordLists` contains geographic terms used for extracting locations from Twitter profiles.
* In `sample-data`, the files `01-raw-profiles.txt` and `10-voter-data.txt` demonstrate the input format the code expects. The actual voter records and Twitter profiles are not public, and the records in these example files are not real.
* The remaining files in `sample-data` are outputs of the code.

##Code
* The files `01-processLines1.R` and `02-processNames2.R` preprocess the Twitter profiles by extracting names. Their output is `sample-data/03-profiles-name-words.txt`.
* The Twitter profiles are then loaded into a MySQL database. The SQL commands in `03-loadProfilesToDB.sql` are meant to be run from within that database. In addition, the database connection details need to be filled in near the bottom of `matchPeople3.R`.
* `04-wholeMatchingProcess.R` queries the voter records against the Twitter profiles. 
    * It saves candidate matches in `sample-data/11-candidate-matches.txt`,
    * extracts locations from the candidate matches as `sample-data/12-cand-matches-locs.txt`,
    * and finally determines if any candidates are matchable. (The sample data yields 0 matches.)
