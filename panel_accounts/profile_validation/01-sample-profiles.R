if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, bit64)

source("util/panel_util.R")

privateDataDir = "private_data"
panel = getPanelData(dataDir=privateDataDir, withTrolls = F, withBots = F, useOrigBotScores = F)  # gets 16442 accts
rowsToKeep = sample(1:nrow(panel), size=500)
voterDemogs = panel[rowsToKeep, .(user_id, sex, birth_date, ethnicity_code, tsmart_state, tsmart_city)]
voterDemogs[, age_start_2018 := 2017 - as.numeric(substr(birth_date, 1, 4))]
fwrite(voterDemogs[, .(user_id, sex, age_start_2018, ethnicity_code, birth_date, tsmart_state, tsmart_city)], file=file.path(privateDataDir, "panel_sample.csv"))
