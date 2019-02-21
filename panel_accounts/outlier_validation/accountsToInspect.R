
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, bit64)

source("util/panel_util.R")

printAccountsToInspect = function(dataDir) {
  # First off, examined fake news superspreaders and top of fake news superconsumer list.
  print("First set manually inspected")
  printTopFakeAccounts(dataDir)
  
  # Then, slightly later, examined top superpeople ranked by overall (political) content.
  # At this point, we had already removed trolls and bots.
  # We also examined any accounts marked as bots that would have made the superpeople lists (either overall or for fake news).
  print("Second set manually inspected")
  printTopOverallAndTopBots(dataDir)

}

printTopFakeAccounts = function(dataDir, fixingBotsAndTrolls = F) {

  if (fixingBotsAndTrolls) {
    # up-to-date version 
    panel = getPanelData(dataDir, withTrolls = F, withBots = F, useOrigBotScores = F)
  } else {
    # older version. Note: we have to have included bots to see troll #1
    panel = getPanelData(dataDir, withTrolls = T, withBots = T, useOrigBotScores = T)
  }
  
  fake_shares_by_person = rankPeopleByX(panel, "n_fn_shares", withCumPerc = T)
  fake_exp_by_person = rankPeopleByX(panel, "n_fn_exp", withCumPerc = T)
  # restrict to those responsible for top 80% of fake content
  fake_shares_by_person = fake_shares_by_person[cumperc <= .8,]
  fake_exp_by_person = fake_exp_by_person[cumperc <= .8,]
  
  print(paste("Number of fake news superspreaders:", nrow(fake_shares_by_person)))
  print("Fake news superspreaders:")
  print(fake_shares_by_person)
  print(paste("Number of fake news superconsumers:", nrow(fake_exp_by_person)))
  print("Top 10 fake news superconsumers:")
  print(head(fake_exp_by_person, 10))
  
}

# How we produced the overall top-10 lists and the "bots" that would count as superconsumers
printTopOverallAndTopBots = function(dataDir) {
  
  panel = getPanelData(dataDir, withTrolls = F, withBots = F, useOrigBotScores = T)

  # We already have columns listing who's super (overall), but need the actual ranking tables. These tables cover those people only.
  overall_exp_by_person = rankPeopleByX(panel[is_superconsumer == T,], "n_pol_exp")  # is_superconsumer already defined top 1%
  overall_shares_by_person = rankPeopleByX(panel[is_supersharer == T,], "n_pol_shares") # is_supersharer already defined top 1%
  
  fake_shares_by_person = rankPeopleByX(panel, "n_fn_shares", withCumPerc = T)
  fake_exp_by_person = rankPeopleByX(panel, "n_fn_exp", withCumPerc = T)
  # restrict to those responsible for top 80% of fake content
  fake_shares_by_person = fake_shares_by_person[cumperc <= .8,]
  fake_exp_by_person = fake_exp_by_person[cumperc <= .8,]
  # note that fake super* aren't a strict subset of overall super*.
  
  # Print out lists for manual checking
  # 1. top 10 for exposures
  print("Top 10 overall superconsumers")
  print(head(overall_exp_by_person, 10))
  
  # 2. top 10 for sharing
  print("Top 10 overall superspreaders")
  print(head(overall_shares_by_person, 10))
  
  # Bots that are in any of the "top" lists
  # 3. Bots in top 1% for overall sharing or exposure
  panelWBots = getPanelData(dataDir, withTrolls = F, withBots = T, useOrigBotScores = T)
  print("Bots in top 1% for overall sharing or exposure")
  minForOvExp = tail(overall_exp_by_person, 1)$n_pol_exp
  minForOvSh = tail(overall_shares_by_person, 1)$n_pol_shares
  if ("twProfileHandle" %in% colnames(panelWBots)) {
	  print(panelWBots[is_bot==T & (n_pol_exp >= minForOvExp | n_pol_shares >= minForOvSh), .(user_id, twProfileHandle, n_pol_exp, n_pol_shares)][order(-n_pol_exp),])
  } else {
	  print(panelWBots[is_bot==T & (n_pol_exp >= minForOvExp | n_pol_shares >= minForOvSh), .(user_id, n_pol_exp, n_pol_shares)][order(-n_pol_exp),])
  }

  
  # 4. Bots in set responsible for 80% of fake shares and exposures
  print("Bots in set responsible for 80% of fake shares and exposures")
  minForFakeExp = tail(fake_exp_by_person, 1)$n_fn_exp  
  minForFakeSh = tail(fake_shares_by_person, 1)$n_fn_shares
  if ("twProfileHandle" %in% colnames(panelWBots)) {
	  print(panelWBots[is_bot==T & (n_fn_exp >= minForFakeExp | n_fn_shares >= minForFakeSh), .(user_id, twProfileHandle, n_fn_exp, n_fn_shares)][order(-n_fn_exp),])
  } else {
	  print(panelWBots[is_bot==T & (n_fn_exp >= minForFakeExp | n_fn_shares >= minForFakeSh), .(user_id, n_fn_exp, n_fn_shares)][order(-n_fn_exp),])
  }
  return()
}

# code is run here:
printAccountsToInspect("restricted_data")
