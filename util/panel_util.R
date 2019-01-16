
library(data.table)

# Function does some reasoning about old versions.
getPanelData = function(dataDir, withTrolls, withBots, useOrigBotScores = F) {
  panel = fread(file.path(dataDir, "final_panel.csv")) 
  if (!withTrolls) {
    panel = panel[is_compromised == F,]
  }
  if (useOrigBotScores) {
    panel[, is_bot:=ifelse(bot_score>=0.7 & ntweets_total>=50, T, F)]
  }
  if (!withBots) {
    panel = panel[is_bot == F,]
  }
  return(panel)
}

# Input: panel-like data.table and column name
# Output: data.table showing (top) users ranked by colname
rankPeopleByX = function(panelSlice, colname, numPeopleWanted = NULL, withCumPerc = F) {
  rank_table = panelSlice[, c("user_id", "twProfileHandle", colname), with=F]
  rank_table = rank_table[order(-rank(get(colname))),]
  rank_table[, rank := .I]
  if (withCumPerc) {
    rank_table[, cumperc := cumsum(get(colname)) / sum(get(colname))]
  }
  if (is.null(numPeopleWanted)) {
    numPeopleWanted = nrow(rank_table)
  }
  return(rank_table[1:numPeopleWanted,])
}
