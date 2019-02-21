
library(data.table)
library(bit64)

# Function does some reasoning about old versions.
getPanelData = function(dataDir, withTrolls, withBots, useOrigBotScores = F) {
  panel = fread(file.path(dataDir, "panel.tsv"))
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
	if ("twProfileHandle" %in% colnames(panelSlice)) {
	  rank_table = panelSlice[, c("user_id", "twProfileHandle", colname), with=F]
	} else {
	  rank_table = panelSlice[, c("user_id", colname), with=F]
	}
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

# NG: THIS IS NO LONGER NEEDED AS panel.tsv contains these pre-computed fields.
# documents where some of the variables in restricted_data/panel_with_counts.tsv come from
# addPanelVarsFromURLs = function() {
# 	# some of this is from load_exp_data.R
# 	urls <- fread("gunzip -c restricted_data/urls.tsv.gz")
# 	panel <- fread("restricted_data/panel.tsv")		# (todo: have panel.tsv contain extra vars currently in panel.LF.tsv)
# 	urls_panel <- urls[is_panel_member==T,]
# 	urls_panel <- merge(x=urls_panel, y=panel, by = "user_id")
# 	setkey(urls_panel, user_id)
# 
# 	print("computing totals for sharing")
# 	share_vars <- urls_panel[, .(total_shares=.N,
#                                  n_black_shares  = sum(domain_color=="Black", na.rm = T), 
#                                  n_red_shares    = sum(domain_color=="Red", na.rm = T),
#                                  n_orange_shares = sum(domain_color=="Orange", na.rm = T),
# 								 n_share = sum(domain_color_num<4),
# 								 n_rts=sum(where_url_found=="retweeted"),
# 								 n_qts=sum(where_url_found=="quoted")),
# 							 by=.(user_id)]
# 
# 	print("getting exposure data")
# 	friendships <- fread("gunzip -c restricted_data/friendships.tsv.gz", showProgress = F)
# 	# keeping old names total_exposures, n_exp, total_shares, n_share for compatibility 
# 	urls_exp <- merge(x=urls[,.(tweet_id, user_id, website, 
#                             domain_color, domain_color_num)],
# 					  y=friendships[,.(panel_uid, friend_uid)],
# 					  by.x = "user_id", by.y = "friend_uid", allow.cartesian = T)
# 	urls_exp <- merge(x=urls_exp, y=panel, by.x = "panel_uid", by.y = "user_id", all.x = T)
# 	exp_vars <- urls_exp[,.(total_exposures=.N, 
# 							n_black_exp = sum(domain_color=="Black", na.rm = T), 
# 							n_red_exp   = sum(domain_color=="Red", na.rm = T),
# 							n_orange_exp= sum(domain_color=="Orange", na.rm = T),
# 							n_exp = sum(domain_color_num<4)),
# 						 by=.(panel_uid)]
# 
# 	pol_exp_share <- merge(x=exp_vars, y=share_vars, by.x="panel_uid", by.y="user_id",all=T)
# 
# 	new_panel_vars = fill_datatable_na_with_zero(pol_exp_share)
# 	new_panel = merge(panel, new_panel_vars, by.x="user_id", by.y="panel_uid", all.x=T)
# 	fwrite(new_panel, file="restricted_data/panel_with_counts.tsv", sep="\t")
# 
# 
# }

fill_datatable_na_with_zero <- function(DT, val = 0){
  for(j in seq_along(DT)){
    set(DT, i = which(is.na(DT[[j]]) & is.numeric(DT[[j]])), j = j, value = val)
  }
  return(DT)
}
