#### loads content shared by panel members and their friends on Twitter to compute potential exposures
# results in the following global variables:
#  - urls: each row represents a url shared by panel members or the accounts they follow.
#  - urls_panel: urls shared by panel members.
#  - urls_exp: potential exposures = all urls panel members could have seen in their feeds.
#  - friendships: lists panel members and the accounts they follow.
#  - panel: information about panel members
# Expected to be run from the repo main directory.

library(data.table)

freeze <- ls() # keep a list of all objects created until this is loaded

### load urls
urls <- fread("gunzip -c ../restricted_data/urls.tsv.gz", showProgress = F)
# parse dates
ts_lt <- as.POSIXlt(urls$tweet_date, format="%a %b %d %H:%M:%S %z %Y")
ts_ct <- as.POSIXct(ts_lt)
urls <- urls[, ts_tzoffset:=ts_lt$gmtoff / 3600]
urls <- urls[, ts_month:=as.numeric(format(ts_lt, format="%m"))]
elect_day <- as.POSIXct("Tue Nov 08 00:00:00 -0500 2016", format="%a %b %d %H:%M:%S %z %Y")
urls <- urls[, ts_rel_days:=floor(as.numeric(ts_ct-elect_day, "days"))]
urls <- urls[, ts_rel_weeks:=floor(as.numeric(ts_ct-elect_day, "weeks"))]
rm(ts_lt); rm(ts_ct)

### load friendships
friendships <- fread("gunzip -c ../restricted_data/friendships.tsv.gz", showProgress = F)

### load panel
panel <- fread("../restricted_data/panel.tsv", showProgress = F)

### derive urls_panel
urls_panel <- urls[is_panel_member==T,]
urls_panel <- merge(x=urls_panel, y=panel, by = "user_id")

### compute exposures
urls_exp <- merge(x=urls[,.(tweet_id, user_id, website, canonical_url, ts, ts_rel_days, ts_rel_weeks, 
                            domain_color, domain_color_num, how_sampled, is_pol_news_site, website_cat,
                            where_url_found)],
                  y=friendships[,.(panel_uid, friend_uid)], 
                  by.x = "user_id", by.y = "friend_uid", allow.cartesian = T) 
urls_exp <- merge(x=urls_exp, y=panel, by.x = "panel_uid", by.y = "user_id", all.x = T)

# calculate exposure and sharing variables and merge to other vars
exp_vars <- urls_exp[,.(n_pol_exp=.N,
                        n_fn_exp=sum(domain_color_num<4),
                        n_fn_exp_last_month=sum(domain_color_num<4 & ts_rel_days>=(-32) & ts_rel_days<=6)), # same period as Guess et al.
                     by=panel_uid]
share_vars <- urls_panel[,.(n_pol_shares=.N,
                            n_fn_shares=sum(domain_color_num<4)), 
                         by=user_id]
pol_exp_share <- merge(x=exp_vars, y=share_vars, by.x="panel_uid", by.y="user_id",all=T)
for (j in c("n_pol_exp", "n_fn_exp", "n_fn_exp_last_month", "n_pol_shares", "n_fn_shares"))
  set(pol_exp_share, which(is.na(pol_exp_share[[j]])),j,0)
urls <- merge(x=urls, y=pol_exp_share, by.x = "user_id", by.y = "panel_uid", all.x = T)
urls_panel <- merge(x=urls_panel, y=pol_exp_share, by.x = "user_id", by.y = "panel_uid", all.x = T)
urls_exp <- merge(x=urls_exp, y=pol_exp_share, by = "panel_uid", all.x = T)
panel <- merge(panel, pol_exp_share, by.x="user_id", by.y="panel_uid", all.x=T)

### panel preperation
panel <- panel[,`:=`(
  party                  = factor(party)
  , gender                 = factor(sex)
  , race_ethnicity         = factor(race_ethnicity, levels = c("Caucasian", "African-American", "Hispanic", "Other"))
  , is_nonwhite            = factor(race_ethnicity != "Caucasian")
  , is_swingstate          = factor(swingstate_factor)
  , n_friends_log10        = log10(1+friends_count)
  , n_followers_log10      = log10(1+followers_count)
  , fol_per_friend_log10   = log10((followers_count+1)/(friends_count+1))
  , exp_rate                          = n_fn_exp/n_pol_exp
  , n_pol_exp_exc_fn_log10            = log10(1+10*(n_pol_exp-n_fn_exp))
  , n_pol_exp_exc_fn_per_week_log10   = log10(1+10*(n_pol_exp-n_fn_exp)/18) # 18 weeks in the study
  , n_tweets_exc_study_log10          = log10(ntweets_total-n_pol_shares)
  , pol_affl_bucket        = cut(pol_affl, seq(-1, 1, length.out = 8)[c(-3, -6)], # == c(-1.0, -0.7142857, -0.1428571, 0.1428571, 0.7142857, 1.0) 
                                 labels = c("extreme left", "left", "center", "right", "extreme right"))
)]
panel <- panel[, pol_affl_act:=as.character(pol_affl_bucket)][n_pol_exp<100, pol_affl_act:="apolitical"]
panel <- panel[is_bot==T, pol_affl_act:="bot"]
panel <- panel[is_superconsumer==T, pol_affl_act:="superconsumer"]
panel <- panel[is_supersharer==T, pol_affl_act:="supersharer"]
pol_act_l <- c("extreme left", "left", "center", "right", "extreme right", "apolitical", "bot", "superconsumer", "supersharer")
panel <- panel[, pol_affl_act:=factor(pol_affl_act, pol_act_l)]


rm(list = setdiff(ls(), c(freeze, "urls", "urls_panel", "urls_exp", "friendships", 
                          "panel"))) # delete all unnecessary objects 
