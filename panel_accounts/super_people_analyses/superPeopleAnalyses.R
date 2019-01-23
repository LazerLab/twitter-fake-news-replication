# to run this code, just call generate_data_table_S3()

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, bit64, binom, Hmisc)  # binom for binom.confint, Hmisc for smean.cl.boot

source("util/panel_util.R")	# for getPanelData() and rankPeopleByX()
options("digits"=22)

generate_data_table_S3 = function() {
	set.seed(5)	# for replicability of bootstrapped CIs 
	prepDataAndDoAnalyses(dataDir="restricted_data", figsDir=NULL) 
}

prepDataAndDoAnalyses = function(dataDir, figsDir, justReturnGroups = F) {
  panel = readPanelAndAddCols(dataDir)
  setkey(panel, user_id)

 
  ## Prep super-people lists for fake news and overall
  # We already have columns listing who's super (for real and fake), but need the actual ranking tables. These tables cover those people only.
  overall_exp_by_person = rankPeopleByX(panel[is_superconsumer == T,], "total_exposures")  # is_superconsumer already defined top 1%
  overall_shares_by_person = rankPeopleByX(panel[is_supersharer == T,], "total_shares") # is_supersharer already defined top 1%

  fake_shares_by_person = rankPeopleByX(panel, "n_share", withCumPerc = T)
  fake_exp_by_person = rankPeopleByX(panel, "n_exp", withCumPerc = T)
  # restrict to those responsible for top 80% of fake content
  fake_shares_by_person = fake_shares_by_person[cumperc <= .8,]
  fake_exp_by_person = fake_exp_by_person[cumperc <= .8,]
  # note that fake super* aren't a strict subset of overall super*.
    
  ## Define groups
  superShareFakeA = fake_shares_by_person[, .(user_id)]
  superShareRealB = fsetdiff(overall_shares_by_person[, .(user_id)], superShareFakeA)
  superConsC = fsetdiff(overall_exp_by_person[, .(user_id)], funion(superShareFakeA, superShareRealB) )
  normalPeople = fsetdiff(panel[, .(user_id)], funion(overall_exp_by_person[, .(user_id)], funion(superShareFakeA, superShareRealB) ))
  
  if (justReturnGroups) {
	  return(list("ssFakeA"=superShareFakeA,
		      "ssRealB"=superShareRealB,
		      "superConsC" = superConsC,
		      "normal"=normalPeople))

  } else {
    summarizeGroupsAndCutoffs(overall_exp_by_person, overall_shares_by_person, fake_shares_by_person, fake_exp_by_person, 
                              superShareFakeA, superShareRealB, superConsC, normalPeople)
    summarizeCumPercsOfGroups(panel, superShareFakeA, superShareRealB, superConsC, normalPeople)
    
    
    # Send these variables in to the rest of the analysis
    doAnalyses(panel, overall_exp_by_person, overall_shares_by_person, fake_shares_by_person, fake_exp_by_person, 
               superShareFakeA, superShareRealB, superConsC, normalPeople, figsDir)
    checkIndivSrcStats(dataDir, panel, overall_exp_by_person, overall_shares_by_person, fake_shares_by_person, 
                       fake_exp_by_person, superShareFakeA, superShareRealB, superConsC, normalPeople, figsDir)
  }
  
}


readPanelAndAddCols = function(dataDir) {
  panel = getPanelData(dataDir, withTrolls = F, withBots = F, useOrigBotScores = F)
  setkey(panel, user_id)
  
  panel[, nonFake_shares := total_shares - n_share]
  panel[, nonFake_exp := total_exposures - n_exp]
  
  panel[, percPoliticalPosts := total_shares / tweetsDuringStudy]
  panel[, prop_exp_followees_alive := exp_followees_alive / orig_exp_followees]

  panel[, rts_prop := n_rts / total_shares]
  panel[, qts_prop := n_qts / total_shares]
  panel[, via_at_prop := n_via / total_shares]
  panel[, orig_via_at_prop := n_orig_via / total_shares]
  
  panel[, sex_male := (sex == "Male")]
  
  return(panel)
}

summarizeGroupsAndCutoffs = function(overall_exp_by_person, overall_shares_by_person, fake_shares_by_person, fake_exp_by_person, 
                          superShareFakeA, superShareRealB, superConsC, normalPeople) {
  print("Groups of people in this analysis")
  print(paste("top 1% of sharing overall:", nrow(overall_shares_by_person), "people, having at least", 
              tail(overall_shares_by_person, 1)$total_shares, "shares"))
  print(paste("top 1% of exposure overall:", nrow(overall_exp_by_person), "people, having at least", 
              tail(overall_exp_by_person, 1)$total_exp, "exposures"))
  
  print(paste("top people sharing fake news:", nrow(fake_shares_by_person), "people, having at least", 
              tail(fake_shares_by_person, 1)$n_share, "shares of fake news, account for",
              round(tail(fake_shares_by_person, 1)$cumperc, digits=3), "of the fake news"))
  print(paste("top people exposed to fake news:", nrow(fake_exp_by_person), "people, having at least", 
              tail(fake_exp_by_person, 1)$n_exp, "exposures to fake news, account for",
              round(tail(fake_exp_by_person, 1)$cumperc, digits=3), "of the fake news"))
  
  print(paste("superShareFakeA: in top 1% of sharing && top 80% of fake news sharing.", nrow(superShareFakeA), "people."))
  print(paste("superShareRealB: in top 1% of sharing BUT NOT in top 80% of fake news sharing.", nrow(superShareRealB), "people."))
  print(paste("superConsC: in top 1% of exposures BUT NOT in top 1% of sharing.", nrow(superConsC), "people."))
  print(paste("normalPeople: NOT in top 1% of exposures or sharing.", nrow(normalPeople), "people."))
  cat("\n")
}

summarizeCumPercsOfGroups = function(panel, superShareFakeA, superShareRealB, superConsC, normalPeople) {
  
  # amount of fake-news, news, fake exposures accounted for by each group
  # all shares: total_shares
  print("Amounts of content attributable to each group")
  print("total_shares:")
  print(paste("superShareFakeA:", round(
    sum(panel[superShareFakeA, total_shares]) / sum(panel[, total_shares]), 3)))
  print(paste("superShareRealB:", round(
    sum(panel[superShareRealB, total_shares]) / sum(panel[, total_shares]), 3)))
  print(paste("superConsC:", round(
    sum(panel[superConsC, total_shares]) / sum(panel[, total_shares]), 3))) 
  print(paste("normalPeople:", round(
    sum(panel[normalPeople, total_shares]) / sum(panel[, total_shares]), 3)))
  
  # fake shares
  print("fake shares:")
  print(paste("superShareFakeA:", round(
    sum(panel[superShareFakeA, n_share]) / sum(panel[, n_share]), 3)))
  print(paste("superShareRealB:", round(
    sum(panel[superShareRealB, n_share]) / sum(panel[, n_share]), 3)))
  print(paste("superConsC:", round(
    sum(panel[superConsC, n_share]) / sum(panel[, n_share]), 3))) 
  print(paste("normalPeople:", round(
    sum(panel[normalPeople, n_share]) / sum(panel[, n_share]), 3)))
  
  # total exposures
  print("total exposures:")
  print(paste("superShareFakeA:", round(
    sum(panel[superShareFakeA, total_exposures]) / sum(panel[, total_exposures]), 3)))
  print(paste("superShareRealB:", round(
    sum(panel[superShareRealB, total_exposures]) / sum(panel[, total_exposures]), 3)))
  print(paste("superConsC:", round(
    sum(panel[superConsC, total_exposures]) / sum(panel[, total_exposures]), 3))) 
  print(paste("normalPeople:", round(
    sum(panel[normalPeople, total_exposures]) / sum(panel[, total_exposures]), 3)))
  
  # fake exposures
  print("fake exposures:")
  print(paste("superShareFakeA:", round(
    sum(panel[superShareFakeA, n_exp]) / sum(panel[, n_exp]), 3)))
  print(paste("superShareFakeA:", round(
    sum(panel[superShareRealB, n_exp]) / sum(panel[, n_exp]), 3)))
  print(paste("superShareFakeA:", round(
    sum(panel[superConsC, n_exp]) / sum(panel[, n_exp]), 3)))
  print(paste("superShareFakeA:", round(
    sum(panel[normalPeople, n_exp]) / sum(panel[, n_exp]), 3)))
  
  
  cat("\n")
}


doAnalyses = function(panel, overall_exp_by_person, overall_shares_by_person, fake_shares_by_person, fake_exp_by_person, 
                      superShareFakeA, superShareRealB, superConsC, normalPeople, figsDir) {
  
  # check intersection of top 10 exp & sharing
  s2people = fintersect(overall_exp_by_person[1:10, .(user_id)], overall_shares_by_person[1:10, .(user_id)])
  print(paste("Intersection of the top-10 lists for overall SS and SC:", nrow(s2people), "people"))
  print(paste("Number of these in top-80% list for fake news sharing:", 
              nrow(fintersect(s2people, fake_shares_by_person[,.(user_id)]))))
  print(paste("Number of these in top-80% list for fake news exposure:", 
              nrow(fintersect(s2people, fake_exp_by_person[,.(user_id)]))))
  
  
  # tweetsDuringStudy
  # For this calc, actually want tweets per day during study
  # number of days in the sample: Aug 1 to Dec 6
  numDaysInStudy = 31 + 30 + 31 + 30 + 6    # = 128
  panel[, tweetsPerDayOfStudy := tweetsDuringStudy / numDaysInStudy]
  plotAndCalcForStat(panel, "tweetsPerDayOfStudy",  superShareFakeA, superShareRealB, superConsC, normalPeople, plotAsLog = T,
                     savePlotAsFile = file.path(figsDir, "tweetsPerDayOfStudy.pdf"))
  checkSigMeanBStrap(panel, "tweetsPerDayOfStudy", superShareFakeA, superShareRealB, superConsC, normalPeople, useLog = T)

  
  # pol_affl plot is clearer than summary stats: plots get more bimodal, with real-share on left, fake-share on right
  plotAndCalcForStat(panel, "pol_affl",  superShareFakeA, superShareRealB, superConsC, normalPeople,
                     savePlotAsFile = file.path(figsDir, "pol_affil.pdf"))
  checkSigMeanBStrap(panel, "pol_affl", superShareFakeA, superShareRealB, superConsC, normalPeople)
  
 
  # verified also as expected: means are fake-share (0) < normal (.005) << cons (.04) << real-share (.125)
  plotAndCalcForStat(panel, "verified",  superShareFakeA, superShareRealB, superConsC, normalPeople)
  # not significant, unsurprisingly
  checkSigBinomialBStrap(panel, "verified", superShareFakeA, superShareRealB, superConsC, normalPeople)
  
  plotAndCalcForStat(panel, "sex_male",  superShareFakeA, superShareRealB, superConsC, normalPeople)
  checkSigBinomialBStrap(panel, "sex_male", superShareFakeA, superShareRealB, superConsC, normalPeople)
  
  
  # followers_count: all three super-groups are higher than normal people. (typo in Table S3: missing asterisk & dagger for this stat)
  plotAndCalcForStat(panel, "followers_count",  superShareFakeA, superShareRealB, superConsC, normalPeople, plotAsLog = T,
                     savePlotAsFile = file.path(figsDir, "followers_count.pdf"))
  checkSigMeanBStrap(panel, "followers_count", superShareFakeA, superShareRealB, superConsC, normalPeople, useLog = T)
  
  # friends_count: all three super-groups are higher than normal people
  plotAndCalcForStat(panel, "friends_count",  superShareFakeA, superShareRealB, superConsC, normalPeople, plotAsLog = T,
                     savePlotAsFile = file.path(figsDir, "friends_count.pdf"))
  checkSigMeanBStrap(panel, "friends_count", superShareFakeA, superShareRealB, superConsC, normalPeople, useLog = T)
  

  # fake news sharing: medians same as in paper already: 0, 0, 3, 213
  plotAndCalcForStat(panel, "n_share",  superShareFakeA, superShareRealB, superConsC, normalPeople, plotAsLog = T,
                     savePlotAsFile = file.path(figsDir, "n_share.pdf"))
  checkSigMeanBStrap(panel, "n_share", superShareFakeA, superShareRealB, superConsC, normalPeople, useLog = T)
  # of course superShareFakeA is higher than everyone else -- it's what defines the group
  # also check if SS-R > SC: -> yes.
  pvalBVsC = t.test(log10(panel[superShareRealB, n_share]+1), log10(panel[superConsC, n_share]+1), alternative = "greater")$p.value
  print(paste("t-test of whether SS-R > SC for n_share: pval =", pvalBVsC))
  
  
  # fake news exposure: medians are 1 (normal) << 536 (real-share) << 3487 (cons) << 13000 (fake-share -- wow, higher than before)
  # (note: values in paper are multiplied by 10 to estimate the full firehose)
  plotAndCalcForStat(panel, "n_exp",  superShareFakeA, superShareRealB, superConsC, normalPeople, plotAsLog = T,
                     savePlotAsFile = file.path(figsDir, "n_exp.pdf"))
  checkSigMeanBStrap(panel, "n_exp", superShareFakeA, superShareRealB, superConsC, normalPeople, useLog = T)
  # all are significant

  # non-fake, measured as total minus fake. Ordered as expected, but no longer significant difference between SS-F and SS-R
  plotAndCalcForStat(panel, "nonFake_shares",  superShareFakeA, superShareRealB, superConsC, normalPeople, plotAsLog = T,
                     savePlotAsFile = file.path(figsDir, "nonFake_shares.pdf"))
  checkSigMeanBStrap(panel, "nonFake_shares", superShareFakeA, superShareRealB, superConsC, normalPeople, useLog = T)
  
  # non-fake exposure. Ordered with consumers > sharers, of course. (typo in Table S3: missing the dagger)
  plotAndCalcForStat(panel, "nonFake_exp",  superShareFakeA, superShareRealB, superConsC, normalPeople, plotAsLog = T,
                     savePlotAsFile = file.path(figsDir, "nonFake_exp.pdf"))
  checkSigMeanBStrap(panel, "nonFake_exp", superShareFakeA, superShareRealB, superConsC, normalPeople, useLog = T)

  
  # How many of their posts are political URLs? medians: normal (0) < scons (.017) < real-share (.090) < fake-share (.13)
  # (Only relevant for individuals who tweeted)
  plotAndCalcForStat(panel[tweetsDuringStudy > 0,], "percPoliticalPosts", merge(superShareFakeA, panel[tweetsDuringStudy > 0,], by="user_id"), 
                     merge(superShareRealB, panel[tweetsDuringStudy > 0,], by="user_id"), merge(superConsC, panel[tweetsDuringStudy > 0,], by="user_id"), 
                     merge(normalPeople, panel[tweetsDuringStudy > 0,], by="user_id"),
                     savePlotAsFile = file.path(figsDir, "percPoliticalPosts.pdf"))
  checkSigMeanBStrap(panel[tweetsDuringStudy > 0,], "percPoliticalPosts", merge(superShareFakeA, panel[tweetsDuringStudy > 0,], by="user_id"), 
                     merge(superShareRealB, panel[tweetsDuringStudy > 0,], by="user_id"), merge(superConsC, panel[tweetsDuringStudy > 0,], by="user_id"), 
                     merge(normalPeople, panel[tweetsDuringStudy > 0,], by="user_id"))
  
  # how many of your friends' accounts are still open? among friends we ever saw in the exposure data
  plotAndCalcForStat(panel, "prop_exp_followees_alive",  superShareFakeA, superShareRealB, superConsC, normalPeople, plotAsLog = F,
                     savePlotAsFile = file.path(figsDir, "prop_exp_followees_alive.pdf"))
  checkSigMeanBStrap(panel, "prop_exp_followees_alive", superShareFakeA, superShareRealB, superConsC, normalPeople)
  
  cat("\n") 

}

checkIndivSrcStats = function(baseDir, panel, overall_exp_by_person, overall_shares_by_person, fake_shares_by_person, fake_exp_by_person, 
                              superShareFakeA, superShareRealB, superConsC, normalPeople, figsDir) {
  # Note: these stats are only relevant for people who share at all
  panelWStats = panel[total_shares > 0,] 
  sc_whoShare = fintersect(superConsC, panelWStats[, .(user_id)])
  normal_whoShare = fintersect(normalPeople, panelWStats[, .(user_id)])
  print("The following analyses restricted to people who shared at least one political URL")
  print(paste("That's", nrow(sc_whoShare), "superconsumers, and", nrow(normal_whoShare), "normal people"))
  

  # RTs: SS-F > SS-R ~= SC ~= normal, but not significant
  plotAndCalcForStat(panelWStats, "rts_prop", superShareFakeA, superShareRealB, sc_whoShare, normal_whoShare,
                     savePlotAsFile = file.path(figsDir, "rts_prop.pdf"))
  checkSigMeanBStrap(panelWStats, "rts_prop", superShareFakeA, superShareRealB, superConsC, normalPeople)
  
  # quotes: SS-F < everyone else, but not significant
  plotAndCalcForStat(panelWStats, "qts_prop", superShareFakeA, superShareRealB, sc_whoShare, normal_whoShare,
                     savePlotAsFile = file.path(figsDir, "qts_prop.pdf"))
  checkSigMeanBStrap(panelWStats, "qts_prop", superShareFakeA, superShareRealB, superConsC, normalPeople)
  
  plotAndCalcForStat(panelWStats, "via_at_prop", superShareFakeA, superShareRealB, sc_whoShare, normal_whoShare,
                     savePlotAsFile = file.path(figsDir, "via_at_prop.pdf"))
  checkSigMeanBStrap(panelWStats, "via_at_prop", superShareFakeA, superShareRealB, superConsC, normalPeople)
  
   # also check "via" in a binary form. 
  panelWStats[, has_via := n_via > 0]
  plotAndCalcForStat(panelWStats, "has_via", superShareFakeA, superShareRealB, sc_whoShare, normal_whoShare,
                     savePlotAsFile = file.path(figsDir, "has_via.pdf"))
  checkSigBinomialBStrap(panelWStats, "has_via", superShareFakeA, superShareRealB, sc_whoShare, normal_whoShare)
  
  # Really, it'd be better to compute "via" on non-RTs. Repeating the above two questions counting only "via" seen in "orig" tweet.
  plotAndCalcForStat(panelWStats, "orig_via_at_prop", superShareFakeA, superShareRealB, sc_whoShare, normal_whoShare,
                     savePlotAsFile = file.path(figsDir, "orig_via_at_prop.pdf"))
  checkSigMeanBStrap(panelWStats, "orig_via_at_prop", superShareFakeA, superShareRealB, superConsC, normalPeople)
  
   # also check "via" in a binary form. 
  panelWStats[, has_orig_via := n_orig_via > 0]
  plotAndCalcForStat(panelWStats, "has_orig_via", superShareFakeA, superShareRealB, sc_whoShare, normal_whoShare,
                     savePlotAsFile = file.path(figsDir, "has_orig_via.pdf"))
  checkSigBinomialBStrap(panelWStats, "has_orig_via", superShareFakeA, superShareRealB, sc_whoShare, normal_whoShare)
  
  
} 

plotAndCalcForStat = function(panel, statName, superShareFakeA, superShareRealB, superConsC, normalPeople, plotAsLog = F, 
                              savePlotAsFile = NULL) {
  panelStat = panel[, .(user_id, stat = get(statName))]
  
  print(paste("Stat:", statName))
  # medians
  print(paste("Median among SShFake:", median(panelStat[superShareFakeA, stat])))
  print(paste("Median among SShReal:", median(panelStat[superShareRealB, stat])))
  print(paste("Median among SCons:", median(panelStat[superConsC, stat])))
  print(paste("Median among normalPeople:", median(panelStat[normalPeople, stat])))
  
  # means
  print(paste("mean among SShFake:", mean(panelStat[superShareFakeA, stat])))
  print(paste("mean among SShReal:", mean(panelStat[superShareRealB, stat])))
  print(paste("mean among SCons:", mean(panelStat[superConsC, stat])))
  print(paste("mean among normalPeople:", mean(panelStat[normalPeople, stat])))
  
  if (! is.numeric(panelStat$stat)) {
    return()
  }
  # distrs
  if (!is.null(savePlotAsFile)) {
      pdf(file = savePlotAsFile)
  }
  if (plotAsLog) {
    plot( density(log10(panelStat[superShareRealB, stat]+1)), xlim=range(log10(panelStat[, stat]+1)), xlab = paste("log(",statName, "+1)"), 
          col="green", main="Supersharers: fake (red), real (green);\nsuperconsumers (blue)")
    lines(density(log10(panelStat[superShareFakeA, stat]+1)), col="red")
    lines(density(log10(panelStat[normalPeople, stat]+1)), col="black")
    lines(density(log10(panelStat[superConsC, stat]+1)), col="cornflowerblue")
    
  } else {
    plot( density(panelStat[superShareRealB, stat]), xlim=range(panelStat[, stat]), xlab = statName, col="green",
          main="Supersharers: fake (red), real (green);\nsuperconsumers (blue)")
    lines(density(panelStat[superShareFakeA, stat]), col="red")
    lines(density(panelStat[normalPeople, stat]), col="black")
    lines(density(panelStat[superConsC, stat]), col="cornflowerblue")
  }    
  if (!is.null(savePlotAsFile)) {
    dev.off()
  }
  return()
  
}


# Check whether the 95% confidence interval for superShareFakeA overlaps with that of other groups.
# Using "Wilson" instead of "Agresti-Coull" method because we have small groups and the latter can give answers outside of [0,1].
checkSigBinomialBStrap = function(panel, statName, superShareFakeA, superShareRealB, superConsC, normalPeople) { 
  method = "wilson" # or "ac"
  #method = "ac"  # (just to test/compare)
  panelStat = panel[, .(user_id, stat = get(statName))]

  SSA_ci = binom.confint(x = sum(panelStat[superShareFakeA, stat]), n = nrow(superShareFakeA), methods=method)
  print(paste("CI for SS-F: [",  round(SSA_ci$lower, 4), ", ", round(SSA_ci$upper, 4), "]", sep="") )
  
  SSB_ci = binom.confint(x = sum(panelStat[superShareFakeA, stat]), n = nrow(superShareRealB), methods=method)
  print(paste("CI for SS-R: [",  round(SSB_ci$lower, 4), ", ", round(SSB_ci$upper, 4), "]", sep="") )
  
  SC_ci = binom.confint(x = sum(panelStat[superConsC, stat]), n = nrow(superConsC), methods=method)
  print(paste("CI for SC: [",  round(SC_ci$lower, 4), ", ", round(SC_ci$upper, 4), "]", sep="") )
  
  normal_ci = binom.confint(x = sum(panelStat[normalPeople, stat]), n = nrow(normalPeople), methods=method)
  print(paste("CI for normal people: [",  round(normal_ci$lower, 4), ", ", round(normal_ci$upper, 4), "]", sep="") )
  
  # Does A overlap with B? etc.
  if (!doesCIoverlap(SSA_ci, SSB_ci)) {
    print(paste("SS-F vs. SS-R: 95% confidence intervals are disjoint"))
  }
  if (!doesCIoverlap(SSA_ci, SC_ci)) {
    print(paste("SS-F vs. SC: 95% confidence intervals are disjoint"))
  }
  if (!doesCIoverlap(SSA_ci, normal_ci)) {
    print(paste("SS-F vs. normal people: 95% confidence intervals are disjoint"))
  }
  
}

# Significance test: are superShareFakeA stats (one per person) higher than [each other group, in turn]?
# Replaces calls to checkSigWithTTests. Uses bootstrap (1000 samples) to estimate 95% CI on the mean.
# The flag useLog is copied over from the TTest version of this, but disabled, since it shouldn't (and doesn't) 
# affect the results of bootstrapping & CI overlap.
checkSigMeanBStrap = function(panel, statName, superShareFakeA, superShareRealB, superConsC, normalPeople, useLog = F) {

  panelStat = panel[, .(user_id, stat = get(statName))]
  # if (useLog) {
  #   panelStat[, stat := log10(stat + 1)]
  #   print("(running sig test on logged var)")
  # }
  SSA_ci = as.data.table(t(smean.cl.boot(panelStat[superShareFakeA, stat])))
  print(paste("CI for SS-F: [",  round(SSA_ci$Lower, 4), ", ", round(SSA_ci$Upper, 4), "]", sep="") )
  SSB_ci = as.data.table(t(smean.cl.boot(panelStat[superShareRealB, stat])))
  print(paste("CI for SS-R: [",  round(SSB_ci$Lower, 4), ", ", round(SSB_ci$Upper, 4), "]", sep="") )
  SC_ci = as.data.table(t(smean.cl.boot(panelStat[superConsC, stat])))
  print(paste("CI for SC: [",  round(SC_ci$Lower, 4), ", ", round(SC_ci$Upper, 4), "]", sep="") )
  normal_ci = as.data.table(t(smean.cl.boot(panelStat[normalPeople, stat])))
  print(paste("CI for normal people: [",  round(normal_ci$Lower, 4), ", ", round(normal_ci$Upper, 4), "]", sep="") )
  
  # Does A overlap with B? etc.
  if (!doesCIoverlap(SSA_ci, SSB_ci)) {
    print(paste("SS-F != SS-R: 95% confidence intervals are disjoint"))
  }
  if (!doesCIoverlap(SSA_ci, SC_ci)) {
    print(paste("SS-F != SC: 95% confidence intervals are disjoint"))
  }
  if (!doesCIoverlap(SSA_ci, normal_ci)) {
    print(paste("SS-F != normal people: 95% confidence intervals are disjoint"))
  }
}

# two intervals overlap if either endpoint of #1 falls within the endpoints of #2 OR vice versa
doesCIoverlap = function(ci1, ci2) {
  # make names lowercase
  names(ci1) = tolower(names(ci1))
  names(ci2) = tolower(names(ci2))
  
  if ((ci1$lower >= ci2$lower && ci1$lower <= ci2$upper) || 
      (ci1$upper >= ci2$lower && ci1$upper <= ci2$upper) ||
      (ci2$lower >= ci1$lower && ci2$lower <= ci1$upper) || 
      (ci2$upper >= ci1$lower && ci2$upper <= ci1$upper)) {
    return(T)
  } else {
    return(F)
  }
  
}
