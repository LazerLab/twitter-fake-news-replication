prepDataAndDoAnalyses = function(dataDir, figsDir, justReturnGroups = F) {
  panel = getPanelData(dataDir, withTrolls = F, withBots = F, useOrigBotScores = F)
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
  
  if (!justReturnGroups) {
    summarizeGroupsAndCutoffs(overall_exp_by_person, overall_shares_by_person, fake_shares_by_person, fake_exp_by_person, 
                              superShareFakeA, superShareRealB, superConsC, normalPeople)
    summarizeCumPercsOfGroups(panel, superShareFakeA, superShareRealB, superConsC, normalPeople)
    
    
    # Send these variables in to the rest of the analysis
    doAnalyses(panel, overall_exp_by_person, overall_shares_by_person, fake_shares_by_person, fake_exp_by_person, 
               superShareFakeA, superShareRealB, superConsC, normalPeople, figsDir)
    checkIndivSrcStats(dataDir, panel, overall_exp_by_person, overall_shares_by_person, fake_shares_by_person, 
                       fake_exp_by_person, superShareFakeA, superShareRealB, superConsC, normalPeople, figsDir)
  }
  
  return(list("ssFakeA"=superShareFakeA,
              "ssRealB"=superShareRealB,
              "superConsC" = superConsC,
              "normal"=normalPeople))
}

