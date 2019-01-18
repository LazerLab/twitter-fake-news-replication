# capping simulation study
# to be run from the twitter-fake-news base directory

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, Hmisc)

ntrials = 200   # CIs are plenty small this way
source("util/load_exp_data.R", chdir=T)		# loads urls_panel as one of the vars
urls_panel = urls_panel[is_bot==F & is_compromised==F,]
urls_panel[, is_fn := domain_color %in% c("Black", "Red", "Orange")]
# the relevant columns are: user_id, ts_rel_days, and is_fn. Each row is a URL share.

# t1 = urls_panel[, .(dailyCnt = .N), by=.(user_id, ts_rel_days)]
# table(t1$dailyCnt) (and summary()) shows that median is 1, [mean is 3.172,] and max is 235.

origSums = c(tot = nrow(urls_panel), totFake = sum(urls_panel[, is_fn]), totNonFake = sum(1 - urls_panel[, is_fn]))
origSums_subdiv = c(tot_ss = nrow(urls_panel[is_supersharer==T]), totFake_ss = sum(urls_panel[is_supersharer==T, is_fn]), totNonFake_ss = sum(1 - urls_panel[is_supersharer==T, is_fn]),
					tot_reg = nrow(urls_panel[is_supersharer==F]), totFake_reg = sum(urls_panel[is_supersharer==F, is_fn]), totNonFake_reg = sum(1 - urls_panel[is_supersharer==F, is_fn]))

cap_exp = c(1:6, 8, 10, 15, 20, 30, 40, 50, 70, 100)
allRes = data.table(matrix(0, nrow=length(cap_exp), ncol=13, dimnames=list(c(), 
							c("cap", "totFake_ss", "totFake_minCI_ss", "totFake_maxCI_ss", "totNon_ss", "totNon_minCI_ss", "totNon_maxCI_ss",
								"totFake_reg", "totFake_minCI_reg", "totFake_maxCI_reg", "totNon_reg", "totNon_minCI_reg", "totNon_maxCI_reg"))))
cnt = 1
for (cap_num in cap_exp) {
    print(paste("cap:", cap_num))

    # at each setting, run ntrials times with random orderings. Store mean and 95% CIs of the fake & non-fake fraction remaining.
    cappedSums = matrix(0, nrow=ntrials, ncol=7)
    for (t in 1:ntrials) {
        if (t %% 100 == 0) {
            print(paste("\ttrial", t))
        }
        urls_rand = urls_panel[sample(nrow(urls_panel)), .(user_id, is_supersharer, ts_rel_days, is_fn)]     # random order
        capped = urls_rand[, .(dailyCnt = min(.N, cap_num), dailyCntFake = sum(head(.SD$is_fn, cap_num))), by=.(user_id, is_supersharer, ts_rel_days)]
        capped[, dailyCntNonFake := dailyCnt - dailyCntFake]
        cappedSums[t,] = c(colSums(capped[, .(dailyCnt, dailyCntFake, dailyCntNonFake)]), colSums(capped[is_supersharer==T, .(dailyCntFake, dailyCntNonFake)]),
							colSums(capped[is_supersharer==F, .(dailyCntFake, dailyCntNonFake)]))
		# future: also keep track of how many people in each category hit the quota
    
    }

    bstr_fake_ss = as.data.table(t(smean.cl.boot(cappedSums[,4])))  # (data.table so we can use the $ operator)
    bstr_non_ss = as.data.table(t(smean.cl.boot(cappedSums[,5])))
    bstr_fake_reg = as.data.table(t(smean.cl.boot(cappedSums[,6])))
    bstr_non_reg = as.data.table(t(smean.cl.boot(cappedSums[,7])))
    allRes[cnt,] = data.table(t(c(cap=cap_num, 
									totFake_ss = bstr_fake_ss$Mean, totFake_minCI_ss = bstr_fake_ss$Lower, totFake_maxCI_ss = bstr_fake_ss$Upper,
                                    totNon_ss = bstr_non_ss$Mean, totNon_minCI_ss = bstr_non_ss$Lower, totNon_maxCI_ss = bstr_non_ss$Upper,
									totFake_reg = bstr_fake_reg$Mean, totFake_minCI_reg = bstr_fake_reg$Lower, totFake_maxCI_reg = bstr_fake_reg$Upper,
                                    totNon_reg = bstr_non_reg$Mean, totNon_minCI_reg = bstr_non_reg$Lower, totNon_maxCI_reg = bstr_non_reg$Upper)))
    cnt = cnt + 1
}
# fractions preserved
allRes[, `:=`(fracFake_ss=totFake_ss/origSums_subdiv[['totFake_ss']], fake_minCI_ss=totFake_minCI_ss/origSums_subdiv[['totFake_ss']], fake_maxCI_ss=totFake_maxCI_ss/origSums_subdiv[['totFake_ss']])]
allRes[, `:=`(fracFake_reg=totFake_reg/origSums_subdiv[['totFake_reg']], fake_minCI_reg=totFake_minCI_reg/origSums_subdiv[['totFake_reg']], fake_maxCI_reg=totFake_maxCI_reg/origSums_subdiv[['totFake_reg']])]
allRes[, `:=`(fracNon_ss=totNon_ss/origSums_subdiv[['totNonFake_ss']], non_minCI_ss=totNon_minCI_ss/origSums_subdiv[['totNonFake_ss']], non_maxCI_ss=totNon_maxCI_ss/origSums_subdiv[['totNonFake_ss']])]
allRes[, `:=`(fracNon_reg=totNon_reg/origSums_subdiv[['totNonFake_reg']], non_minCI_reg=totNon_minCI_reg/origSums_subdiv[['totNonFake_reg']], non_maxCI_reg=totNon_maxCI_reg/origSums_subdiv[['totNonFake_reg']])]
write.csv(allRes, file="analyses/capping_simulation/capping_preserved2.csv", row.names=F)
save(origSums, origSums_subdiv, cap_exp, file="analyses/capping_simulation/origSums.Rdata")


