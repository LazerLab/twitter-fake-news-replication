# capping simulation plot
# to be run from the twitter-fake-news base directory

if (!require("pacman")) install.packages("pacman")
pacman::p_load(Hmisc)

allRes = read.csv("analyses/capping_simulation/capping_preserved2.csv")
load("analyses/capping_simulation/origSums.Rdata")	# restores origSums, origSums_subdiv and cap_exp

# error bars: CIs might be so narrow we don't need them.
max_ci_horiz_ss = max(allRes$fake_maxCI_ss - allRes$fake_minCI_ss)
max_ci_vert_ss = max(allRes$non_maxCI_ss - allRes$non_minCI_ss)
max_ci_horiz_reg = max(allRes$fake_maxCI_reg - allRes$fake_minCI_reg)
max_ci_vert_reg = max(allRes$non_maxCI_reg - allRes$non_minCI_reg)
print(paste("max CI:", max(max_ci_horiz_ss, max_ci_vert_ss, max_ci_horiz_reg, max_ci_vert_reg)))	# .0024


# labels for points 
alltext_labels = as.character(cap_exp)
alltext_x = 1 - (allRes$totFake_ss + allRes$totFake_reg)/origSums[['totFake']]
alltext_y = 1 - (allRes$totNon_ss + allRes$totNon_reg)/origSums[['totNonFake']]


pdf("img/cappingSim4.pdf")
# Show how supersharers vs regular accounts contribute to the fake & non-fake removed.
# show: overall non-fake removed (shared y axis), amount fake removed (x axis) for ss & reg accounts as separate lines
# Let the subdivisions have separate y axes. always looking at contribution to overall amounts.
# volume of non-fake removed due to ss = orig ss non-fake - preserved ss non-fake
plot(x=1 - (allRes$totFake_ss + allRes$totFake_reg)/origSums[['totFake']], y=1 - (allRes$totNon_ss + allRes$totNon_reg)/origSums[['totNonFake']], 
	type='o', pch=19, xlim=c(0,1), ylim=c(0,1), ylab="Non-fake news source URLs removed", xlab="Fake news source URLs removed")
lines(x= (origSums_subdiv[['totFake_ss']] - allRes$totFake_ss) / origSums[['totFake']],  y=(origSums_subdiv[['totNonFake_ss']] - allRes$totNon_ss)/origSums[['totNonFake']], lty=2, col="red", pch=19, type="o")
#lines(x=(origSums_subdiv[['totFake_reg']] - allRes$totFake_reg) / origSums[['totFake']],  y=(origSums_subdiv[['totNonFake_reg']] - allRes$totNon_reg)/origSums[['totNonFake']], lty=2, col="green", pch=19, type="o")
# Each line has, for the same cap, its (x,y) in a different place. But "overall" & "ss" lines are close enough to each other we can see the correspondence. The "reg" line is harder to match to -- unnecessarily complicated.

# connect the caps that are far apart (vector arithmetic: these grey lines = amounts contributed by non-supersharers)
segments(x0=(1 - (allRes$totFake_ss + allRes$totFake_reg)/origSums[['totFake']])[1:6], x1=((origSums_subdiv[['totFake_ss']] - allRes$totFake_ss) / origSums[['totFake']])[1:6],
	y0=(1 - (allRes$totNon_ss + allRes$totNon_reg)/origSums[['totNonFake']])[1:6], y1=((origSums_subdiv[['totNonFake_ss']] - allRes$totNon_ss)/origSums[['totNonFake']])[1:6], col="grey")

legend("topleft", lty=c(1, 2), col=c("black", "red"), legend=c("Total removed for each cap", "Amount coming from supersharers"))

# display all labels, treating 1 & 100 specially, and move low cap vals to upper left
whichtext = which(cap_exp <= 20 & cap_exp > 1)
text(x=alltext_x[whichtext], y=alltext_y[whichtext], labels=alltext_labels[whichtext], adj=c(1, -.5))
text(x=alltext_x[1], y=alltext_y[1], labels="cap=1", adj=c(1, -.5))
text(x=alltext_x[length(cap_exp)], y=alltext_y[length(cap_exp)], labels="100", pos=1)
whichtext = which(cap_exp > 20 & cap_exp < 100)
text(x=alltext_x[whichtext], y=alltext_y[whichtext], labels=alltext_labels[whichtext], adj=c(0, 1.5))
dev.off()


