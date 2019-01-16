
# Script saves the 2 PDFs that make up Fig 2 into <top-level-dir>/img/.

# make sure all libraries we'll ask for are installed
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, bit64, ggplot2, extrafont, scales, grid, RColorBrewer)

source("util/plotting.R")				# for my_custom_theme
source("util/panel_util.R")				# for getPanelData
source("panel_accounts/super_people_analyses/superPeopleAnalyses.R")	# for prepDataAndDoAnalyses

prep_panel = function(multiplyExpBy10 = T) {
  panel = getPanelData(dataDir="private_data", F, F)
  if (multiplyExpBy10) {
    expColNames = c("total_exposures", "n_exp", "n_black_exp", "n_red_exp", "n_orange_exp")
    for (colN in expColNames) {
      panel[, (colN) := 10 * get(colN)]
    }
  }
  
  panel[,n_nonfake_shares := total_shares - n_share]
  panel[,n_nonfake_exp := total_exposures - n_exp]
  panel[,pol_affl_bucket := cut(pol_affl, 
                                c(-1.0, -0.7142857, -0.1428571, 0.1428571, 0.7142857, 1.0),
                                labels = c("extreme left", "left", "center", "right", "extreme right"))]
  
  # merge in superspreader labels
  d <- prepDataAndDoAnalyses(dataDir="private_data", figsDir=NULL, justReturnGroups = T)
  
  superlabels <- data.table(rbind(d[[1]],d[[2]],d[[3]],d[[4]]),
                   supergroup=c(rep.int("ssFake",nrow(d[[1]])),
                           rep.int("ssReal",nrow(d[[2]])),
                           rep.int("sConsume",nrow(d[[3]])),
                           rep.int("normal",nrow(d[[4]]))))
  panel = merge(panel, superlabels, by="user_id", all.x=T)
  
  return(panel)
}
  
# v: number of people to break out. 
# panel: data to use
# content_type: shares or exp
compute_bar_info <- function(v,panel,content_type,show_pol_affl=T, scale_factor = 1){
  v <- v + 1  # for the bar showing "everyone else"
  if(content_type == "shares"){
    panel <- panel[order(-total_shares)]
  } else{
    panel <- panel[order(-total_exposures)]
  }
  panel[, ind := 1:nrow(panel)]
  panel[, ind := ifelse(ind >= v,v,ind)]
  
  pk <- panel[, list(n_nonfake = sum(get(paste0("n_nonfake_",content_type))),
                     n_orange = sum(get(paste0("n_orange_",content_type))),
                     n_red = sum(get(paste0("n_red_",content_type))),
                     n_black = sum(get(paste0("n_black_",content_type)))), 
              by=.(ind,pol_affl_bucket,supergroup)]

  cap <- pk[ind < v,.(n_nonfake,n_orange,n_red,n_black)][,list(nf=sum(n_nonfake),
                                                               f=sum(n_orange+n_red+n_black))]
  tot <- pk[,.(n_nonfake,n_orange,n_red,n_black)][,list(nf=sum(n_nonfake),
                                                        f=sum(n_orange+n_red+n_black))]
  pk[ind == v, n_nonfake := 0]
  pk <- melt(pk, c("ind","pol_affl_bucket","supergroup"))
  pk$variable <- factor(pk$variable,levels=c("n_nonfake", "ssFake", "ssReal", "n_orange","n_red","n_black"))
  pk$pol_affl_bucket <- factor(pk$pol_affl_bucket,
                               levels=
                                 c("extreme left","left","center","right","extreme right"),
                               labels=c("L*","L","C","R","R*"))
  pk[variable == "n_nonfake" & (supergroup == "ssFake" | supergroup == "ssReal"), variable := supergroup]
  pk[, supergroup := NULL]
  pk[ind == v, pol_affl_bucket := NA]
  if(!show_pol_affl){
    pk$pol_affl_bucket <- NA
  }
  pk[, value := value / scale_factor]
  p <- ggplot(pk, aes(ind,value))
  p <- p + scale_color_manual(
    values=c('darkblue', '#2c7bb6', "black", '#d7191c','darkred'), 
    name = "Political affiliation",
    labels=c("Extreme Left","Left","Center","Right","Extreme Right")) 
  p <- p + scale_fill_manual("Content Type",values=c("n_nonfake"="grey", 
                                                     "ssFake" = "#ADDD8E",
                                                     "ssReal" = "grey", 
                                                     "n_orange"="orange",
                                                     "n_red"="red",
                                                     "n_black"="black"),
                             labels=c(rep("All other content",3),"Orange Fake News","Red Fake News","Black Fake News"))
  p <- p + theme(legend.position = "none")
  return(list("pk"=pk,"metrics" = cap/tot, "plt"=p))
  
}


panel = prep_panel()

theme_set(my_custom_theme())
numSS = sum(panel$is_supersharer)
bar_info <- compute_bar_info(numSS,panel,"shares")
p <- bar_info[['plt']] + geom_bar(aes(fill=variable), stat='identity',width=.8) 
p <- p + geom_text(data=bar_info[['pk']][,sum(value),by=.(ind,pol_affl_bucket)],
                   aes(ind,V1+100,label=pol_affl_bucket,color=pol_affl_bucket),size=5,hjust=.4) 
p <- p + scale_x_continuous(paste("Supersharers: Top",numSS, "by political URLs shared"),breaks=c())
p <- p + ylab("Shares of political URLs")
print(bar_info[['metrics']])
ggsave("img/bar.pdf",p,w=15,h=7,dpi=400)

numSC = sum(panel$is_superconsumer)
sf = 1000000
bar_info <- compute_bar_info(numSC,panel,"exp",show_pol_affl=F, scale_factor = sf)
p <- bar_info[['plt']] + geom_bar(aes(fill=variable), stat='identity',width=.8) 
p <- p + geom_text(data=bar_info[['pk']][,sum(value),by=.(ind,pol_affl_bucket)],
                   aes(ind,V1+(25000 / sf),label=pol_affl_bucket,color=pol_affl_bucket),size=5,hjust=.4) 
p <- p + scale_x_continuous(paste("Superconsumers: Top", numSC, "by exposures to political URLs"),breaks=c())
p <- p + ylab("Exposures to political URLs (millions)")
print(bar_info[['metrics']])
ggsave("img/bar_exp.pdf",p,w=15,h=7,dpi=400)




