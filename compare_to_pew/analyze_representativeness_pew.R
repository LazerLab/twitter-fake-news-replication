library(survey)
library(binom)
library(plyr)
library(dplyr)
library(data.table)
library(ggplot2)
source("util/plotting.R")

######## Contained in restricted release***********
panel <- fread("restricted_data/panel.tsv")
######################################

pew_data <- fread("restricted_data/pew_stats.csv")
#Is Internet User
pew_data[ , is_internet_user := eminuse == 1 | intmob == 1]

# Is Twitter User
pew_data[, is_twitter_user := ifelse(is.na(act112),NA, 
                                     ifelse(act112 == 9, NA,
                                            ifelse(act112 == 1, "Yes","No")))]

#Party Score
pew_data[, party_score := party]
pew_data[party_score > 3, party_score := 4]
pew_data[, party_score := mapvalues(party_score,c(1,2,3,4),c("Republican","Democrat","Independent","No Party/No Info"))]

#Sex
pew_data[, sex := mapvalues(sex,c(1,2),c("Male","Female"))]

#Race
pew_data[,race_ethnicity := ifelse(hisp == 1, 1, ifelse(race3m1 <3, race3m1+1, 4))]
pew_data[, race_ethnicity := mapvalues(race_ethnicity, c(1,2,3,4), 
                                       c("Hispanic","Caucasian","African-American","Other"))]

# Is registered to vote
pew_data[, vote_registered := ifelse(reg < 2, 1, ifelse(reg == 3, 0, NA))]


v <- svydesign(ids=~1,data=pew_data,weights=~weight)

# how many people are internet users in their sample
svyciprop(~is_internet_user,v,method="logit")

# how many internet using adults use twitter?
svyciprop(~I(is_twitter_user=="Yes"),subset(v, !is.na(is_twitter_user)),method="logit")

# how many of those on Twitter are registered to vote, likely at the address they are located at?
svyciprop(~vote_registered,subset(v, !is.na(vote_registered) & is_twitter_user=="Yes"),method="logit")

# Okay, lets now check if our sample is representative of voters on Twitter

make_confint_table <- function(x){
  tab <- data.table(table(x))
  setnames(tab, c("name","N"))
  tab <- tab[N > 0]
  tab <- tab[,binom.confint(N,sum(tab$N),methods = "ac"),by=name][,.(name,mean,lower,upper)]
  tab$Data <- "Panel"
  return(tab)
}

gen_pew_table <- function(factor_var,data_subset){
  pew <- data.frame(svymean(as.formula(paste0("~",factor_var)),data_subset, method="logit"))
  setnames(pew, c("mean","SE"))
  pew$name <- sub(factor_var,"",rownames(pew))
  pew$upper <- pew$mean + 2*pew$SE
  pew$lower <- pew$mean - 2*pew$SE
  pew$Data <- "PEW"
  return(pew[,c("name","mean","lower","upper","Data")])
}

voters <- subset(v,!is.na(vote_registered) & vote_registered==1 & is_twitter_user == "Yes")

dim(voters)

# Party
dim(subset(voters, party_score != "No Party/No Info"))
pew_party <- gen_pew_table("party_score",subset(voters, party_score != "No Party/No Info"))
panel_party <- make_confint_table(panel[party %in% pew_party$name]$party)
party_dat <- rbind(panel_party,pew_party)

# Sex
pew_sex <- gen_pew_table("sex",voters)
panel_sex <- make_confint_table(panel[sex %in% pew_sex$name]$sex)
sex_dat <- rbind(pew_sex,panel_sex)

# Race/Ethnicity
pew_race <- gen_pew_table("race_ethnicity",voters)
panel_race <- make_confint_table(panel$race_ethnicity)
race_dat <- rbind(pew_race,panel_race)
race_dat[,name := mapvalues(name, c("Hispanic","Caucasian","African-American","Other"),
                          c("Hispanic","White","Black","Other"))]


dat <- rbind(race_dat[name != "Other"],party_dat,sex_dat)
dat$name <- factor(dat$name,levels=unique(dat$name))
plt <- ggplot(dat, aes(name,mean,ymin=lower,ymax=upper,color=Data)) 
plt <- plt + geom_pointrange(position=position_dodge(.5),size=1.) + coord_flip()
plt <- plt + scale_y_continuous("Estimated Percent",labels=percent,limits=c(0,1))
plt <- plt + my_custom_theme() +xlab("Demographic Characteristic")
ggsave(plt,file="compare_to_pew/img/pew_rep.pdf",h=6,w=7)

# Age
smean.cl.boot(panel$age)
svymean(~age,voters)
