library(Matrix)
library(igraph)
source("util/plotting.R")
setwd("util/domain_lists/")
source("load_all.R")
setwd("../../")


############## Which Sites are we using to build the networks ########################
yellow_sites <- c("addictinginfo.org", "awm.com", "blakkpepper.com", "breitbart.com", "cheezburger.com", 
                  "christiannews.net", "cosmopolitan.com", "dailykos.com", "dailymail.co.uk", 
                  "deadstate.org", "hellochristian.com", "hinterlandgazette.com", "hngn.com",
                  "lifenews.com", "lifesitenews.com", "metalsucks.net", "metro.co.uk", "nypost.com", 
                  "redstate.com", "standard.co.uk", "thefederalist.com", "theintellectualist.co",
                  "thesun.co.uk", "tmz.com")

# find all the political news/blog websites
nonfake_anno <- fread("util/domain_lists/nonfake_annotated_2.csv", showProgress = F)
sites <- c(yellow_sites,
           nonfake_anno[website_cat %in% c("news/blog")]$website,
           domains_colored[domain_color != "Satire"]$domain)
sites <- unique(sites)
# for plotting purposes
clean_sitenames <- fread("coexposure_network/data/site_map.tsv")

######################################################################################
######################################################################################
######################################################################################
#### ********** USE THE CODE BELOW IF YOU HAVE ACCESS TO THE RESTRICTED DATA******####
#### ********** Please also note that due to K-anonymization of the data, the results******####
#### ********** if you use this code will be slightly different - some sites in the final ******####
#### ********** network in the paper are removed.  We are retaining this code to ******####
#### ********** show the full construction of hte network, but please skip to ******####
#### ********** line 96 if you would like to exactly replicate the paper using ******####
#### ********** aggregate (full) data ******####
######################################################################################
######################################################################################
######################################################################################

source("util/load_exp_data.R", chdir = T)
# subset exposures to this set of websites
exposure_sites <- urls_exp[website %in% sites]
exposure_sites[, website := mapvalues(website,clean_sitenames$from,clean_sitenames$to)]

########################################################################################################
#######Construct the co-exposure network network##########
########################################################################################################

# how many sites are actually in the data?
length(unique(exposure_sites$website))

# how many of all exposures are we capturing?
nrow(exposure_sites)/nrow(urls_exp[!website %in% c("twitter.com","amp.twimg.com")])

# N exposures of each website to each panel member
uid_domain_net <- exposure_sites[,.N,by=.(panel_uid,website)]

# Raw data - N exposures of each website to each panel member
uid_to_consider <- unique(uid_domain_net$panel_uid)

# user counts per website
user_counts_per_site <- uid_domain_net[,.N,by=website]

###
# Construct a site x site matrix of shared users, with raw counts as well
###
site_df <- data.table(website=unique(uid_domain_net$website),site_idv=1:length(unique(uid_domain_net$website)))
user_df <- data.table(panel_uid=uid_to_consider,user_idv = 1:length(uid_to_consider))

uid_domain_net <- merge(uid_domain_net,site_df,by="website")
uid_domain_net <- merge(uid_domain_net,user_df,by="panel_uid")

mat <- sparseMatrix(i = uid_domain_net$site_idv,
                    j = uid_domain_net$user_idv,
                    x = 1)
# From the user -> site matrix, now compute the site -> site matrix
r <- t(mat)
mat <- mat %*% r 
m <- as (mat, "dgTMatrix")
df <- data.table(i = m@i + 1, j = m@j + 1, x = m@x)
df <- merge(df, site_df, by.x="i",by.y="site_idv")
setnames(df, "website","site_i")
df <- merge(df, site_df, by.x="j",by.y="site_idv")
setnames(df, "website","site_j")
df[, i:= NULL]
df[, j:= NULL]

# function to construct the sparse network
# based on Dianti's methodology (see paper for reference and description)
gen_sparse_network <- function(df){
  weight_df <-df[,list(weight=sum(x)),by=site_i]
  setnames(weight_df,"site_i","website")
  
  total_degree <- sum(weight_df$weight)
  
  d <- df[site_i <= site_j]
  d <- merge(d,weight_df,by.x="site_i",by.y="website")
  setnames(d, "weight","weight_i")
  
  d <- merge(d,weight_df, by.x="site_j",by.y="website")
  setnames(d, "weight","weight_j")
  
  d[, q := total_degree / 2.0]
  d[, p := weight_i * weight_j / q / q / 2.0]
  d[,pval :=  pbinom(x - 1, round(q), p, lower.tail = FALSE),]
  d[,significance := -log(pval)]
  d <- d[order(-significance)]
  max_sig <- max(d[!is.infinite(significance)]$significance)
  d[is.infinite(significance), significance := max_sig]
  return(d)
}
d <- gen_sparse_network(df)

######################################################################################
######################################################################################
######################################################################################
d <- fread("coexposure_network/data/site_to_site_connections.csv")
######################################################################################
######################################################################################
######################################################################################

######################################################################################
######################################################################################
########### Generate final network ###########
# Construct the graph, the significance level used is the top 3% of links
# also, get rid of self-loops
######################################################################################
######################################################################################

net_start <- d[site_i != site_j ]
g <- graph_from_data_frame(net_start[significance > quantile(net_start$significance, probs=seq(0,1,.01))[97]][,.(site_i,site_j)],directed=F)


# Focus only on the LWCC
comps <- clusters(g, mode="weak")
comps$csize
max_component <- which.max(comps$csize)
max_connected_comp <- as.undirected(induced.subgraph(g,which(comps$membership==max_component)))

############################################################
############### CLUSTERING ################################
############################################################

### Generate the three different clusterings
clust <- cluster_louvain(max_connected_comp)
clust1 <- cluster_label_prop(max_connected_comp)
clust2 <- cluster_walktrap(max_connected_comp)

clust_df <- data.table(V(max_connected_comp)$name, clust$membership, clust1$membership,clust2$membership )
setnames(clust_df, c("site","louv","lp","wt"))

# How many major sets of clusters do we have?
clust_df[, .N, by=.(louv,lp,wt)][order(-N)]
# identify each cluster by sites we know from looking at the data have consistent 
# groupings and are in the three different large clusters
cnn <- clust_df[site == "cnn.com"]
dc <- clust_df[site == 'dcclothesline.com']
oth <- clust_df[site == "advocate.com"]
clust_df[, in_mainstream := louv == cnn$louv & lp == cnn$lp & wt == cnn$wt]
clust_df[, in_right := louv == dc$louv & lp == dc$lp & wt == dc$wt]
clust_df[, in_oth := louv == oth$louv & lp == oth$lp & wt == oth$wt]
clust_df[, clust := ifelse(in_mainstream, "Mainstream", ifelse(in_right, "Right", 
                                                               ifelse(in_oth,"Left","Other")))]

##################### Plot network pictures###########################

plot_fn <- function(cl,l,filename){
  pdf(filename)
  plot(cl,max_connected_comp,
       edge.width = .1,
       vertex.label.cex=.3,
       vertex.label = NA,
       vertex.frame.color=ifelse(V(max_connected_comp)$name %in% domains_colored$domain, "black",NA),
       vertex.size=2.5,
       layout=l,
       edge.arrow.size=0,
       mark.groups=NULL,
       edge.color=adjustcolor("grey",.4))
  dev.off()
}
# plot the network
l <- layout_with_kk(max_connected_comp)

plot_fn(clust,l,"coexposure_network/img/figure_s11_1.pdf")
plot_fn(clust1,l,"coexposure_network/img/figure_s11_2.pdf")
plot_fn(clust2,l,"coexposure_network/img/figure_s11_3.pdf")

#### Pretty plot

tot_exp <- fread("coexposure_network/data/total_exposure_counts.csv")
V(max_connected_comp)[tot_exp$website]$size <- ifelse(tot_exp$N/300000 < 1.5, 1.5, tot_exp$N/300000)

V(max_connected_comp)[clust_df$site]$clust <- clust_df$clust
v_cl <- V(max_connected_comp)$clust
V(max_connected_comp)$fc <- ifelse(v_cl == "Mainstream", "darkgreen", 
                                   ifelse(v_cl == "Left", "purple", ifelse(v_cl == "Right", "orange", "darkgrey")))

V(max_connected_comp)$color <- V(max_connected_comp)$fc
V(max_connected_comp)[!V(max_connected_comp)$name %in% domains_colored[domain_color!="Satire"]$domain]$color <- "white"

pdf("coexposure_network/img/figure_7.pdf",height = 8,width = 8)
plot(max_connected_comp,
     vertex.frame.color=V(max_connected_comp)$fc,
     vertex.label=NA,
     edge.width = .04,
     layout=l,
     edge.arrow.size=0,
     edge.color=adjustcolor("grey",.4),
     vertex.color=adjustcolor(V(max_connected_comp)$color,.8))
dev.off()


pdf("coexposure_network/img/figure_s10.pdf",height = 8,width = 8)
plot(max_connected_comp,
     vertex.frame.color=adjustcolor(V(max_connected_comp)$fc,.4),
     vertex.label.cex=.15,
     edge.width = .04,
     layout=l,
     edge.arrow.size=0,
     edge.color=adjustcolor("grey",.4),
     vertex.color=adjustcolor(V(max_connected_comp)$color,.4))
dev.off()




######### Partisanship and Fakeness metrics for the clusters   ####### 

setnames(clust_df, "site","domain")

######################################################################################
######################################################################################
### As noted in the readme, this file must be obtain from the authors of Bakshy et al.
######################################################################################
######################################################################################
fb_dat <- fread("restricted_data/fb_top500_domain_affl.csv")
fb_dat <- merge(clust_df, by="domain",fb_dat, all.x=T)
fb_dat <- merge(fb_dat, domains_colored, all.x=T)
ggplot(fb_dat, 
       aes(factor(clust),avg_align))+ geom_boxplot()

fb_dat[!is.na(avg_align), as.list(smean.cl.boot(avg_align)),by=clust]

t.test(fb_dat[clust == "Mainstream"]$avg_align, 
       fb_dat[clust == "Left"]$avg_align,"greater")

t.test(fb_dat[clust == "Right"]$avg_align, 
       fb_dat[clust == "Mainstream"]$avg_align,"greater")
t.test(fb_dat[clust == "Other"]$avg_align, 
       fb_dat[clust == "Mainstream"]$avg_align)

ptest <- fb_dat[, list(n=.N,fn=sum(!is.na(domain_color))),by=clust]
ptest[, p := fn/n]
prop.test(c(ptest[clust == "Mainstream"]$fn,ptest[clust== "Right"]$fn),
          c(ptest[clust == "Mainstream"]$n,ptest[clust== "Right"]$n))

prop.test(c(ptest[clust == "Mainstream"]$fn,ptest[ clust== "Other"]$fn),
          c(ptest[clust == "Mainstream"]$n,ptest[clust== "Other"]$n))

prop.test(c(ptest[clust == "Mainstream"]$fn,ptest[clust== "Left"]$fn),
          c(ptest[clust == "Mainstream"]$n,ptest[clust== "Left"]$n))




################# At individual level  - percent in group 1#############################

# what percent of exposures are to those sites in exposure_sites?
exp_clust <- merge(uid_domain_net, clust_df,by.x="website",by.y="site",all.x=T)
exp_clust <- merge(exp_clust[, list(total=sum(N)),by=panel_uid],
                   exp_clust[, list(per_clust=sum(N)),by=.(clust,panel_uid)])
exp_clust[, percent_clust := per_clust/total]
# take only people w/ > 100 exposures
exp_clust <- exp_clust[total > 100]

# what percentage of mainstream content did people see across partisanship?
smean.cl.boot(exp_clust[clust == "Mainstream"]$percent_clust)


#### Cannot show for political affiliation bucket, however, here is the code for that
#exp_clust <- merge(exp_clust, panel[,.(user_id,pol_affl_bucket)],by.x="panel_uid",by.y="user_id")
#exp_clust[clust=="Mainstream", as.list(smean.cl.boot(percent_clust)),by=pol_affl_bucket]


