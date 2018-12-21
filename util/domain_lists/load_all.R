library(data.table)
library(stringr)

freeze <- ls() # keep a list of all objects created until this is loaded

source("buzzfeed.R")
source("nyhan.R")
source("factcheck.R")

# Politifact set downlaoded from http://www.politifact.com/punditfact/article/2017/apr/20/politifacts-guide-fake-news-websites-and-what-they/
# Contains 200 distinct sites in data dated as May 16, 2017, downloaded Aug 2017.
politifact <- fread("politifact.tsv", showProgress = F)
setnames(politifact, c("domain", "classification"))
politifact <- politifact[, domain:=tolower(domain)]
politifact <- politifact[classification=="Parody site", domain_color:="Satire"]
politifact <- politifact[classification=="Some fake stories", domain_color:="Orange"]
politifact <- politifact[is.na(domain_color), domain_color:="Black"]

snopes_anno <- fread("snopes_annotated.tsv", showProgress = F)
snopes_anno <- snopes_anno[, domain:=tolower(domain)]
setnames(snopes_anno, paste0("Annotation #", 1:3), paste0("anno", 1:3))
setnames(snopes_anno, "Final", "domain_color")
snopes_domain_color_l <- c("N/A", "Green", "Yellow", "Orange", "Red", "Satire")
snopes_anno <- snopes_anno[,`:=`(anno1_fac=factor(anno1, snopes_domain_color_l), 
                                 anno2_fac=factor(anno2, snopes_domain_color_l), 
                                 domain_color_fac=factor(domain_color, snopes_domain_color_l))]

nonfake_anno <- data.table(read.csv("nonfake_annotated_2.csv"))
nonfake_anno$website_cat <- str_trim(nonfake_anno$website_cat)
setnames(nonfake_anno, "denom", "is_pol_news_site")
nonfake_anno <- nonfake_anno[, website:=tolower(website)]
website_cat_l <- c('news/blog', 'non-political news/blog', 'org', 'fact checker', 'government', 
                   'link shortener', 'other', 'politician', 'satire/comedy', 'social media/UGC')
org_site_cat_l <- c('political', 'commercial', 'neither')
website_cat_combined_l <- c(website_cat_l[website_cat_l != "org"], paste("org", org_site_cat_l, sep = "_"), "org")
paste2 <- function(...,sep="_") {
  L <- list(...)
  L <- lapply(L,function(x) {x[is.na(x)] <- ""; x})
  gsub(paste0("(^",sep,"|",sep,"$)"),"",
       gsub(paste0(sep,sep),sep,
            do.call(paste,c(L,list(sep=sep)))))
}
nonfake_anno <- nonfake_anno[,`:=`(
  how_sampled=factor(how_sampled, c("top_80pct", "weighted_tail")),
  website_main_cat=factor(website_cat, website_cat_l), 
  website_main_cat1=factor(website_category1, website_cat_l), 
  website_main_cat2=factor(website_category2, website_cat_l), 
  website_cat=factor(paste2(website_cat, website_subcat), website_cat_combined_l),
  website_subcat=factor(website_subcat, org_site_cat_l), 
  website_subcat1=factor(website_subcategory1, org_site_cat_l), 
  website_subcat2=factor(website_subcategory2, org_site_cat_l))]

# merging of all colored lists and leave only Black, Red, Orange & Satire
domains_colored <- data.table(domain=unique(c(buzzfeed, nyhan_new, factcheck)), domain_color="Black")
domains_colored <- merge(domains_colored, politifact, by="domain", all=T, suffixes = c("", "_pf"))
domains_colored <- domains_colored[is.na(domain_color), domain_color:=domain_color_pf]
domains_colored <- domains_colored[,.(domain, domain_color)]
domains_colored <- merge(x=snopes_anno[domain_color %in% c("Orange", "Red", "Satire"),.(domain, domain_color)], 
                   y=domains_colored, by="domain", all=T, suffixes = c("", "_rest"))
domains_colored <- domains_colored[is.na(domain_color), domain_color:=domain_color_rest]
domains_colored <- domains_colored[,.(domain, domain_color)]

domains_colored <- domains_colored[!duplicated(domain)]
rm(list = setdiff(ls(), c(freeze, "domains_colored", "snopes_anno", "nonfake_anno"))) # delete all unnecessary objects 
