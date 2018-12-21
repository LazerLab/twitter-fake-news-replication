library(data.table)
library(ggplot2)
library(prophet)
library(changepoint)
library(lubridate)
library(Hmisc)
source("util/plotting.R")

theme_set(my_custom_theme())

######### ECDFs

plot_ecdf <- function(data_filename, out_of_k, x_lab, y_lab){
  fep <- fread(data_filename)
  p <- ggplot(fep, aes(percentage_of_total,cumulative_percentage,color=category))
  p <- p + scale_x_log10(paste("% of ",x_lab), limits=c(0.00001,1),breaks=c(0,0.001,.01,.1,1),labels=percent)
  p <- p + geom_line(size=1.8) 
  p <- p + scale_color_manual("Site Type",values=c("Everything Else"="blue",
                                                   "Red"="red",
                                                   "Black"="black",
                                                   "Orange"="orange"))
  p <- p + scale_y_continuous(paste0("% of All ",y_lab),labels=percent)
  p <- p + theme(legend.position = "None")
  p
}

#### How many people do we have after removing bot-like panel members in addition to exclusion criterion in paper? 
n_nonbot_panel <- 16442

# Figure 1b - site exposures ecdf
p <- plot_ecdf("figure_1/data/figure_1b_data.csv", n_nonbot_panel, "Websites", "Exposures")
ggsave("figure_1/img/figure_1b.pdf",p,h=6,w=7)
# Figure 1c - panel_share_nobot_ecdf
p <- plot_ecdf("figure_1/data/figure_1c_data.csv", n_nonbot_panel, "Panel Members", "Shares")
ggsave("figure_1/img/figure_1c.pdf",p,h=6,w=7)
# Figure 1d - panel_exp_nobot_ecdf
p <- plot_ecdf("figure_1/data/figure_1d_data.csv", n_nonbot_panel, "Panel Members", "Exposures")
ggsave("figure_1/img/figure_1d.pdf",p,h=6,w=7)


########### Bar Plots and Time Series
# Figure 1a - Exposures per day
# Figure S6 - Shares of Fake News URLs
# Figure S7 - Exposures using only Nyhan list
# Figure S8 - Time series checks

gen_timeseries_analysis <- function(data_filename,data_type, penalty_val=.001, fake_sites = c(),ymax=NA){
  day_stats <- fread(data_filename)
  day_stats$date <- ymd(day_stats$date)
  p1 <- ggplot(data = melt(day_stats,"date",c("pct_black","pct_red","pct_orange")), 
               aes(x=date, y=value, group=variable)) +
    geom_bar(stat="identity", position = position_stack(reverse = TRUE), aes(fill=variable)) + 
    geom_vline(xintercept = ymd("2016-11-08"), colour="black", linetype="dashed") +
    scale_fill_manual(values=c("pct_red"= set1_colors(7),
                               "pct_orange"=set1_colors(4),
                               "pct_black"="black")) +
    scale_x_date("Date",date_breaks="1 week",date_minor_breaks = "1 week") +
    my_custom_theme() + theme(legend.position = "none",axis.text.x=element_text(angle=45,hjust=1,size=10))
  if(grepl("exp",data_type)){
    p1 <- p1 +  scale_y_continuous(limits=c(0,0.0826),
                                   "Fake news sources in\naggregate political exposures\n(% per day)", labels=percent)
  } else{
    p1 <- p1 +  scale_y_continuous(
      "Fake news sources in\naggregate political shares\n(% per day)", labels=percent)
  }
  ggsave(paste0("figure_1/img/","fig1a_",data_type,".pdf"),p1,h=6,w=10) 
  
  
  # Does the percentage of fake news increase closer to the election?
  day_stats[,pct_fake := pct_red+pct_orange+pct_black]
  forecast_data <- day_stats[,.(date,pct_fake)][order(date)]
  setnames(forecast_data, c("ds","y"))
  # One thing we can look at are changepoints. They pretty
  m <- cpt.mean(forecast_data$y, 
                test.stat="Normal", 
                method = "PELT", 
                penalty = "Manual", 
                pen.value = penalty_val)
  
  fd <- forecast_data
  fd$ch <- 1
  cpt_dts <- forecast_data$ds[m@cpts]
  for(i in 2:length(m@cpts)){
    fd[ds <= cpt_dts[i] & ds > cpt_dts[i-1]]$ch <- i
  }
  fd <- merge(fd, fd[,mean(y),by=ch])
  plt_ch <- ggplot(fd) + geom_line(aes(ds,y),color='black') + geom_line(aes(ds,V1),color='red') 
  plt_ch <- plt_ch + xlab("Date")
  if(grepl("exp",data_type)){
    plt_ch <- plt_ch +  scale_y_continuous(
      "Exposures to\nFake News", labels=percent)
  } else{
    plt_ch <- plt_ch +  scale_y_continuous(
      "Shares of\nFake News", labels=percent)
  }
  
  ggsave(paste0("figure_1/img/","changepoint_daily_",data_type,".pdf"), plt_ch, h=4,w=6)
  
  # Prophet confirms our suspicions from the more 
  # ad-hoc changepoint detection
  p <- prophet(forecast_data[ds < ymd("2016-11-07")])
  future <- make_future_dataframe(p, periods = 1)
  forecast <- predict(p, future)
  #plot(p, forecast,uncertainty = T)
  plt <- prophet_plot_components(p, forecast)
  plt_sv <- plt[[1]] + xlab("Month") 
  if(grepl("exp",data_type)){
    plt_sv <- plt_sv +  scale_y_continuous("Trend in %age Fake\nNews Exposures", labels=percent)
  } else{
    plt_sv <- plt_sv +  scale_y_continuous("Trend in %age Fake\nNews Shares", labels=percent)
  }
  
  ggsave(paste0("figure_1/img/","trend_",data_type,".pdf"), plt_sv, h=4,w=6)
  
  print(smean.cl.boot(forecast_data$y))
  print(cpt_dts)
  return(day_stats)
}

# Figure 1a, Top row of Figure S8
exposure_stats <- gen_timeseries_analysis("figure_1/data/daily_counts_exposures.csv","exp",.001)

# Figure S6, Bottom row of Figure S8
source("domain_lists/nyhan.R")
exposure_nyhan_stats <- gen_timeseries_analysis("figure_1/data/daily_counts_exposures_guess.csv",
                                                "exp_nyh",
                                                .0003,
                                                fake_sites = c(nyhan_new,"ijr.com") )
share_stats <- gen_timeseries_analysis("figure_1/data/daily_counts_shares.csv","share",.01)

