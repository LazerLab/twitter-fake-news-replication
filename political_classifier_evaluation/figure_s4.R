library(binom)
source("util/plotting.R")

theme_set(my_custom_theme())

# Load in the annotation data, with an additional field for our labeling
survey_data <- fread("political_classifier_evaluation/data/pol_classifier_eval_survey_res_20k.csv")
survey_min <- survey_data[!is.na(answer) & answer != "idk" ]
survey_min[, answer_is_pol := answer %in% c("election","politics")]

# confidence intervals for error rates
fn_p1 <- rbind(binom.confint(sum(survey_min$answer_is_pol & survey_min$fake_url_count >0),
                             sum(survey_min$answer_is_pol),
                             methods="ac"),
               binom.confint(sum(survey_min$is_pol & survey_min$fake_url_count >0),
                             sum(survey_min$is_pol),
                             methods="ac"),
               binom.confint(sum((!survey_min$answer_is_pol) & survey_min$fake_url_count >0),
                             sum((!survey_min$answer_is_pol)),
                             methods="ac"),
               binom.confint(sum((!survey_min$is_pol) & survey_min$fake_url_count >0),
                             sum((!survey_min$is_pol)),
                             methods="ac"))
fn_p1$lab <- c("By Hand\n(MTurk)","Classifier","By Hand\n(MTurk)", "Classifier")
fn_p1$typev <- factor(c("Political Tweets","Political Tweets", "Non-political Tweets","Non-political Tweets"),
                      levels=c("Political Tweets","Non-political Tweets"))

p <- ggplot(fn_p1, aes(lab,mean,ymin=lower,ymax=upper)) + geom_pointrange(size=1.4) + my_custom_theme() + facet_wrap(~typev)
p <- p + xlab("How tweets were labeled (N=19,819)") + scale_y_continuous("Percentage of Tweets\nContaining Link to\n Fake News Site",
                                                                         labels=percent)
ggsave("img/percentage_fake_news.pdf",p,h=5,w=10)