library(ggplot2)
library(extrafont)
library(scales)
library(grid)
library(RColorBrewer)
# checkout this blogpost http://blog.revolutionanalytics.com/2012/09/how-to-use-your-favorite-fonts-in-r-charts.html

# fontFamily <- "Source Sans Pro"
# fontTitle <- "Source Sans Pro Semibold"
# fontText <- "Source Sans Pro"
# fontFamily <- "Avenir Next Condensed Demi Bold"
# fontTitle <- "Avenir Next Condensed Heavy"
# fontText <- "Avenir Next Condensed"
fontText <- "Helvetica"
# extra_fonts <- c(fontFamily, fontTitle, fontText)

colors = c("#16a085","#27ae60","#2980b9","#8e44ad","#f39c12","#c0392b","#1abc9c", "#2ecc71", "#3498db", "#9b59b6", "#f1c40f","#e74c3c")

neutral_colors = function(number) {
	return (brewer.pal(11, "RdYlBu")[-c(5:7)][(number %% 8) + 1])
}

set1_colors = function(number) {
	return (brewer.pal(9, "Set1")[c(-6,-8)][(number %% 7) + 1])
}

my_custom_theme <- function(no.y.line=FALSE, base_size = 24) {theme_bw(base_size = base_size) + 
                             theme(#panel.background = element_rect(fill="#eaeaea"),
                                   #plot.background = element_rect(fill="white"),
                                   #plot.background = element_rect(fill="#eaeaea", color="#eaeaea"),
                                   legend.background = element_rect(color="black", linetype="dashed"),#, fill="#eaeaea"),
                                   panel.grid.minor = element_blank(),
                                   #panel.grid.major = element_line(color="#dddddd"),
                                   panel.grid.major = element_line(color="#cfcfcf", size=1),
                                   axis.ticks.x = element_blank(),
                                   axis.ticks.y = element_blank(),
                                   axis.title.x = element_text(family=fontText),#, face="bold", size=rel(1.2), vjust=-.15),
                                   axis.title.y = element_text(family=fontText),#, face="bold", size=rel(1.2), vjust=1),
                                   #panel.border = element_rect(color="#cccccc"),
                                   panel.border = element_blank(), 
                                   text = element_text(color = "#1a1a1a", family=fontText),
                                   #plot.margin = unit(c(0.75,0.6,0.30,0.35), "cm"),
                                   plot.margin = unit(c(1, 1, 0.5, 0.5), "lines"),
                                   axis.line = element_line(color = "black", size=2),
                                   axis.line.y=element_blank(),
                                   plot.title = element_text(family=fontText, size=rel(1.4), vjust=2))                          
}

scale_dimension.custom_expand <- function(scale, expand = ggplot2:::scale_expand(scale)) {
  expand_range(ggplot2:::scale_limits(scale), expand[[1]], expand[[2]])
}

custom_scale_y_continuous <- function(...) {
  s <- ggplot2::scale_y_continuous(...)
  class(s) <- c('custom_expand', class(s))
  s
}

perc_labels <- function(x) {paste(format(100*x/sum(x),digits=0,scientific = FALSE),"%",sep='')}

# data <- as.data.frame(table(rpois(50,5)))
# custom_color <- set1_colors(4)
# ggplot(aes(x=Var1, y=Freq), data=data) +
#   geom_bar(stat="identity", fill=custom_color, alpha=0.8) + 
#   custom_scale_y_continuous(expand=list(c(0,0), c(0,0.2)), breaks=c(0,3,6,9,12)) + 
#   geom_text(label=perc_labels(data$Freq), color = "#FFFFFF", vjust=1.5) +
#   labs(x="Var", y="Freq", title="rpois(50,5)") +
#   my_custom_theme() + 
#   theme(axis.title.y = element_text(color=custom_color), axis.title.x = element_text(color=custom_color))
