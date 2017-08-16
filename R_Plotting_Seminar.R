#######################   Libraries    #############################

library(ggplot2)
library(reshape2)
library(cowplot)
library(survminer)
library(synergyfinder)
library(ggthemes)
library(plotly)
library(RColorBrewer)
library(NMF)

#####################  Formatting Preferences  #####################

# Colors
mycols = c("#000000", "#3F60AE" , "#D92120","#781C81","#539EB6", "#CAB843", "#6DB388", "#E78532")

mytheme <- theme_classic(12) + theme( axis.line=element_line(size=1,color="black"),
                                      axis.ticks = element_line(colour="black",size=1),
                                      axis.title = element_text(size=12,family="Arial"),
                                      axis.text =  element_text(size=12,family="Arial"),
                                      legend.text = element_text(size=12,family="Arial"),
                                      title = element_text(size=12,family="Arial"),
                                      strip.background=element_blank())

##  "Cheat Sheet"  ##
#https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf
##  Themes  ##
#https://cran.r-project.org/web/packages/ggthemes/vignettes/ggthemes.html

#############  Scatterplot   ##########################

data1 = read.table("/home/zach/Desktop/GGPLOT_Demo_Data-master/Desipramine.csv",sep=",",header=T)