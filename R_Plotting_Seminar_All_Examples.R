#######################   Libraries    #############################

library(ggplot2)
library(reshape2)
library(cowplot)
library(survminer)
library(ggthemes)
library(plotly)
library(RColorBrewer)
library(NMF)

#####################  Formatting Preferences  #####################

# Colors
mycols = c("#000000", "#3F60AE" , "#D92120","#781C81","#539EB6", "#CAB843", "#6DB388", "#E78532")

mytheme <- theme_classic(12) + theme( axis.line=element_line(size=1,color="black"),
                                      axis.ticks = element_line(colour="black",size=1),
                                      axis.title = element_text(size=10,family="Arial"),
                                      axis.text =  element_text(size=10,family="Arial"),
                                      legend.text = element_text(size=10,family="Arial"),
                                      title = element_text(size=10,family="Arial"),
                                      strip.background=element_blank(),
                                      plot.margin = unit(c(2,2,2,2), "lines"))

##  "Cheat Sheet"  ##
#https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf
##  Themes  ##
#https://cran.r-project.org/web/packages/ggthemes/vignettes/ggthemes.html

#############  Scatterplot   ##########################

data1 = read.table("/home/zach/Desktop/GGPLOT_Demo_Data-master/Desipramine.csv",sep=",",header=T)

data1$Genotype = factor(data1$Genotype,levels=c("WildType","En2 -/-"))

plot = ggplot(data = data1, aes(x=Treatment,y=Latency,color=Genotype)) + 
  geom_jitter(width=.25,height=0) + 
  scale_color_manual(values=mycols)

agg.data = aggregate(Latency~Treatment + Genotype,data1,"mean")
agg.data$SE = aggregate(Latency~Treatment + Genotype,data1,function(x){sd(x)/length(x)})$Latency

plot = ggplot(data = data1, aes(x=Treatment,y=Latency,color=Genotype,fill=Genotype)) + 
  geom_bar(data=agg.data,stat="identity",position=position_dodge(width=1),aes(x=Treatment,y=Latency,color=Genotype),width=.75) +
  geom_errorbar(data=agg.data,aes(ymin=Latency-SE,ymax=Latency+SE),width=.5,position=position_dodge(width=1),color="black",size=1) +
  scale_color_manual(values=mycols) + scale_fill_manual(values=mycols) + scale_y_continuous(expand=c(0,0)) + mytheme
  
plot2 = ggplot(data = data1, aes(x=Treatment,y=Latency,color=Genotype,fill=Genotype)) + 
  geom_bar(data=agg.data,stat="identity",position=position_dodge(width=1),aes(x=Treatment,y=Latency,color=Genotype),width=.75,fill="white",size=1) +
  geom_errorbar(data=agg.data,aes(ymin=Latency-SE,ymax=Latency+SE),width=.5,position=position_dodge(width=1),color="black",size=.5) +
  geom_point(position=position_jitterdodge(jitter.width=.25,jitter.height=0,dodge.width=1),size=3) + 
  scale_color_manual(values=mycols) + scale_y_continuous(expand=c(0,.2)) + mytheme 

#Create PDF of plot and plot2
cairo_pdf("/home/zach/plot.pdf",width=4,height=4)
plot
dev.off()

cairo_pdf("/home/zach/plot2.pdf",width=4,height=4)
plot2
dev.off()


######    Cumulative Percent Bars   #########

pct_dat = read.table("/home/zach/Desktop/GGPLOT_Demo_Data-master/Percent_Death.csv",sep=",",header=T)
melt_pct = melt(pct_dat,id.vars="group")

pct.bar = ggplot(melt_pct,aes(x=group,y=value,colour=variable,fill=variable)) +
  geom_bar(stat="identity")+coord_flip() +scale_y_continuous(position="top",expand=c(0,0)) + scale_color_manual(values=c(brewer.pal(5,"Blues")[2:4]))+
  scale_fill_manual(values=c(brewer.pal(5,"Blues")[2:4])) + xlab("") + ylab("Percentage") + mytheme+ theme(legend.title=element_blank(),legend.position="bottom")



######  survival  #######  
surv.d = read.table("/home/zach/Desktop/GGPLOT_Demo_Data-master/Rasagiline_s.csv",sep=",",header=T)
fit.s = survfit(Surv(Age)~Treatment + Sex,data=surv.d)

surv.p  = ggsurvplot(fit.s,data=surv.d)$plot + scale_color_manual(values=mycols,labels=c("F Placebo", "M Placebo","F Rasagiline" ,"M Rasagiline")) + 
  scale_x_continuous(limits=c(-Inf,Inf)) + mytheme +
  theme(legend.title=element_blank(),legend.position=c(.3,.5))



####    Longitudinal  ####
long.d = read.table("/home/zach/Desktop/GGPLOT_Demo_Data-master/Tumor_Growth.csv",sep=",",header=T,check.names=F)
long.d.m = melt(data4,id.vars=c("Mouse","Treatment","Sex"))
long.d.m = na.omit(data4.m)


ggplot(long.d.m,aes(x=variable,y=value,color=Treatment)) + geom_point() + geom_line(aes(group=Mouse)) + scale_color_manual(values=mycols)+mytheme

ggplot(long.d.m,aes(x=variable,y=value,color=Treatment)) + stat_smooth(aes(group=Treatment,fill=Treatment),alpha=.5) + scale_color_manual(values=mycols)+scale_fill_manual(values=mycols)+mytheme

agg.data = aggregate(value~variable+Treatment,data4.m,"mean")
agg.data$SE = aggregate(value~variable+Treatment,data4.m,function(x){sd(x)/length(x)})$value

tg = ggplot(agg.data,aes(x=variable,y=value,color=Treatment)) + geom_point() + geom_line(aes(group=Treatment)) + scale_color_manual(values=mycols)+
  geom_errorbar(aes(ymin=value-SE,ymax=value+SE),width=.15) + mytheme + theme(legend.position=c(.3,.8))


####  Geom_tile  ####
synergy.d = read.table("/home/zach/Desktop/GGPLOT_Demo_Data-master/Synergy.csv",sep=",",header=T)
synergy.d$ispinesib = as.factor(synergy.d$ispinesib)
synergy.d$ibrutinib = as.factor(synergy.d$ibrutinib)

synergy = ggplot(synergy.d, aes(x = ispinesib, y =  ibrutinib))  +
  geom_point(size=-1) + 
  geom_rect(aes(xmin=(as.numeric(synergy.d[,2])-0.5*abs(Inhibition/100)),xmax=as.numeric(synergy.d[,2])+0.5*abs(Inhibition/100),ymin=(as.numeric(synergy.d[,3])-0.5*abs(Inhibition/100)),ymax=(as.numeric(synergy.d[,3])+0.5*abs(Inhibition/100)),fill=Synergy,color=Synergy)) +
  geom_text(aes_string(fill = "Synergy", label = "Inhibition"),family ="Arial") +
  ggtitle("") + 
  scale_fill_gradient2(low = "blue3", mid = "white", high="firebrick3",
                       midpoint = 0,limits=c(-100,100), name = "Synergy",na.value="black") +
  scale_color_gradient2(low = "blue3", mid = "white", high="firebrick3",
                        midpoint = 0,limits=c(-100,100), name = "Synergy",na.value="black") +
  xlab(colnames(synergy.d)[2]) + ylab(colnames(synergy.d)[3]) + mytheme


## Create a multipanel figure
cairo_pdf("/home/zach/fig_ex.pdf",height=11,width=8.5)
  plot_grid(plot2,pct.bar,surv.p,tg,synergy,labels=c("A","B","C","D","E"),ncol=2,rel_widths=c(1,1,1,1,1),label_size=18)
dev.off()


##########  Heat Map  ##########################

heat.d = read.table("/home/zach/Desktop/GGPLOT_Demo_Data-master/Gene_Express.csv",sep=",",header=T)

annotate = subset(heat.d,select=c(Treatment,Genotype))
my_palette=colorRampPalette( c("blue","black","red"))(400)
heat.p = aheatmap(t(data.matrix(subset(heat.d,select=-c(Genotype,Treatment)))),annCol=annotate,scale="row",color=my_palette,breaks=c(0),annColors=list(colorRampPalette(c("gray","black"))(2),colorRampPalette(c("purple","orchid"))(2)))
#saves it as a pdf
heat.p = aheatmap(t(data.matrix(subset(heat.d,select=-c(Genotype,Treatment)))),annCol=annotate,scale="row",color=my_palette,breaks=c(0),annColors=list(colorRampPalette(c("gray","black"))(2),colorRampPalette(c("purple","orchid"))(2)),file="/home/zach/heat_ex.pdf")



