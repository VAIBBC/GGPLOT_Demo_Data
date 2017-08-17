#######################   Libraries    #############################

library(ggplot2)
library(reshape2)
library(cowplot)
library(survminer)
library(survival)
library(ggthemes)
library(plotly)
library(RColorBrewer)
library(gridGraphics)
library(pheatmap)

#####################  Formatting Preferences  #####################

# Colors
mycols = c("#000000", "#3F60AE" , "#D92120","#781C81","#539EB6", "#CAB843", "#6DB388", "#E78532")

mytheme <- theme_classic(10) + theme( axis.line=element_line(size=1,color="black"),
                                      axis.ticks = element_line(colour="black",size=1),
                                      axis.title = element_text(size=10,family="Arial"),
                                      axis.text =  element_text(size=10,family="Arial"),
                                      legend.text = element_text(size=10,family="Arial"),
                                      title = element_text(size=10,family="Arial"),
                                      strip.background=element_blank(),
                                      plot.margin = unit(c(0, 0, 0, 0), "cm"),
                                      legend.key.size = unit(.5,"line"))

##  "Cheat Sheet"  ##
#https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf
##  Themes  ##
#https://cran.r-project.org/web/packages/ggthemes/vignettes/ggthemes.html

options(stringsAsFactors=F)

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
  geom_bar(data=agg.data,stat="identity",position=position_dodge(width=.65),aes(x=Treatment,y=Latency,color=Genotype),width=.5,fill="white",size=1) +
  geom_errorbar(data=agg.data,aes(ymin=Latency-SE,ymax=Latency+SE),width=.25,position=position_dodge(width=.65),color="black",size=1) +
  geom_point(position=position_jitterdodge(jitter.width=.25,jitter.height=0,dodge.width=.65),size=1) + 
  scale_color_manual(values=mycols) + scale_x_discrete(expand=c(0,0)) + scale_y_continuous(expand=c(0,0),limits=c(0,35)) + mytheme + theme(legend.position="top")

#Create PDF of plot and plot2
cairo_pdf("/home/zach/plot.pdf",width=4,height=4)
plot
dev.off()

cairo_pdf("/home/zach/plot2.pdf",width=4,height=4)
plot2
dev.off()


######    Cumulative Percent Bars   #########

pct_dat = read.table("/home/zach/Desktop/GGPLOT_Demo_Data-master/Percent_Death.csv",sep=",",header=T,check.names=F)
melt_pct = melt(pct_dat,id.vars="group")

#Making labels shorter
melt_pct$variable = as.character(melt_pct$variable)
melt_pct$variable[melt_pct$variable == "Euthanized at End of Study"] = "Healthy "
melt_pct$variable[melt_pct$variable == "Unrelated Sickness"] = "Unrelated "
melt_pct$variable[melt_pct$variable == "Due to Tumor"] = "Tumor "



pct.bar = ggplot(melt_pct,aes(x=group,y=value,colour=variable,fill=variable)) +
  geom_bar(stat="identity")+coord_flip() +scale_y_continuous(position="top",expand=c(0,0)) + scale_color_manual(values=mycols[5:7])+
  scale_fill_manual(values=mycols[5:7]) + xlab("") + ylab("Percentage") +
  mytheme+ theme(legend.title=element_blank(),legend.position="bottom",axis.text.y=element_text(angle=45,hjust=1),legend.key.size = unit(.5,"lines"))



######  survival  #######  
surv.d = read.table("/home/zach/Desktop/GGPLOT_Demo_Data-master/Rasagiline_s.csv",sep=",",header=T)
fit.s = survfit(Surv(Age)~Treatment + Sex,data=surv.d)

surv.p  = ggsurvplot(fit.s,data=surv.d,linetype="strata")$plot + scale_color_manual(values=mycols[c(1,1,2,2)],labels=c("F Placebo", "M Placebo","F Rasagiline" ,"M Rasagiline")) + 
  scale_linetype_manual(values=c(1,2,1,2),labels=c("F Placebo", "M Placebo","F Rasagiline" ,"M Rasagiline")) +
  scale_x_continuous(limits=c(-Inf,Inf)) + mytheme + ylab("Age") +
  theme(legend.title=element_blank(),legend.position=c(.3,.35))



####    Longitudinal  ####
long.d = read.table("/home/zach/Desktop/GGPLOT_Demo_Data-master/Tumor_Growth.csv",sep=",",header=T,check.names=F)
long.d.m = melt(long.d,id.vars=c("Mouse","Treatment","Sex"))
long.d.m = na.omit(long.d.m)

long.d.m$Treatment = factor(long.d.m$Treatment,levels=c("Vehicle","Erlotinib","midostaurin ","Erl + Mido"))

ggplot(long.d.m,aes(x=variable,y=value,color=Treatment)) + geom_point() + geom_line(aes(group=Mouse)) + scale_color_manual(values=mycols)+mytheme

ggplot(long.d.m,aes(x=variable,y=value,color=Treatment)) + stat_smooth(aes(group=Treatment,fill=Treatment),alpha=.5) + scale_color_manual(values=mycols)+scale_fill_manual(values=mycols)+mytheme

agg.data = aggregate(value~variable+Treatment,long.d.m,"mean")
agg.data$SE = aggregate(value~variable+Treatment,long.d.m,function(x){sd(x)/length(x)})$value

tg = ggplot(agg.data,aes(x=variable,y=value,color=Treatment)) + geom_point() + geom_line(aes(group=Treatment)) + scale_color_manual(values=mycols)+
  geom_errorbar(aes(ymin=value-SE,ymax=value+SE),width=.15) + ylab(expression(Tumor~Volume~(mm^3))) + xlab("Days Since TX Start") +
  mytheme + theme(legend.position=c(.3,.8),legend.title=element_blank())


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
  xlab(colnames(synergy.d)[2]) + ylab(colnames(synergy.d)[3]) + mytheme + theme(legend.position="right",axis.text.x=element_text(angle=45,hjust=1))

p = plot_grid(plot2,pct.bar,surv.p,tg,synergy,labels=c("A","B","C","D","E"),ncol=2,rel_widths=c(1,1,1,1,1),label_size=18)




##########  Heat Map  ##########################

heat.d = read.table("/home/zach/Desktop/GGPLOT_Demo_Data-master/Gene_Express.csv",sep=",",header=T)

annotate = subset(heat.d,select=c(Treatment,Genotype))
annotate$Treatment = as.factor(annotate$Treatment)
my_palette=colorRampPalette( c("green","black","red"))(400)

ann_colors = list(Genotype = c(WT = "gray",KO="Blue"),
     Treatment = c(Control="black",Drug="purple")
)

#save the heatmap
p = pheatmap(t(data.matrix(subset(heat.d,select=-c(Genotype,Treatment)))),annotation_col=annotate,scale="row",color=my_palette,annotation_colors=ann_colors,silent=T,treeheight_row=25,treeheight_col=25,font.size=10,fontsize_col = 5,border_color=NA)

#align x and y axis
ps =align_plots(plot2,pct.bar,surv.p,synergy,tg, align="hv", axis="tblr")

#plot eevrything into one labeled figure
multi = plot_grid(ps[[1]],ps[[2]],ps[[3]],ps[[4]],ps[[5]],p$gtable,labels=c("A","B","C","D","E","F"),ncol=2,label_size=14,scale=c(1,1,1,1,1,.85))
 
dev.off()
## Create a multipanel figure
cairo_pdf("/home/zach/fig_ex",height=12,width=9)
multi
dev.off()




