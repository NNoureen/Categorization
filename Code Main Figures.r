##############  libraries required
library('ggplot2')
library('gplots')
library('dplyr')
library('ggpubr')
stringsAsFactors=FALSE
library(circlize)
library(stringr)
library(Hmisc)
library(forcats)
library(data.table)
library(grid)
library(ComplexHeatmap)


################################################################################################
############################ MAIN FIGURES #################################


###				Data for main figures is present in the Data Source File (Excel Sheet)


############### Figure 1 #################################################
############# 1A ###################################

comball = read.table('Fig1A.txt',sep='\t',head=T)

SigTestcombsub = SigTestcomb[,c(1,2,3,8)]

comball2 = merge(comball,SigTestcombsub,by='Cancer')
comballsub1 = comball2[which(comball2$Cancer %in% c("COAD","GBM","HNSC","LAML","LUSC","READ","STAD","TGCT","THYM","UCEC","UVM")),]
comballsub2 = comball2[-which(comball2$Cancer %in% c("COAD","GBM","HNSC","LAML","LUSC","READ","STAD","TGCT","THYM","UCEC","UVM")),]

comballsub1$Group = "G1"
comballsub2$Group = "G2"

comball_recomb = rbind(comballsub1,comballsub2)
comball_recomb$ColorClass = paste(comball_recomb$Group,comball_recomb$EXTENDclass,sep="_")

pdf('Fig1A.pdf')
P1 = ggplot(comball_recomb,aes(x=Cancer, y=Fraction,fill=EXTENDclass,color=EXTENDclass))+
  geom_bar(stat = "identity",width=0.6,position="dodge",linewidth=0.1)+scale_fill_manual(values=c("High"="darkred","Low"="pink"))+scale_color_manual(values=c("High"="pink","Low"="darkred"))+scale_x_discrete(limits=c("COAD","GBM","HNSC","LAML","LUSC","READ","STAD","TGCT","THYM","UCEC","UVM","ACC","BLCA","BRCA","CESC","CHOL","DLBC","ESCA","KICH","KIRC","KIRP","LGG","LIHC","LUAD","MESO","OV","PAAD","PCPG","PRAD","SARC","SKCM","THCA","UCS"))+theme_classic()+theme(axis.text.x=element_text(angle=35,size=7,vjust=1,hjust=1,color="black"),legend.key.size=unit(0.1,"cm"),legend.position="top")#+coord_flip()

grid.newpage()
# Create layout : nrow = 2, ncol =2
pushViewport(viewport(layout = grid.layout(nrow =9, ncol = 1)))
# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 

print(ggpar(P1,font.xtickslab =c("black",9),font.ytickslab =c("black",10),font.x = 9,font.y=9,font.legend=7),vp = define_region(row = 1:3, col = 1))

dev.off()

############# 1B ###################################


SigTest = read.table('Fig1B.txt',sep='\t',head=T)
SigTest = SigTest[order(SigTest$Cancer),]

SigTest$Size = as.factor(SigTest$Size)


pdf('Fig1B.pdf')


P2 = ggscatter(SigTest, x="High_Mean", y="Low_Mean",size ="Size",stroke = 0.6,color = "Significance",palette = c('Sig'='red','NS'='skyblue'),alpha=0.6,,label = "Cancer",label.select = c("GBM","TGCT","KIRP","BRCA","THYM","LGG"),repel=TRUE,font.label = c(6,"plain","black"))+theme_classic()+theme(axis.text.x=element_text(angle=0,size=7,vjust=1,hjust=1),legend.key.size=unit(0.1,"cm"),legend.position="top")+geom_abline(slope=1,intercept=0)


grid.newpage()
# Create layout : nrow = 2, ncol =2
pushViewport(viewport(layout = grid.layout(nrow =10, ncol = 10)))
# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 

print(ggpar(P2,font.xtickslab =c("black",10),font.ytickslab =c("black",10),font.x = 10,font.y=10,ylim=c(-1,0.6),font.legend=10,xlab="High EXTEND TL Ratio",ylab="Low EXTEND TL Ratio"),vp = define_region(row = 1:4, col = 1:4))

dev.off()

############# 1C ###################################
AS_Scores_Pancan = read.table('Fig1C.txt',sep='\t',head=T)


labes1 = AS_Scores_Pancan$Cancer[which(AS_Scores_Pancan$Significance == "Sig")]


pdf('Fig1C.pdf')

P1 = ggscatter(AS_Scores_Pancan, x="HighEXTEND_CNA", y="LowEXTEND_CNA",size ="Size",shape = 21,stroke = 0.8,color = "Significance",palette = c('NS'='azure4','Sig'='red'),label="Cancer",label.select= labes1,repel=T,font.label=c("black",5))+theme_classic()+theme(axis.text.x=element_text(angle=0,size=7,vjust=1,hjust=1,color="black"),legend.key.size=unit(0.05,"cm"),legend.position="top")+geom_abline(slope=1,intercept=0)


grid.newpage()
# Create layout : nrow = 2, ncol =2
pushViewport(viewport(layout = grid.layout(nrow =10, ncol = 10)))
# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 

print(ggpar(P1,font.xtickslab =c("black",10),font.ytickslab =c("black",10),font.x = 10,font.y=10,font.legend=9,xlab="High EXTEND CNA",ylab="Low EXTEND CNA"),vp = define_region(row = 1:4, col = 1:4))

dev.off()

############# 1D ###################################
WGDFrac_Comb = read.table('Fig1D.txt',sep='\t',head=T)

WGDFrac_Comb = WGDFrac_Comb[order(WGDFrac_Comb$HighEXTEND_Ratio),]

WGDFrac_Comb$Size = NULL

for(i in 1:nrow(WGDFrac_Comb)){

if(WGDFrac_Comb$HighEXTEND_Ratio[i] <0.25){
WGDFrac_Comb$Size[i] = 1}else if(WGDFrac_Comb$HighEXTEND_Ratio[i] >= 0.25 & WGDFrac_Comb$HighEXTEND_Ratio[i] < 0.5){
WGDFrac_Comb$Size[i] = 2}else if(WGDFrac_Comb$HighEXTEND_Ratio[i] >= 0.5){
WGDFrac_Comb$Size[i] = 3}

}

WGDFrac_Comb$Size = as.factor(WGDFrac_Comb$Size)


pdf('Fig.1D.pdf')


P2 = ggscatter(WGDFrac_Comb, x="HighEXTEND_WGD", y="LowEXTEND_WGD",size ="Size",stroke = 0.8,color = "Significance",palette = c('Sig'='red','NS'='azure4'),alpha=0.6,label = "Cancer",label.select = c("ACC","BLCA","BRCA","KIRP","LUAD","LUSC","PRAD","SKCM","TGCT"),repel=TRUE,font.label = c(6,"plain","black"))+theme_classic()+theme(axis.text.x=element_text(angle=0,size=7,vjust=1,hjust=1),legend.key.size=unit(0.1,"cm"),legend.position="top")+geom_abline(slope=1,intercept=0)


grid.newpage()
# Create layout : nrow = 2, ncol =2
pushViewport(viewport(layout = grid.layout(nrow =10, ncol = 10)))
# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 

print(ggpar(P2,font.xtickslab =c("black",10),font.ytickslab =c("black",10),font.x = 10,font.y=10,font.legend=9,xlab="High EXTEND WGD Ratio",ylab="Low EXTEND WGD Ratio",xlim=c(0,0.9),ylim=c(0,0.9)),vp = define_region(row = 1:4, col = 1:4))

dev.off()



######################################### Figure 2 ########################################################
###################### Figure 2A

samples <- read.table('Fig2A.txt',sep='\t',head=T)
samples = samples[order(samples$Cancer),]
can_types <- unique(samples$Cancer)

Extend_high  <- samples[which(samples$EXTENDclass=='High'),]
Extend_low  <- samples[which(samples$EXTENDclass=='Low'),]


pdf('plot_tmb.pdf')
par(xaxs='i',mar=c(25,5,4,2))
plot.new()
plot.window(xlim=c(0.3,length(can_types)+1.0),ylim=c(-4,6))

for (i in 1:length(can_types)){

  j <- i-0.2
  k <- i+0.2
  
  can_high <- Extend_high[which(Extend_high$Cancer==can_types[i]),]
  N=dim(can_high)[1]
  
  idx=seq(j-0.2,j+0.2,length=N)
  a=can_high$mutLoad_nonsilentLog
  points(idx,a,pch=20,type='p',cex=0.3, col='red')
  mv=median(a)
  lines(x=c(j-0.15,j+0.15),y=c(mv,mv),lwd=2,col='black')
  
  can_low <- Extend_low[which(Extend_low$Cancer==can_types[i]),]
  N=dim(can_low)[1]
  
  idx=seq(k-0.2,k+0.2,length=N)
  a=can_low$mutLoad_nonsilentLog
  points(idx,a,pch=20,type='p',cex=0.3, col='blue')
  mv=median(a)
  lines(x=c(k-0.15,k+0.15),y=c(mv,mv),lwd=2,col='black')

}

par(xpd=FALSE)
abline(h=10,lwd=1,col='blue')
axis(1,labels=NA,tick=F)
axis(2,at=seq(-4,6,by=1),las=2, cex.axis=0.6)
mtext('TMB', side=2,line=3)
box()
text(1:length(can_types), par("usr")[3]-0.0, 
     srt = 45, pos=2,offset=-0.1, xpd = TRUE,
     labels=can_types, cex = 0.6)
	 
dev.off()



###################### Figure 2B


dataforRatio = read.table('Fig.2A.txt',sep='\t',head=T)

dataforRatio$Size = as.factor(dataforRatio$Size)


pdf('Fig.2A.pdf')


P3 = ggscatter(dataforRatio, x = "Gene", y = "Cancer",color = "ClassColor",palette = c('Low'='blue2','High'='deeppink'),size = "Size")+theme_linedraw()+theme(axis.text.x=element_text(angle= 50,size=8,vjust=1,hjust=1),legend.key.height=unit(0.1,"cm"),legend.position="bottom")

grid.newpage()
# Create layout : nrow = 2, ncol =2
pushViewport(viewport(layout = grid.layout(nrow = 11, ncol = 9)))
# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
}
print(ggpar(P3,font.xtickslab =6,font.ytickslab =6,font.x = 7,font.y=7,font.title=6,font.legend=6,),vp = define_region(row = 1:7, col = 2:5))  

dev.off()


################# Figure 2C


data = read.table('Fig.2C.txt',sep='\t',head=T)
data = data[which(data$Pattern == "NonRec"),]
dataHigh = data[which(data$EXTENDClass == "High"),]
dataLow = data[which(data$EXTENDClass == "Low"),]


pdf('Fig.2C.pdf')
P1 = ggplot(dataHigh, aes(x=GeneOrder, y=Percent,fill=Cancer)) + 
  geom_bar(stat = "identity",width=0.7,color='black',size=0.1)+scale_fill_manual(values=c("BLCA"="red","BRCA"="blue","HNSC"="green4","LGG"="khaki","LIHC"="limegreen","LUAD"="darkblue","GBM"="orange","PAAD"="pink","PRAD"="darkolivegreen3","SARC"="purple","SKCM"="bisque1","STAD"="brown4","TGCT"="grey","THCA"="darkgoldenrod1","THYM"="blueviolet","UCS"="hotpink","UCEC"="cornflowerblue"))+theme_classic()+theme(legend.key.size=unit(0.1,"cm"))+coord_flip()


P2 = ggplot(dataLow, aes(x=GeneOrder, y=Percent,fill=Cancer)) + 
  geom_bar(stat = "identity",width=0.7,color='black',size=0.1)+scale_fill_manual(values=c("BLCA"="red","BRCA"="blue","HNSC"="green4","LGG"="khaki","LIHC"="limegreen","LUAD"="darkblue","GBM"="orange","PAAD"="pink","PRAD"="darkolivegreen3","SARC"="purple","SKCM"="bisque1","STAD"="brown4","TGCT"="grey","THCA"="darkgoldenrod1","THYM"="blueviolet","UCS"="hotpink","UCEC"="cornflowerblue"))+theme_classic()+theme(legend.key.size=unit(0.1,"cm"))+coord_flip()


grid.newpage()
# Create layout : nrow = 2, ncol =2
pushViewport(viewport(layout = grid.layout(nrow = 7, ncol = 6)))
# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 
print(ggpar(P1,font.xtickslab =6,font.ytickslab =6,font.x = 7,font.y=7,font.title=6,font.legend=6),vp = define_region(row = 1:4, col = 1:2))  
print(ggpar(P2,font.xtickslab =6,font.ytickslab =6,font.x = 7,font.y=7,font.title=6,font.legend=6,),vp = define_region(row = 1:4, col = 3:4))  

dev.off()  #### High and Low EXTEND Mutation Panels of the Figure are combined in Adobe to finalize the plots



##################### Figure 2D


TotalCases = read.table('Fig.2D.txt',sep='\t',head=T)


TotalCasesMut = TotalCases[which(TotalCases$Status == "Mutant"),]

pdf('Fig.2D.pdf')
P1 = ggplot(TotalCasesMut, aes(x=Cancer, y=RatioClass,fill=EXTENDclass,color=EXTENDclass)) + 
  geom_bar(stat = "identity", position=position_dodge(),width=0.7,size=0.1)+scale_fill_manual(values=c("High"="red","Low"="blue"))+scale_color_manual(values=c("High"="red","Low"="blue"))+theme_classic()+theme(axis.text.x=element_text(angle= 50,size=8,vjust=1,hjust=1),legend.key.height=unit(0.1,"cm"),legend.position="bottom")
##+coord_flip()



grid.newpage()
# Create layout : nrow = 2, ncol =2
pushViewport(viewport(layout = grid.layout(nrow = 7, ncol = 8)))
# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 
print(ggpar(P1,font.xtickslab =8,font.ytickslab =8,font.x = 8,font.y=8,font.title=6,font.legend=6),vp = define_region(row = 1:2, col = 1:4))  


dev.off()


#############  Figure 2E ##############



TotalCases = read.table('Fig.2E.txt',sep='\t',head=T)

TotalCasesMut = TotalCases[which(TotalCases$Status == "Mutant"),]
TotalCasesMut = TotalCasesMut[which(TotalCasesMut$Cancer %in% c("GBM","LGG","SARC")),]
pdf('Fig.2E.pdf')
P1 = ggplot(TotalCasesMut, aes(x=Cancer, y=RatioClass,fill=EXTENDclass,color=EXTENDclass)) + 
  geom_bar(stat = "identity", position=position_dodge(),width=0.8,size=0.1)+scale_fill_manual(values=c("High"="red","Low"="blue"))+scale_color_manual(values=c("High"="red","Low"="blue"))+theme_classic()+theme(axis.text.x=element_text(angle= 50,size=8,vjust=1,hjust=1),legend.key.height=unit(0.1,"cm"),legend.position="bottom")
##+coord_flip()



grid.newpage()
# Create layout : nrow = 2, ncol =2
pushViewport(viewport(layout = grid.layout(nrow = 7, ncol = 8)))
# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 
print(ggpar(P1,font.xtickslab =8,font.ytickslab =8,font.x = 8,font.y=8,font.title=6,font.legend=6),vp = define_region(row = 1:2, col = 1:2))  


dev.off()

#################################################### Figure 3 ###################################################################################
################## Figure 3A

SigLevel = read.table('Fig.3A.txt',sep='\t',head=T)

SigLevel$Size = as.factor(SigLevel$Size)

SigLevel$Low_Ratio2 = SigLevel$Low_Ratio/100
SigLevel$High_Ratio2 = SigLevel$High_Ratio/100
SigLevel$High_Ratio2 = round(SigLevel$High_Ratio2,2)

SigLevel$Low_Ratio2 = round(SigLevel$Low_Ratio2,2)

SigLevel= SigLevel[order(SigLevel$Cancer),]
SigLevel$Order = paste(SigLevel$RepOrd,SigLevel$Fusion,sep='.')

pdf('SigReccFusions_01232025.pdf')
P1 = ggplot(SigLevel, aes(x=Order, y=Low_Ratio2,fill=Cancer)) + 
  geom_bar(stat = "identity",width=0.8,color='black',size=0.1)+scale_fill_manual(values=c("ACC"="palevioletred4",'BLCA'='skyblue',"CESC"="aquamarine",'CHOL'='blue',"COAD"="blanchedalmond","GBM"="red","KIRP"="plum2","LAML"="grey","LGG"="green","LIHC"="black","OV"="darkolivegreen","PAAD"="orange","PCPG"="cyan", "PRAD"="gold4", "SARC"="purple", "STAD"="darkolivegreen1","TGCT"="magenta3", "THCA"="lightcoral","UCEC"="deeppink","ESCA"="cornflowerblue","THYM"="aquamarine4","LUSC"="green3"))+theme_classic()+theme(legend.key.size=unit(0.1,"cm"),legend.position = 'top')+coord_flip()


P2 = ggplot(SigLevel, aes(x=Order, y=High_Ratio2,fill=Cancer)) + 
  geom_bar(stat = "identity",width=0.8,color='black',size=0.1)+scale_fill_manual(values=c("ACC"="palevioletred4",'BLCA'='skyblue',"CESC"="aquamarine",'CHOL'='blue',"COAD"="blanchedalmond","GBM"="red","KIRP"="plum2","LAML"="grey","LGG"="green","LIHC"="black","OV"="darkolivegreen","PAAD"="orange","PCPG"="cyan", "PRAD"="gold4", "SARC"="purple", "STAD"="darkolivegreen1","TGCT"="magenta3", "THCA"="lightcoral","UCEC"="deeppink","ESCA"="cornflowerblue","THYM"="aquamarine4","LUSC"="green3"))+theme_classic()+theme(legend.key.size=unit(0.1,"cm"),legend.position = 'top')+coord_flip()

  
grid.newpage()
# Create layout : nrow = 2, ncol =2
pushViewport(viewport(layout = grid.layout(nrow = 8, ncol = 6)))
# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 
print(ggpar(P1,font.xtickslab =7,font.ytickslab =6,font.x = 7,font.y=7,font.title=6,font.legend=6),vp = define_region(row = 1:6, col = 1:3))  
print(ggpar(P2,font.xtickslab =7,font.ytickslab =6,font.x = 7,font.y=7,font.title=6,font.legend=6,),vp = define_region(row = 1:6, col =4:6))  

dev.off()



################## Figure 3B


dataFusions = read.table('Fig.3B.txt',sep='\t',head=T)

pdf('Fig3B.pdf')


P2 = ggscatter(dataFusions, x="Cancer", y="DrugTarget",size ="Size",color = "Label",facet.by='EXTENDClass',palette = c('OffLabel'='salmon','OnLabel'='mediumvioletred'))+theme_linedraw()+theme(axis.text.x=element_text(angle=90,size=7,vjust=1,hjust=1),legend.key.size=unit(0.1,"cm"),legend.position="top")

grid.newpage()
# Create layout : nrow = 2, ncol =2
pushViewport(viewport(layout = grid.layout(nrow =5, ncol = 6)))
# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 

print(ggpar(P2,font.xtickslab =6,font.ytickslab =6,font.x = 8,font.y=8,font.legend=9),vp = define_region(row = 1:3, col = 1:6))

dev.off()




##################################################### Figure 4 ##################################################################################
################## Figure 4A

SenSig = read.table('Fig.4A.txt',sep='\t',head=T)

labes1 = SenSig$Cancer[which(SenSig$Sig == "Sig")]

SenSig$Size = as.factor(SenSig$Size)
pdf('Fig.4A.pdf')


P1 = ggscatter(SenSig, y="MeanLow", x="MeanHigh",size ="Size",color = "Sig",palette = c('Sig'='red','NS'='blue'),alpha=0.6,,label = "Cancer",font.label = c(6,"plain","black"),label.select = labes1,repel=TRUE)+theme_classic()+theme(axis.text.x=element_text(angle=0,size=7,vjust=1,hjust=1),legend.key.size=unit(0.1,"cm"),legend.position="top")+geom_abline(slope=1,intercept=0)##+geom_hline(yintercept=0.45)+geom_vline(xintercept =0.4)



grid.newpage()
# Create layout : nrow = 2, ncol =2
pushViewport(viewport(layout = grid.layout(nrow =5, ncol = 5)))
# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 

print(ggpar(P1,font.xtickslab =c("black",10),font.ytickslab =c("black",10),font.x = 10,font.y=10,font.legend=9,xlab="High EXTEND Senescence",ylab="Low EXTEND Senescence",xlim=c(0,0.6),ylim=c(0,0.6)),vp = define_region(row = 1:2, col = 1:2))

dev.off()



################## Figure 4B ############

data = read.table('Fig.4B.txt',sep='\t',head=T)


source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

my_comparisons1 = list(c("High","Low"))


pdf('Fig.4B.pdf')
  
P1 = ggplot(data = combdata, 
         aes(x = EXTENDclass, y = SenescenceScores, fill = "dodgerblue4")) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0),fill="dodgerblue4",color="darkblue",alpha=0.5) +
   #geom_point(aes(y = Senescence),color="blue",fill = "white",shape = 21,stroke = .1,  
            # position = position_jitter(width = .15), size = .2)+  
  geom_boxplot(width = .15, outlier.shape = NA,fill="white",color="black",size=0.2) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Spectral") +
  scale_fill_brewer(palette = "Spectral") +  
  theme_classic()+theme(axis.text.x=element_text(size=8,vjust=1,hjust=1),legend.position="top")+stat_compare_means(comparisons = my_comparisons1, method= "t.test",size=3)# coord_flip() + # flip or not
 
 grid.newpage()
# Create layout : nrow = 2, ncol =2
pushViewport(viewport(layout = grid.layout(nrow = 7, ncol = 6)))
# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 


print(ggpar(P1,font.xtickslab =c("black",10),font.ytickslab =c("black",10),font.x = 10,font.y=10,ylab="Senescence Scores",xlab="EXTEND Class"),vp = define_region(row = 1:2, col = 1:2))

dev.off()


################## Figure 4C, 4D and 4G (Single cell GBM) ############

mydata = read.table('Fig.4C-D-G.txt',sep='\t',head=T)


pdf('Fig.4C-D-G.pdf')
P1= FeaturePlot(mydata, features = "EXTEND",min.cutoff = 0.2, max.cutoff = 0.5)+ scale_colour_gradientn(colours = rev(brewer.pal(n = 11, name = "RdBu")))+theme_classic()+theme(axis.text.x=element_text(angle= 0,size=8,vjust=1,hjust=1),legend.key.height=unit(0.3,"cm"),legend.key.width=unit(0.5,"cm"),legend.position="top")

P2 = FeaturePlot(mydata, features = "SenescenceScores",min.cutoff = 0.2, max.cutoff = 0.5)+ scale_colour_gradientn(colours = rev(brewer.pal(n = 11, name = "RdBu")))+theme_classic()+theme(axis.text.x=element_text(angle= 0,size=8,vjust=1,hjust=1),legend.key.height=unit(0.3,"cm"),legend.key.width=unit(0.5,"cm"),legend.position="top")


P3 = DimPlot(mydata, reduction = 'umap',group.by='Phase',cols = c('NonCycling'='#FF68A1','G2_M'='#00A9FF','G1_S'='#00BFC4'))+theme_classic()+theme(axis.text.x=element_text(angle= 0,size=8,vjust=1,hjust=1),legend.key.height=unit(0.5,"cm"),legend.key.width=unit(0.5,"cm"),legend.position="top")

grid.newpage()
# Create layout : nrow = 2, ncol =2
pushViewport(viewport(layout = grid.layout(nrow = 5, ncol = 3)))
# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 

print(ggpar(P1,font.xtickslab =8,font.ytickslab =8,font.x = 8,font.y=8, font.legend=8),vp = define_region(row = 1:2, col = 1))
print(ggpar(P2,font.xtickslab =8,font.ytickslab =8,font.x = 8,font.y=8, font.legend=8),vp = define_region(row = 1:2, col = 2))
print(ggpar(P3,font.xtickslab =8,font.ytickslab =8,font.x = 8,font.y=8, font.legend=8),vp = define_region(row = 1:2, col = 3))

dev.off()


################## Figure 4E, 4F and 4H (Single cell GBM) ############

mydata = read.table('Fig.4E-F-H.txt',sep='\t',head=T)


pdf('Fig.4E-F-H.pdf')
P1= FeaturePlot(mydata, features = "EXTEND",min.cutoff = 0, max.cutoff = 0.57,pt.size=0.5)+ scale_colour_gradientn(colours = rev(brewer.pal(n = 11, name = "RdBu")))+theme_classic()+theme(axis.text.x=element_text(angle= 0,size=8,vjust=1,hjust=1),legend.key.height=unit(0.3,"cm"),legend.key.width=unit(0.5,"cm"),legend.position="top")

P2 = FeaturePlot(mydata, features = "SenescenceScores",min.cutoff = 0.1, max.cutoff = 0.57,pt.size=0.5)+ scale_colour_gradientn(colours = rev(brewer.pal(n = 11, name = "RdBu")))+theme_classic()+theme(axis.text.x=element_text(angle= 0,size=8,vjust=1,hjust=1),legend.key.height=unit(0.3,"cm"),legend.key.width=unit(0.5,"cm"),legend.position="top")


P3 = DimPlot(mydata, reduction = 'umap',group.by='Phase',cols = c('NonCycling'='#FF68A1','G2_M'='#00A9FF','G1_S'='#00BFC4'),pt.size=0.5)+theme_classic()+theme(axis.text.x=element_text(angle= 0,size=8,vjust=1,hjust=1),legend.key.height=unit(0.5,"cm"),legend.key.width=unit(0.5,"cm"),legend.position="top")

grid.newpage()
# Create layout : nrow = 2, ncol =2
pushViewport(viewport(layout = grid.layout(nrow = 5, ncol = 3)))
# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 

print(ggpar(P1,font.xtickslab =8,font.ytickslab =8,font.x = 8,font.y=8, font.legend=8),vp = define_region(row = 1:2, col = 1))
print(ggpar(P2,font.xtickslab =8,font.ytickslab =8,font.x = 8,font.y=8, font.legend=8),vp = define_region(row = 1:2, col = 2))
print(ggpar(P3,font.xtickslab =8,font.ytickslab =8,font.x = 8,font.y=8, font.legend=8),vp = define_region(row = 1:2, col = 3))

dev.off()





##########################################  Figure 5 ##########################################################################



combMeta = read.table('5A.txt',sep='\t',head=T)
combMeta2 = read.table('5B.txt',sep='\t',head=T)

pdf('Fig5.pdf')

P1 = ggplot(combMeta, aes(x = PosX, y =PosY ,color=EXTENDScores))+geom_point(size=0.4)+scale_color_gradientn(colors = c("yellow","yellow","yellow", "red","red", "red"), limits = c(0, 1),breaks = c(0,0.5,1))+theme_classic()+theme(axis.text.x=element_text(size=8,vjust=1,hjust=1),legend.key.height=unit(0.2,"cm"),legend.position="top")

P2 = ggplot(combMeta, aes(x = PosX, y =PosY ,color=SenescenceScores))+geom_point(size=0.4)+scale_color_gradientn(colors = c("yellow","yellow","red", "red"), limits = c(0, 0.2),breaks = c(0,0.1,0.2))+theme_classic()+theme(axis.text.x=element_text(size=8,vjust=1,hjust=1),legend.key.height=unit(0.2,"cm"),legend.position="top")

P3 =ggscatter(combMeta, y="PosY", x="PosX",size =0.4,color = "Phase",palette=c("G1"="yellow","S"="red","G2M"="hotpink"),alpha=0.9)+theme_classic()+theme(legend.key.size= unit(0.4,"cm"),legend.key.width = unit(0.2,"cm"),legend.position = "top")



P4 = ggplot(combMeta2, aes(x = PosX, y =PosY ,color=EXTENDScores))+geom_point(size=0.4)+scale_color_gradientn(colors = c("yellow","yellow","yellow","red", "red","red", "red"), limits = c(0, 1),breaks = c(0,0.5,1))+theme_classic()+theme(axis.text.x=element_text(size=8,vjust=1,hjust=1),legend.key.height=unit(0.2,"cm"),legend.position="top")



P5 = ggplot(combMeta2, aes(x = PosX, y =PosY ,color=SenescenceScores))+geom_point(size=0.4)+scale_color_gradientn(colors = c("yellow","yellow","red", "red"), limits = c(0, 0.2),breaks = c(0,0.1,0.2))+theme_classic()+theme(axis.text.x=element_text(size=8,vjust=1,hjust=1),legend.key.height=unit(0.2,"cm"),legend.position="top")



P6 =ggscatter(combMeta2, y="PosY", x="PosX",size =0.4,color = "Phase",palette=c("G1"="yellow","S"="red","G2M"="hotpink"),alpha=0.9)+theme_classic()+theme(legend.key.size= unit(0.4,"cm"),legend.key.width = unit(0.2,"cm"),legend.position = "top")



grid.newpage()
# Create layout : nrow = 2, ncol =2
pushViewport(viewport(layout = grid.layout(nrow = 3, ncol =3)))
# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 



print(ggpar(P1,font.xtickslab =8,font.ytickslab =8,font.x = 8,font.y=8, font.legend=8),vp = define_region(row = 1, col = 1))
print(ggpar(P2,font.xtickslab =8,font.ytickslab =8,font.x = 8,font.y=8, font.legend=8),vp = define_region(row = 1, col = 2))
print(ggpar(P3,font.xtickslab =8,font.ytickslab =7,font.x = 8,font.y=8, font.legend=8),vp = define_region(row = 1, col = 3))

print(ggpar(P4,font.xtickslab =8,font.ytickslab =8,font.x = 8,font.y=8, font.legend=8),vp = define_region(row = 2, col = 1))
print(ggpar(P5,font.xtickslab =8,font.ytickslab =8,font.x = 8,font.y=8, font.legend=8),vp = define_region(row = 2, col = 2))
print(ggpar(P6,font.xtickslab =8,font.ytickslab =8,font.x = 8,font.y=8, font.legend=8),vp = define_region(row = 2, col = 3))


dev.off()


##################################### Figure 6 #######################################################
#################### Figure 6A ####################

comball = read.table('Fig.6A.txt_112023.txt',sep='\t',head=T)

comball = comball[order(comball$SampleSize),]
comball$Size = NA

for(i in 1:nrow(comball)){
if(comball$SampleSize[i]<100){
comball$Size[i]=1}else if(comball$SampleSize[i]>100 & comball$SampleSize[i]<200){
comball$Size[i]=2}else if(comball$SampleSize[i]>200 & comball$SampleSize[i]<300){
comball$Size[i]=3}else if(comball$SampleSize[i]>300){
comball$Size[i]=4}}

pdf('Fig.6A.pdf')
   
P3 = ggplot(comball, aes(y = Cancer, x = EXTENDclass,color=MeanSen))+geom_point(size=comball$Size,shape=19,stroke=0.7)+scale_color_gradient2(midpoint=0.3, low="aquamarine",mid="blue",high="red")+theme_linedraw()+theme(axis.text.x=element_text(angle= 40,size=8,vjust=1,hjust=1),legend.key.height=unit(0.3,"cm"),legend.key.width=unit(0.4,"cm"),legend.position="bottom")



grid.newpage()
# Create layout : nrow = 2, ncol =2
pushViewport(viewport(layout = grid.layout(nrow = 10, ncol = 13)))
# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 

print(ggpar(P3,font.xtickslab =8,font.ytickslab =8,font.x = 8,font.y=8, font.legend=8),vp = define_region(row = 3:8, col = 1:2))

dev.off()



################## Figure 6B ###################################
combdata2 = read.table('Fig.5B.txt',sep='\t',head=T)

LungOnly = combdata2[which(combdata2$Tissue == "Lung"),]
EsophagusOnly = combdata2[which(combdata2$Tissue == "Esophagus"),]
SkinOnly = combdata2[which(combdata2$Tissue == "Skin"),]
PituitaryOnly = combdata2[which(combdata2$Tissue == "Brain"),]


pdf('Fig.6B.pdf',onefile=FALSE)

cols2 <- c("Low" = "skyblue", "High" = "darkblue")

P1 =  ggviolin(LungOnly,x="AgeGroup",y="SenescenceGenes",size =  0.4, width = 0.9,draw_quantiles=c(0.5),outlier.shape=NA,color = "white",fill="EXTENDclass",order = c("AYA","Adult","OA"))+scale_fill_manual(values = cols2, name = "EXTENDclass")+theme_classic()+theme(axis.text.x=element_text(angle=0,size=7,vjust=0,hjust=0.5),legend.position = "top",legend.key.size= unit(0.3,"cm"),legend.key.width = unit(0.3,"cm"),legend.title = element_text(size=7),legend.text =element_text(size=6))+stat_compare_means(aes(group = EXTENDclass,label = paste0("p = ", ..p.format..)),method = "t.test",size=2.5)
	
P2 =  ggviolin(EsophagusOnly,x="AgeGroup",y="SenescenceGenes",size =  0.4, width = 0.9,draw_quantiles=c(0.5),outlier.shape=NA,color = "white",fill="EXTENDclass",order = c("AYA","Adult","OA"))+scale_fill_manual(values = cols2, name = "EXTENDclass")+theme_classic()+theme(axis.text.x=element_text(angle=0,size=7,vjust=0,hjust=0.5),legend.position = "top",legend.key.size= unit(0.3,"cm"),legend.key.width = unit(0.3,"cm"),legend.title = element_text(size=7),legend.text =element_text(size=6))+stat_compare_means(aes(group = EXTENDclass,label = paste0("p = ", ..p.format..)),method = "t.test",size=2.5)
	

P3 =  ggviolin(SkinOnly,x="AgeGroup",y="SenescenceGenes",size =  0.4, width = 0.9,draw_quantiles=c(0.5),outlier.shape=NA,color = "white",fill="EXTENDclass",order = c("AYA","Adult","OA"))+scale_fill_manual(values = cols2, name = "EXTENDclass")+theme_classic()+theme(axis.text.x=element_text(angle=0,size=7,vjust=0,hjust=0.5),legend.position = "top",legend.key.size= unit(0.3,"cm"),legend.key.width = unit(0.3,"cm"),legend.title = element_text(size=7),legend.text =element_text(size=6))+stat_compare_means(aes(group = EXTENDclass,label = paste0("p = ", ..p.format..)),method = "t.test",size=2.5)
	
P4 =  ggviolin(PituitaryOnly,x="AgeGroup",y="SenescenceGenes",size =  0.4, width = 0.9,draw_quantiles=c(0.5),outlier.shape=NA,color = "white",fill="EXTENDclass",order = c("AYA","Adult","OA"))+scale_fill_manual(values = cols2, name = "EXTENDclass")+theme_classic()+theme(axis.text.x=element_text(angle=0,size=7,vjust=0,hjust=0.5),legend.position = "top",legend.key.size= unit(0.3,"cm"),legend.key.width = unit(0.3,"cm"),legend.title = element_text(size=7),legend.text =element_text(size=6))+stat_compare_means(aes(group = EXTENDclass,label = paste0("p = ", ..p.format..)),method = "t.test",size=2.5)
	
grid.newpage()
# Create layout : nrow = 3, ncol = 3
pushViewport(viewport(layout = grid.layout(nrow =12, ncol = 4)))
# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 

print(ggpar(P1,font.xtickslab =c("black",0),font.ytickslab =c("black",10),font.x = 10,font.y=10,font.legend=10,xlab="Lung Tissue",ylab="Senescence Score"),vp = define_region(row = 1:3, col = 1))
print(ggpar(P2,font.xtickslab =c("black",0),font.ytickslab =c("black",10),font.x = 10,font.y=10,font.legend=10,xlab="Esophagus Tissue",ylab="Senescence Score"),vp = define_region(row = 1:3, col = 2))
print(ggpar(P3,font.xtickslab =c("black",0),font.ytickslab =c("black",10),font.x = 10,font.y=10,font.legend=10,xlab="Skin Tissue",ylab="Senescence Score"),vp = define_region(row = 4:6, col = 1))
print(ggpar(P4,font.xtickslab =c("black",0),font.ytickslab =c("black",10),font.x = 10,font.y=10,font.legend=10,xlab="Brain Tissue",ylab="Senescence Score"),vp = define_region(row = 4:6, col = 2))

dev.off()


###################### Figure 6F-G #############################


HumanLiver = read.table('Fig.6F.txt',sep='\t',head=T)
HumanHeart = read.table('Fig.6G.txt',sep='\t',head=T)



pdf('Fig_6F_G.pdf')


P1 = ggscatter(HumanLiver, y="EXTENDScores", x="SenescenceScores",size =2.5,shape=21,stroke =0.8,color = "Group",palette = c("S1"="red","S2"="blue","S3"="blue","S4"="blue","S5"="blue","S6"="blue","S7"="green4","S8"="green4","S9"="green4"), add = "reg.line", 
add.params = list(color = "blue", fill = "grey",size=0.2),conf.int = TRUE)+theme_classic()+theme(axis.text.x=element_text(angle=0,size=7,vjust=1,hjust=1),legend.key.size=unit(0.1,"cm"),legend.position="top")+stat_cor(method="spearman",size=3)


P2 = ggscatter(HumanHeart, y="EXTENDScores", x="SenescenceScores",size =2.5,shape=21,stroke =0.8,color = "Group",palette = c("S1"="red","S2"="blue","S3"="blue","S4"="blue","S5"="blue","S6"="blue","S7"="green4"), add = "reg.line", 
add.params = list(color = "blue", fill = "grey",size=0.2),conf.int = TRUE)+theme_classic()+theme(axis.text.x=element_text(angle=0,size=7,vjust=1,hjust=1),legend.key.size=unit(0.1,"cm"),legend.position="top")+stat_cor(method="spearman",size=3)


grid.newpage()
# Create layout : nrow = 2, ncol =2
pushViewport(viewport(layout = grid.layout(nrow =5, ncol = 5)))
# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 

print(ggpar(P1,font.xtickslab =c("black",10),font.ytickslab =c("black",10),font.x = 10,font.y=10,font.legend=9),vp = define_region(row = 1:2, col = 1:2))

print(ggpar(P2,font.xtickslab =c("black",10),font.ytickslab =c("black",10),font.x = 10,font.y=10,font.legend=9),vp = define_region(row = 1:2, col = 3:4))


dev.off()


########################## Figure 7 ########################################################################

############# Figure 7A #######################


comball = read.table('Fig.7A.txt',sep='\t',head=T)
comball = comball[order(comball$Ratio),]
comball$Size = NULL

for(i in 1:nrow(comball)){
if(comball$Ratio[i]<=0.2){
comball$Size[i]=1}else if(comball$Ratio[i]>0.2 & comball$Ratio[i]<=0.4){
comball$Size[i]=2}else if(comball$Ratio[i]>0.4 & comball$Ratio[i]<=0.6){
comball$Size[i]=3}else if(comball$Ratio[i]>0.6){
comball$Size[i]=4}}

comball$Size = as.factor(comball$Size)
comball = comball[order(comball$ImmuneCT),]
comball$Class = paste(comball$EXTENDclass,comball$Significance,sep="_")
subcomb = comball[which(comball$Significance == "Sig"),]

pdf('Fig.7A.pdf')

P2 = ggscatter(subcomb, x="ImmuneCT", y="Cancer",size ="Size",shape=21,stroke = 1,alpha=0.9,color = "Class",palette=c("High_Sig"="red","High_NS"="skyblue","Low_Sig"="blue","Low_NS"="pink"))+theme_linedraw()+theme(axis.text.x=element_text(angle=90,size=7,vjust=1,hjust=1,color="black"),legend.key.size=unit(0.05,"cm"),legend.position="top")


grid.newpage()
# Create layout : nrow = 2, ncol =2
pushViewport(viewport(layout = grid.layout(nrow =12, ncol = 13)))
# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 

print(ggpar(P2,font.xtickslab =c("black",9),font.ytickslab =c("black",10),font.x = 10,font.y=10,font.legend=7),vp = define_region(row = 1:8, col = 1:4))

dev.off()

############# Figure 7B #######################
SenSig = read.table('Fig.7B.txt',sep='\t',head=T)
SenSig = SenSig[complete.cases(SenSig),]

SenSig$log10FDR = -log10(SenSig$FDR)

SenSig = SenSig[order(SenSig$log10FDR),]
SenSig$Size = NULL
SenSig = SenSig[order(SenSig$log10FDR),]
SenSig$Size = NULL

for(i in 1:nrow(SenSig)){
if(SenSig$log10FDR[i] <= 2){
SenSig$Size[i]=1}else if(SenSig$log10FDR[i] > 2 & SenSig$log10FDR[i] <= 5){
 SenSig$Size[i]=2}else if(SenSig$log10FDR[i] > 5){
 SenSig$Size[i]=3}else{
 SenSig$Size[i]=NA}
 }

SenSig$Size = as.factor(SenSig$Size)

SenSigsub = SenSig[which(SenSig$Sig == "Sig"),]
SenSigsub = SenSigsub[order(SenSigsub$Cancer),]



pdf('Fig.7B.pdf')

P2= ggscatter(SenSigsub, x="ImmuneCT", y="Cancer",size ="Size",shape=21,alpha=0.8,stroke=0.8,color="HighClass",palette = c('High'='red','Low'='blue'))+theme_linedraw()+theme(axis.text.x=element_text(angle=35,size=7,vjust=1,hjust=1),legend.key.size=unit(0.1,"cm"),legend.position="top")+stat_cor(method='spearman',size=3)

##+scale_shape_manual(values=c(7,8))
grid.newpage()
# Create layout : nrow = 2, ncol =2
pushViewport(viewport(layout = grid.layout(nrow =10, ncol = 7)))
# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 

print(ggpar(P2,font.xtickslab =c("black",7),font.ytickslab =c("black",9),font.x = 10,font.y=10,font.legend=9),vp = define_region(row = 1:8, col = 1:4))

dev.off()


################################# Figure 8 ##########################################

############## Fig.8A ######################


comballDF = read.table('Fig.8A.txt',sep='\t',head=T)
comballDF$Size = NULL



for(i in 1:nrow(comballDF)){
if(comballDF$Corr[i] <= 0.55){
comballDF$Size[i]=1}else if(comballDF$Corr[i] > 0.55 & comballDF$Corr[i] <= 0.65){
comballDF$Size[i]=2}else if(comballDF$Corr[i] > 0.65){
 comballDF$Size[i]=3}
 }

comballDF$Size = as.factor(comballDF$Size)
#
pdf('Fig.8A.pdf')


P1 = ggscatter(comballDF,y="Cancer", x="Type",shape=21,,size ="Size",stroke=0.8,color = "Sig",palette = c('Sig'='red','NS'='skyblue3'))+theme_linedraw()+theme(axis.text.x=element_text(angle=0,size=7,vjust=1,hjust=1),legend.key.size=unit(0.1,"cm"),legend.position="right")


grid.newpage()
# Create layout : nrow = 2, ncol =2
pushViewport(viewport(layout = grid.layout(nrow =5, ncol = 11)))
# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 

print(ggpar(P1,font.xtickslab =c("black",10),font.ytickslab =c("black",10),font.x = 10,font.y=10,font.legend=9),vp = define_region(row = 1:5, col = 1:3))


dev.off()


############## Fig.8B ##########################

comballC3 = read.table('Fig.8B.txt',sep='\t',head=T)

library(ggsankey)

df <- comballC3 %>%
  make_long(ImmuneCT,EXTENDclass,Senclass,ROSclass,MAPKclass)

pdf('Fig.7B.pdf')
ggplot(df, aes(x = x, 
               next_x = next_x, 
               node = node, 
               next_node = next_node,
               fill = factor(node),
               label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = 1) +
  geom_sankey_label(size = 3.5, color = 1, fill = "white") +
  scale_fill_viridis_d(option = "A", alpha = 0.95) +
  theme_sankey(base_size = 16)
dev.off()
