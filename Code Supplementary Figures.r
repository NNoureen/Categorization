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
############################ SUPPLEMENTARY FIGURES #################################
 

##################################################################################################################
################### Supplementary Figure 1 ######################################

################### Fig.S1A


Female_Ratio = read.table('Fig.S1A.txt',sep='\t',head=T)

pdf('Fig.S1A.pdf')

P1 = ggscatter(Female_Ratio, x="RatioFH", y="RatioFL",size ="Size",stroke = 0.6,color = "Significance",palette = c('Sig'='red','NotSig'='skyblue'),alpha=0.6,label = "Cancer",label.select = labes,repel=TRUE,font.label = c(7,"plain","black"))+theme_classic()+theme(axis.text.x=element_text(angle=0,size=7,vjust=0,hjust=1),legend.key.size=unit(0.05,"cm"),legend.position="top")+geom_abline(slope=1,intercept=0,linetype="dashed")


grid.newpage()
# Create layout : nrow = 2, ncol =2
pushViewport(viewport(layout = grid.layout(nrow =10, ncol = 10)))
# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 

print(ggpar(P1,font.xtickslab =c("black",10),font.ytickslab =c("black",10),font.x = 10,font.y=10,font.legend=10,xlab="High EXTEND Female Ratio",ylab="Low EXTEND Female Ratio"),vp = define_region(row = 1:4, col = 1:4))


dev.off()

################### Fig.S1B

Age_Ratio = read.table('Fig.S1B.txt',sep='\t',head=T)

labes = Age_Ratio$Cancer[which(Age_Ratio$Significance == "Sig")]

pdf('Fig.S1B.pdf')

P1 = ggscatter(Age_Ratio, x="Ratio_OA_High", y="Ratio_OA_Low",size ="Size",stroke = 0.6,color = "Significance",palette = c('Sig'='red','NotSig'='skyblue'),alpha=0.6,label = "Cancer",label.select = labes,repel=TRUE,font.label = c(7,"plain","black"))+theme_classic()+theme(axis.text.x=element_text(angle=0,size=7,vjust=0,hjust=1),legend.key.size=unit(0.05,"cm"),legend.position="top")+geom_abline(slope=1,intercept=0,linetype="dashed")

grid.newpage()
# Create layout : nrow = 2, ncol =2
pushViewport(viewport(layout = grid.layout(nrow =10, ncol = 10)))
# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 

print(ggpar(P1,font.xtickslab =c("black",10),font.ytickslab =c("black",10),font.x = 10,font.y=10,font.legend=10,xlab="High EXTEND OA Ratio",ylab="Low EXTEND OA Ratio"),vp = define_region(row = 1:4, col = 1:4))

dev.off()

############### Fig.S1C

Stage_Ratio = read.table('Fig.S1C.txt',sep='\t',head=T)

labes = Stage_Ratio$Cancer[which(Stage_Ratio$Significance == "Sig")]

pdf('Fig.S1C.pdf')

P1 = ggscatter(Stage_Ratio, x="Ratio_LS_High", y="Ratio_LS_Low",size ="Size",stroke = 0.6,color = "Significance",palette = c('Sig'='red','NotSig'='skyblue'),alpha=0.6,label = "Cancer",label.select = labes,repel=TRUE,font.label = c(7,"plain","black"))+theme_classic()+theme(axis.text.x=element_text(angle=0,size=7,vjust=0,hjust=1),legend.key.size=unit(0.05,"cm"),legend.position="top")+geom_abline(slope=1,intercept=0,linetype="dashed")

grid.newpage()
# Create layout : nrow = 2, ncol =2
pushViewport(viewport(layout = grid.layout(nrow =10, ncol = 10)))
# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 

print(ggpar(P1,font.xtickslab =c("black",10),font.ytickslab =c("black",10),font.x = 10,font.y=10,font.legend=10,xlab="High EXTEND Low Stage Ratio",ylab="Low EXTEND Low Stage Ratio"),vp = define_region(row = 1:4, col = 1:4))


dev.off()


############### Supp.Fig1C 

combinedAll = read.table('Fig.S1C.txt',sep='\t',head=T)

combinedAll$Size = as.factor(combinedAll$Size)
combinedAll = combinedAll[order(combinedAll$Cancer),]

pdf('Fig.S1C.pdf')

P = ggscatter(combinedAll, x="Cancer", y="TumorStage",size ="Size",shape = 4,stroke = 0.8,color = "Significane",palette = c('NS'='azure4','Sig'='red'))+theme_linedraw()+theme(axis.text.x=element_text(angle=35,size=7,vjust=1,hjust=1,color="black"),legend.key.size=unit(0.05,"cm"),legend.position="top")

grid.newpage()
# Create layout : nrow = 2, ncol =2
pushViewport(viewport(layout = grid.layout(nrow =10, ncol = 10)))
# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 

print(ggpar(P,font.xtickslab =c("black",9),font.ytickslab =c("black",10),font.x = 10,font.y=10,font.legend=9),vp = define_region(row = 1:3, col = 1:8))

dev.off()


############### Supp.Fig1D


HR_Results = read.table('Fig.S1D.txt',sep='\t',head=T)

HR_Results$Significance = NULL
HR_Results$Significance = ifelse(HR_Results$p.value< 0.55,"Sig",'NS')


cols2 <- c("NS" = "azure4",  "Sig" = "red")

HR_Results$HR = log(HR_Results$HR)
HR_Results$LowerInterval = log(HR_Results$LowerInterval)
HR_Results$UpperInterval = log(HR_Results$UpperInterval)


pdf('Fig.S1D.pdf')

  plot2 <- ggplot(data=HR_Results, aes(x=fct_reorder(Cancer,HR,.desc=F), y=HR,ymin=LowerInterval, ymax=UpperInterval),color=Significance) +
  geom_point(size=1,position=position_dodge(1),) +scale_fill_manual(values = cols2)+scale_color_manual(values = cols2)+
  geom_errorbar(width=0.2,size=0.1,position=position_dodge(1))+
        geom_hline(yintercept=0, lty=2,color="red",size=0.2) + xlab("Cancer") + ylab("Hazard Ratio") +
        theme_classic()+  # use a white background
		theme(axis.text.x=element_text(angle=45,size=7,vjust=1,hjust=1),legend.key.size= unit(0.3,"cm"),legend.key.width = unit(0.3,"cm"),legend.position = "bottom",axis.text.y = element_blank(), axis.title.y = element_blank())
  
grid.newpage()
# Create layout : nrow = 2, ncol =2
pushViewport(viewport(layout = grid.layout(nrow = 11, ncol = 11)))
# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 
print(ggpar(plot2,font.xtickslab =c("black",7),font.ytickslab =c("black",10),font.x = 10,font.y=0),vp = define_region(row = 1:3, col = 1:7))  

dev.off()

################### Supplementary Figure 2 ######################################

AS_Scores_Pancan = read.table('Fig.S2.txt',sep='\t',head=T)

labes3 = AS_Scores_Pancan$Cancer[which(AS_Scores_Pancan$Sig_LOH == "Sig")]


pdf('Fig.S2.pdf')


P3 = ggscatter(AS_Scores_Pancan, x="Mean_HighLOH", y="Mean_LowLOH",size ="Size",shape = 21,stroke = 0.8,color = "Sig_LOH",palette = c('NS'='azure4','Sig'='red'),label="Cancer",label.select= labes3,repel=T,font.label=c("black",5))+theme_classic()+theme(axis.text.x=element_text(angle=0,size=7,vjust=1,hjust=1,color="black"),legend.key.size=unit(0.05,"cm"),legend.position="top")+geom_abline(slope=1,intercept=0)


grid.newpage()
# Create layout : nrow = 2, ncol =2
pushViewport(viewport(layout = grid.layout(nrow =10, ncol = 10)))
# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 

print(ggpar(P3,font.xtickslab =c("black",10),font.ytickslab =c("black",10),font.x = 10,font.y=10,font.legend=9,xlab="High EXTEND LOH",ylab="Low EXTEND LOH"),vp = define_region(row = 5:8, col = 1:4))

dev.off()


################### Supplementary Figure 3 ######################################


######## Supp.Fig3A


TCGA_TM_Fusions = read.table('Fig.S3A.txt',sep='\t',head=T)


pdf('Fig.S3A.pdf')
P1 = ggplot(TCGA_TM_Fusions, aes(x=Cancer , y=FusionFreq,fill=EXTENDclass)) + geom_bar(stat = "identity",size=0.3,width=0.8, position=position_dodge())+scale_fill_manual(values=c("High"="red","Low"="blue"))+theme_classic()+theme(axis.text.x=element_text(angle=35,size=7,vjust=1,hjust=1),legend.position = "top",legend.key.size=unit(0.4,"cm"))#+coord_flip()


  
grid.newpage()
# Create layout : nrow = 2, ncol =2
pushViewport(viewport(layout = grid.layout(nrow = 10, ncol = 10)))
# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 
print(ggpar(P1,font.xtickslab =8,font.ytickslab =8,font.x = 9,font.y=9,font.legend=8,ylab ="Fusion Frequency"),vp = define_region(row = 1:3, col = 1:10))  

dev.off()


############## Supp.Fig3B

RecFusionsFreqTotal = read.table('Supp.Fig3D.txt',sep='\t',head=T)

pdf('Supp.Fig3D.pdf')

P1 = ggplot(RecFusionsFreqTotal, aes(x=Cancer , y=RecFusionsNo,fill=factor(EXTENDclass,levels = c("Low","High")))) + geom_bar(stat = "identity",size=0.1,width=0.7)+geom_text(aes(label=RecFusionsNo), position=position_dodge(width=0.9),hjust=0.35, vjust=-0.5,size=2)+scale_fill_manual(values=c("High"="hotpink","Low"="skyblue"))+theme_classic()+theme(axis.text.x=element_text(angle=35,size=7,vjust=1,hjust=1),legend.position = "top",legend.key.size=unit(0.4,"cm"))#+coord_flip()


  
grid.newpage()
# Create layout : nrow = 2, ncol =2
pushViewport(viewport(layout = grid.layout(nrow = 10, ncol = 11)))
# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 
print(ggpar(P1,font.xtickslab =8,font.ytickslab =8,font.x = 9,font.y=9,font.legend=8,ylab ="Fusion Frequency"),vp = define_region(row = 1:3, col = 1:7))  

dev.off()


################### Supplementary Figure 4 ######################################

######## Fig.S4A

subdata = read.table('Fig.S4A.txt',sep='\t',head=T)

pdf('Fig.S4A.pdf',onefile=FALSE)


P4 = ggscatter(subdata, x = "EXTENDScores", y = "SenescenceScores",color = "Mechanism",palette = c("TERT_Rearrangement"="maroon","TERT_High"="pink","MYCN_Amp"="red"),size = 2,add = "reg.line",  # Add regressin line
add.params = list(color = "black", fill = "grey",size=0.5), # Customize reg. line
conf.int = TRUE # Add confidence interval
)+stat_cor(method="spearman",size=3)



grid.newpage()
# Create layout : nrow = 3, ncol = 3
pushViewport(viewport(layout = grid.layout(nrow = 2, ncol = 2)))
# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 

print(ggpar(P4,font.xtickslab =8,font.ytickslab =8,font.x = 8,font.y=8,xlab="EXTEND Scores",ylab ="Senescence Signature"),vp = define_region(row = 2, col = 2))

dev.off()



######## Fig.S4B 

combdata = read.table('Fig.S4B.txt',sep='\t',head=T)


source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

my_comparisons1 = list(c("High","Low"))



colpa = c("Low_A_Ganglia"="blue","Low_BiliaryTract"="blue","Low_Bone"="blue","Low_Breast"="blue","Low_Cervix"="blue","Low_CNS"="blue","Low_Endometrium"="blue","Low_Fibroblast" ="skyblue","Low_Haematpoetic&Lymphoid"="blue","Low_Kidney"="blue","Low_LargeIntestine"="blue","Low_Liver"="blue","Low_Lung"="blue","Low_Oesophagus"="blue","Low_Ovary"="blue","Low_Pancreas"="blue","Low_Pleura"="blue","Low_Prostate"="blue","Low_SalivaryGland"="blue","Low_Skin"="blue","Low_SmallIntestine"="blue","Low_SoftTissue"="blue","Low_Stomach"="blue","Low_Thyroid"="blue","Low_UpperAeroDigestiveTract"="blue","Low_UrinaryTract"="blue","High_A_Ganglia"="red","High_BiliaryTract"="red","High_Bone"="red","High_Breast"="red","High_Cervix"="red","High_CNS"="red","High_Endometrium"="red","High_Fibroblast" ="red","High_Haematpoetic&Lymphoid"="pink","High_Kidney"="red","High_LargeIntestine"="red","High_Liver"="red","High_Lung"="red","High_Oesophagus"="red","High_Ovary"="red","High_Pancreas"="red","High_Pleura"="red","High_Prostate"="red","High_SalivaryGland"="red","High_Skin"="red","High_SmallIntestine"="red","High_SoftTissue"="red","High_Stomach"="red","High_Thyroid"="red","High_UpperAeroDigestiveTract"="red","High_UrinaryTract"="red")


pdf('Fig.S4B.pdf')
  
P2 = ggscatter(combdata, y="SenescenceScores",x="EXTENDScores",size =1,color = "colorTiss",palette = colpa, add = "reg.line",
add.params = list(color = "blue", fill = "grey",size=0.2),conf.int = TRUE)+ stat_cor(method = "spearman",size=3)  +theme_classic()+theme(legend.key.size= unit(0.4,"cm"),legend.key.width = unit(0.2,"cm"),legend.position = "none")
   
 
  ##$#coord_flip()+
grid.newpage()
# Create layout : nrow = 2, ncol =2
pushViewport(viewport(layout = grid.layout(nrow = 8, ncol = 6)))
# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 

print(ggpar(P2,font.xtickslab =c("black",10),font.ytickslab =c("black",10),font.x = 10,font.y=10,ylab="Senescence Scores",xlab="EXTEND Class"),vp = define_region(row = 1:2, col = 1:2))
dev.off()


######################### Supplementary Figure 5 #############################################################


############## Supp.Fig.5A-B

comball1 = read.table('Fig.S5A.txt',sep='\t',head=T)
comball1 = read.table('Fig.S5B.txt',sep='\t',head=T)


pdf('Fig.S5A-B.pdf')
  
P1 = ggplot(data = comball1, 
         aes(x = EXTENDclass, y = SenescenceScores, fill = "dodgerblue4")) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0),fill="dodgerblue4",color="darkblue",alpha=0.5) +
   #geom_point(aes(y = Senescence),color="blue",fill = "white",shape = 21,stroke = .1,  
            # position = position_jitter(width = .15), size = .2)+  
  geom_boxplot(width = .15, outlier.shape = NA,fill="white",color="black",size=0.2) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Spectral") +
  scale_fill_brewer(palette = "Spectral") +  
  theme_classic()+theme(axis.text.x=element_text(size=8,vjust=1,hjust=1),legend.position="none")+stat_compare_means(comparisons = my_comparisons1, method= "t.test",size=3)# coord_flip() + # flip or not
  
  
P2 = ggplot(data = comball2, 
         aes(x = EXTENDclass, y = SenescenceScores, fill = "dodgerblue4")) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0),fill="dodgerblue4",color="darkblue",alpha=0.5) +
   #geom_point(aes(y = Senescence),color="blue",fill = "white",shape = 21,stroke = .1,  
            # position = position_jitter(width = .15), size = .2)+  
  geom_boxplot(width = .15, outlier.shape = NA,fill="white",color="black",size=0.2) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Spectral") +
  scale_fill_brewer(palette = "Spectral") +  
  theme_classic()+theme(axis.text.x=element_text(size=8,vjust=1,hjust=1),legend.position="none")+stat_compare_means(comparisons = my_comparisons1, method= "t.test",size=3)# coord_flip() + # flip or not

 
  ##$#coord_flip()+
grid.newpage()
# Create layout : nrow = 2, ncol =2
pushViewport(viewport(layout = grid.layout(nrow = 7, ncol = 6)))
# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 


print(ggpar(P1,font.xtickslab =c("black",10),font.ytickslab =c("black",10),font.x = 10,font.y=10,ylab="Senescence Scores",xlab="EXTEND Class"),vp = define_region(row = 1:2, col = 1:2))

print(ggpar(P2,font.xtickslab =c("black",10),font.ytickslab =c("black",10),font.x = 10,font.y=10,ylab="Senescence Scores",xlab="EXTEND Class"),vp = define_region(row = 1:2, col = 3:4))

dev.off()

############## Supp.Fig.5C-D


comball1 = read.table('Fig.S5C.txt',sep='\t',head=T)
comball2 = read.table('Fig.S5D.txt',sep='\t',head=T)


cols2 = c("NonCycling"="skyblue","G2_M"="pink","G1_S" = "hotpink")
cols3 = c("NonCycling"="skyblue","G2_M"="red","G1_S" = "maroon")


my_comparisons1 = list(c("G1_S","G2_M"),c("G1_S","NonCycling"),c("G2_M","NonCycling"))


pdf('Fig.S5C-D.pdf')

P1 = ggplot(data = comball1, 
         aes(x = Phase, y = Senescence, fill = Phase,color=Phase)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0)) +
  geom_point(aes(y = Senescence),
             position = position_jitter(width = .07), size = .1) +
  geom_boxplot(size=0.5, width = .15, outlier.shape = NA,fill="white",color="black") +
  #expand_limits(x = 5.25) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_manual(values = cols3) +
  scale_fill_manual(values = cols3) +
  theme_classic()+theme(axis.text.x=element_text(size=8,vjust=1,hjust=1),legend.position="none")+stat_compare_means(comparisons = my_comparisons1, method= "t.test",size=2.5)# coord_flip() + # flip or not
 

P2 = ggplot(data = comball2, 
         aes(x = Phase, y = Senescence, fill = Phase,color=Phase)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0)) +
  geom_point(aes(y = Senescence),
             position = position_jitter(width = .07), size = .1) +
  geom_boxplot(size=0.5, width = .15, outlier.shape = NA,fill="white",color="black") +
  #expand_limits(x = 5.25) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_manual(values = cols3) +
  scale_fill_manual(values = cols3) +
  theme_classic()+theme(axis.text.x=element_text(size=8,vjust=1,hjust=1),legend.position="none")+stat_compare_means(comparisons = my_comparisons1, method= "t.test",size=2.5)# coord_flip() + # flip or not
 
grid.newpage()
# Create layout : nrow = 2, ncol =2
pushViewport(viewport(layout = grid.layout(nrow = 7, ncol = 6)))
# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 


print(ggpar(P1,font.xtickslab =8,font.ytickslab =8,font.x = 8,font.y=8),vp = define_region(row = 1:2, col = 1:2))
print(ggpar(P2,font.xtickslab =8,font.ytickslab =8,font.x = 8,font.y=8),vp = define_region(row = 1:2, col = 3:4))

dev.off()


###################################### Supplementary Figure 6 #########################################################################################

SenSig1 = read.table('Fig.S6A.txt',sep='\t',head=T)
SenSig2 = read.table('Fig.S6A.txt',sep='\t',head=T)

pdf('Supp.Fig.6A.pdf')


P1 = ggscatter(SenSig1, y="MeanLow", x="MeanHigh",size ="Size",color = "Sig",palette = c('Sig'='red','NS'='blue'),alpha=0.6,,label = "Cancer",font.label = c(6,"plain","black"),label.select = labsel,repel=TRUE)+theme_classic()+theme(axis.text.x=element_text(angle=0,size=7,vjust=1,hjust=1),legend.key.size=unit(0.1,"cm"),legend.position="top")+geom_abline(slope=1,intercept=0)##+geom_hline(yintercept=0.45)+geom_vline(xintercept =0.4)


P2 = ggscatter(SenSig2, y="MeanLow", x="MeanHigh",size ="Size",color = "Sig",palette = c('Sig'='red','NS'='blue'),alpha=0.6,,label = "Cancer",font.label = c(6,"plain","black"),label.select = labsel,repel=TRUE)+theme_classic()+theme(axis.text.x=element_text(angle=0,size=7,vjust=1,hjust=1),legend.key.size=unit(0.1,"cm"),legend.position="top")+geom_abline(slope=1,intercept=0)##+geom_hline(yintercept=0.45)+geom_vline(xintercept =0.4)






grid.newpage()
# Create layout : nrow = 2, ncol =2
pushViewport(viewport(layout = grid.layout(nrow =5, ncol = 5)))
# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 

print(ggpar(P1,font.xtickslab =c("black",10),font.ytickslab =c("black",10),font.x = 10,font.y=10,font.legend=9,xlab="High EXTEND ROS",ylab="Low EXTEND ROS",xlim=c(1.2,1.8),ylim=c(1.2,1.8)),vp = define_region(row = 1:2, col = 1:2))
##
print(ggpar(P2,font.xtickslab =c("black",10),font.ytickslab =c("black",10),font.x = 10,font.y=10,font.legend=9,xlab="High EXTEND MAPK Signalling",ylab="Low EXTEND MAPK Signalling",xlim=c(1,1.35),ylim=c(1,1.35)),vp = define_region(row = 1:2, col = 3:4))
dev.off()


