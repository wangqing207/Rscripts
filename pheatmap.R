library(pheatmap)
##################
#total = read.delim(file ="total_beta.txt",sep="\t",header = T)
#save(total,file="total.RData")
#index<-grep(".AVG_Beta",colnames(total))
#total_beta<-total[,c(2,index)]

data_12 = read.delim(file ="g1g2_fdr_diff13_delta01.txt",sep="\t",header = T)
g12<-merge(total,data_12,by="TargetID")		 

#可能得去重复基因
#################################画热图########################################
library(pheatmap)
g12=read.delim(file ="g1g2heatmap.txt",sep="\t",header = T)


g12_new = g12[,c(2:21)]

colnames(g12_new)<-gsub(".AVG_Beta","",colnames(g12_new))
g12_new = as.matrix(g12_new)
rownames(g12_new) = g12[,1]

people=c(1:20)
people[c(1:10)]="g5"
people[c(11:20)]="g6"
annotation = data.frame(group=people)
rownames(annotation) = colnames(g12_new)
pdf(file="exp_pheat.pdf")#,height=30,width=22)###注意是否相等，注释行名要和数据列名一一对应
pheatmap(g12_new,cellwidth = 30,file="g3vsg4pheatmap.pdf", cellheight = 3,color = colorRampPalette(c("blue","white","red"))(50),cluster_cols=T,main="pheatmap",clustering_distance_rows = "correlation",show_rownames =F,show_colnames = T)
dev.off()
#cluster_row = FALSE
,annotation=annotation


		 