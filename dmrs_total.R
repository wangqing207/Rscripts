#从total文件进行dmr分析
library(bumphunter)
total=read.delim(file="total_filter.txt",header=T)
pos=total$MAPINFO
chr=paste("chr",total$CHR,sep="")
cl<- clusterMaker(chr, pos, maxGap = 1500)
X=cbind(rep(1,102),rep(c(0,1),each=51))
y=as.matrix(total[,1:103])  #样本beta值
library(doParallel)
registerDoParallel(cores =12)					 #并行计算，选择合适的进程数
dmrs <- bumphunter(y, X, chr, pos, cl, cutoff=0.05,B=1000)
save(dmrs,file="dmrs14.RData")
names(dmrs)
head(dmrs$table, n=3)

####添加注释信息
dmrs2=dmrs$table
library("TxDb.Hsapiens.UCSC.hg19.knownGene")
library("org.Hs.eg.db")
genes <- annotateTranscripts(TxDb.Hsapiens.UCSC.hg19.knownGene)
tab=matchGenes(dmrs2,genes)
maps=cbind(tab,dmrs2)
write.table(maps,file="dmr_annog1vsg4.txt",sep="\t",col.names=T,row.names=F)
