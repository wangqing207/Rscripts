library(mime)
library(markdown)
library(beanplot)
library(illuminaio)
library(nor1mix)
library(siggenes)
library(Biobase)
library(BiocGenerics)
library(parallel)
library(multtest)
library(splines)
library(stats)
library(base)
library(Biobase)
library(lattice)
library(reshape)
library(GenomicRanges)
library(IRanges)
library(XVector)
library(Biostrings)
library(foreach)
library(iterators)
library(locfit)
library(minfi)
library(bumphunter)
library(IlluminaHumanMethylationEPICmanifest)
library(IlluminaHumanMethylationEPICanno.ilm10b2.hg19) 

baseDir = getwd() 

######RGset <- read.metharray.exp(file.path(baseDir, "200483200014"))
file.table = read.metharray.sheet(baseDir)  #读取csv工作表
file.table
#RGset = read.450k.exp(target = file.table) #读取整个450k实验的red.idat,grn.idat文件，输出为工作表
#RGset 
exp<-read.metharray.exp(target = file.table)
exp
#pd <- pData(exp)
#pd
GRset.funnorm <- preprocessFunnorm(exp,nPCs=2,sex=NULL,bgCorr = TRUE) 
pheno <- pData(GRset.funnorm)$Sample_Group       #filename
designMatrix <- model.matrix(~ pheno)
library(doParallel)
registerDoParallel(cores =12)
#dmrsfind<-function (X){
#bumphunter(X, design = designMatrix,cutoff = 0.2,maxGap=500, B=1000,type="Beta")
#}
#dmrs <- mclapply(GRset.funnorm,dmrsfind,mc.preschedule = FALSE)
dmrs <-bumphunter(GRset.funnorm,design = designMatrix,cutoff = 0.14,maxGap=500, B=1000,type="Beta")
save(dmrs,file="dmrs5.RData")
names(dmrs)
head(dmrs$table, n=3)


