
###高密度处理散点图1
smoothScatter(x=c(1:30),y=jitter(c(1:30),5),  
nbin=200,colramp=colorRampPalette(c("white","yellow","red")), #nbin 格子数，colramp 密度分布颜色
col="red",nrpoints = 10)  #nrpoints，密度核心点数

library(hexbin)
ab=hexbin(x=rnorm(300,5),y=jitter(rnorm(300,5),5),xbins=50)
plot(ab,col="red")
###### 高密度处理散点图2
dataa=data.frame(x=rnorm(300,5),y=jitter(rnorm(300,5),5))
hexbinplot(x~y,data=dataa,colramp=colorRampPalette(c("blue","white","yellow","red")))


#######3d散点图
library("scatterplot3d")
scatterplot3d(x=c(1:10),y=c(3:12),z=c(5:14),color="red",pch=18,lwd=5,type="h")

##相关系数图
library(corrgram)
corrgram(dataa,lower.panel=panel.pie,upper.panel=panel.pts,text.panel=panel.txt,diag.panel=panel.minmax)