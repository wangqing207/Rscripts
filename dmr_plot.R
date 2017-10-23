d2=tol[tol$CHR==7 &  27142000 <= tol$MAPINFO　&　tol$MAPINFO <=27149500,]
d1=d2[order(-d2$MAPINFO),]
pdf("dmr1.5__.pdf",height=6,width=6)
#par(fig=c(0,1,0,0.6)
par(mfcol=c(2,1),mar=c(0,2,0.5,2))
plot(y=d1[,2],x=d1$MAPINFO,col="#5CACEE",ylim=c(0,1),xlim=c(27142000,27149500),ylab="Methylation",xlab="",cex=0.6, pch=20,xaxt="n",cex.axis=0.6,mgp=c(1,0,0),tcl=-0.2)
for (i in 3:25){
if(i<14){col="#5CACEE"}
else {col="red"}
points(x=d1$MAPINFO,y=d1[,i],col=col,pch=20,cex=0.6)
}
lines(y=c(0,0),x=c(27144595,27147084),col="green",lwd=4,type="l")
text(27146000,0.05,"DMR",col="green",font=2,cex=1)
y1=apply(d1[,2:13],1,mean)
y2=apply(d1[,14:25],1,mean)
sp1=smooth.spline(x=d1$MAPINFO,y=y1, spar=0.6)   #spar调节光滑度，越大越光滑
sp2=smooth.spline(x=d1$MAPINFO,y=y2, spar=0.6)
lines(sp1,col="#5CACEE",type="l",xlab=NULL)
lines(sp2,col="red",type="l",xlab=NULL)
legend("topleft",legend=c("D","A"),col=c("#5CACEE","red"),pch=20,cex=0.6)
sp3=smooth.spline(x=d1$MAPINFO,y=y1-y2,spar=0.6)
par(fig=c(0,1,0,0.35),mar=c(4,2,0,2),new=TRUE)
plot(sp3,ylab="Mean diff",cex.axis=0.6,type="l",xlab="CHR7",xlim=c(27142000,27149500),xaxp=c(27142000,27149500,8),mgp=c(1,0,0),tcl=-0.2)
#xaxp x轴做标刻度数；mgp 标题，坐标名（x，y)距离图的距离
abline(a=-0.15,b=0,col="red")
dev.off()
