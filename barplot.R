
par(mar=c(15,10,5,4)+0.1)
barplot(cpg$count,col=rep(c("#104E8B","#63B8FF","#458B00"),each=20),xaxt="n",ylim=c(0,30))
text(x=seq(1,72,1.2),y=-0.5, srt = 45, adj = 1, labels =cpg$name ,xpd = T,cex=0.7)
legend("topleft",legend=c("biological_process","cellular_component" ,"molecular_function"),col=c("#104E8B","#63B8FF","#458B00"),fill=c("#104E8B","#63B8FF","#458B00"),cex=0.8)
dev.off()