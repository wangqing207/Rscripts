library(ape)
d=dist(t(tol),method="euclidean")
fit <- hclust(d, method="ward.D")
tree=as.phylo(fit)
# tree$edge.length[96]=150 更改某分支的长度
# tree$edge.length[1]=150
mypal = c("#1B676B",  "#C44D58")
clus5 = cutree(fit, 2) #为分支标签配色，并分为两大类
pdf("hclust.pdf",height=8,width=8)
plot(ddd,type="fan",tip.color=mypal[clus5],col="red",cex=0.6, use.edge.length = T) #type="fan"圆形,"unrooted"枯树型,"cladogram"分支型,"radial"射线型
dev.off()


