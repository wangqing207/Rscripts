pca=princomp(tol,cor=FALSE)#cor=TRUE 使用相关矩阵，否则使用协方差矩阵;样本值变化范围大使用相关矩阵，变化范围小使用协方差矩阵
pc=loadings(pca)
pca1=pca$loadings[,1]
pca2=pca$loadings[,2]

pdf("test_sigpca.pdf")
plot(pca1,pca2,col=rep(c("red","green"),each=12),pch=rep(c(16,17),each=12))
text(pca1,pca2-0.02,labels=colnames(tol),col=rep(c("red","green"),each=12),cex=0.6)
legend("topleft",legend=c("R","A"),col=c("red","green"),pch=c(16,17),cex=0.6)
dev.off()