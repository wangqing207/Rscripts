library(ggplot2)

site[,6]="color"
for (i in 1:nrow(site)){
if (site$P.Value[i]<0.05 & site$Beta.Difference[i] > 0.14){
site[i,6]="hyper"
}
else if(site$P.Value[i]<0.05 & site$Beta.Difference[i] <  -0.14){
site[i,6]="hypo"
}
else{
site[i,6]="no"
}
}
colnames(site)[6]="threshold"
p<-ggplot(site,aes(x=Beta.Difference,y=-log10(P.Value),colour=threshold))
p<-p+xlab("Beta.Difference")+ylab("-log10(P.Value)")+scale_color_manual(values =c("red","green", "black"))
p<-p+geom_point(pch=20)
p
ggsave(file="volcano.pdf",height=8,width=8)
