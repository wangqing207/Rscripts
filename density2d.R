#geom_density_2d(mapping = NULL, data = NULL, stat = "density2d", position = "identity", ..., lineend = "butt", linejoin = "round", linemitre = 1, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)
#p+stat_density2d(geom="tile", aes(fill = ..density..,alpha=..density..), contour=FALSE)+ scale_colour_gradient2(low = c("blue","yellow","red"), space = "Lab",  na.value = "grey50")

library(ggplot2)
pdf("da_density2d.pdf")
p<-ggplot(da1,aes(x=Mean_A,y=Mean_D)) 
p<-p+stat_density2d(geom="tile", aes(fill = ..density..), contour=FALSE)+scale_fill_gradientn(colours = c("blue", "yellow", "red"),space="lab")+geom_abline(slope=1,intercept=0,color="white",linetype =3)
p<-p+theme(panel.grid.major=element_line(colour=NA))
p
dev.off()
############

heat=read.delim(file="heatplot.txt",header=T)
df <- data.frame(x1 =2.5, x2 = 15.0, y1 = 2.5, y2 = 15.0)
p<-ggplot(heat,aes(x=APA,y=TPA),ymax=15,ymin=2.5,xmax=15,xmin=2.5) 
p<-p+stat_density2d(geom="tile", aes(fill = ..density..),n=100,h=10, contour=FALSE)
#n表示网格数，越大表示颜色分布越平滑；h表示密度区间，h越大表示展现的密度越低
p<-p+scale_fill_gradientn(colours = c("blue", "yellow", "red"),space="lab")
p<-p+ geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2),linetype=3, colour = "white", data = df)
p<-p+theme(panel.grid.major=element_line(colour=NA))
p
ggsave(file="density2d.pdf",height=7,width=7)
