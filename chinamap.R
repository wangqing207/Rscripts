
library(ggplot2) #绘图函数

library(plyr) #数据合并工具

library(maptools) #地图素材导入

library(Cairo) #图片高清导出

library(RColorBrewer) #该保重存储着一些高质量的地图配色模板可以参考

library(scales) #分割数据
china_map<-readShapePoly("bou2_4p.shp")

x <- china_map@data

xs <- data.frame(id=row.names(x),x)

china_map0<- fortify(china_map)

china_map_data <- join(china_map0, xs, type ="full")

mydata <-read.csv("geshengzhibiao.csv")

china_data<-join(china_map_data,mydata,type="full")

#离散颜色标度分割：

qa <- quantile(na.omit(china_data$zhibiao),c(0,0.2,0.4,0.6,0.8,1.0))

china_data$zhibiao_q<-cut(china_data$zhibiao,qa,labels= c("0-20%","20-40%","40-60%","60-80%","80-100%"),include.lowest = TRUE)

province_city <-read.csv("province.csv")

province_city$zhibao<-round(runif(34,90,100))

ggplot(china_data[china_data$id==3,],aes(long,lat))+

geom_polygon(aes(group=group,fill=zhibiao_q),colour="black",size=0.25)+
geom_text(aes(x = Jd,y = Wd,label = provice), family = "GB1",size=2, data = province_city)+  #
scale_fill_brewer(palette="RdYlGn")+

coord_map("polyconic") +

guides(fill=guide_legend(reverse=TRUE,title=NULL))+

theme(

panel.grid = element_blank(),

panel.background = element_blank(),

axis.text = element_blank(),

axis.ticks = element_blank(),

axis.title = element_blank(),

legend.position = c(0.3,0.25),

legend.background = element_blank(),

legend.text.align=1,

legend.key.width=unit(1,"line"),

legend.key.height=unit(1,"line")

)