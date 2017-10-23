glms=function(x){
datas=cbind(data3[,1:6],x)
name=paste("group~gender+age+height+weight+BMI+",colnames(datas)[7],sep="")
fac=as.formula(as.character(name))
summary(glm(fac,datas,family="binomial"))$ coefficients[2,4]
}
ss=apply(data3[,5:ncol(data3)],2,glms)
