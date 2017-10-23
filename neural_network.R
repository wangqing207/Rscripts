#----------------------------load packages-----------------------------------------------
if(!suppressWarnings(require(Rcpp))){install.packages("Rcpp");library(Rcpp)}
if(!suppressWarnings(require(nnet))){install.packages("nnet");library(nnet)}
#----------------------------scale data by C++-------------------------------------------
code <- '
 NumericMatrix scale01(NumericMatrix x, int row, int col){
     NumericMatrix newmatc(row, col);
     for(int i = 0;i < col;i++){
         NumericVector Max(row);
         NumericVector Min(row);
         for(int j = 0;j < row;j++)Max[j] = x(j, i);
         for(int k = 0;k < row;k++)Min[k] = x(k, i);
         double nmax = max(Max);
         double nmin = min(Min);
       for(int h = 0;h < row;h++){
         newmatc(h, i) = (x(h, i) - nmin) / (nmax - nmin);
       }
     }
     return newmatc;
} '
Rcpp::cppFunction(code = code)
#--------------------------import dataset---------------------------------------------
if(class(try(wine, TRUE)) == "try-error"){
 wine <- read.csv("D:/wine数据集.txt", header = FALSE, as.is = FALSE) 
 names(wine) <- c("quality", "fixed", "volatile", "citric", "residual", "chlorides", "free",
                  "total", "density", "PH", "sulphates", "alcohol", "mm", "price")
}
#---------------------------dealwith data--------------------------------------------
wine[which(wine$quality == 1), 1] <- 'A'
wine[which(wine$quality == 2), 1] <- 'B'
wine[which(wine$quality == 3), 1] <- 'C'
wine[, 1] <- as.factor(wine[, 1])
#----------------------------set seed-------------------------------------------------
set.seed(1314)
samp <- sample(1:nrow(wine), 100)  #抽取样本
Inputmat <- as.matrix(wine[samp, 2:14])
colnames(Inputmat) <- NULL
wine[samp, 2:14] <- scale01(Inputmat, NROW(samp), ncol(Inputmat))  #数据标准化
r <- 1 / max(abs(wine[, 2:14]))		# 确定参数rang的变化范围
set.seed(101)
#---------------------------building nnet model--------------------------------------
if(TRUE){
cat("buiding model is begin...\n")
nnet_model <- nnet(quality~., data = wine, size = 5, subset = samp, rang = r, decay = 5e-4, maxit = 200)
}
#---------------------------------------------------------------------------------
summary(nnet_model)
#-----------------------------predict newdata-------------------------------------
newdata <- wine[, 2:14]
pred <- predict(nnet_model, newdata, type = "class")
print(pred[sample(1:nrow(newdata), 10)])
#----------------------------Confusion matrix-------------------------------------
print(table(pred, wine$quality))
#-----------------------------K-S-------------------------------------------------
predata <- data.frame(pred = pred, quality = wine[, 1]) #存放预测值与响应变量
predata <- sapply(predata, as.numeric) # 转换为数值型
right_data <- predata[which(predata[, 1] == predata[, 2]), 1]
rong_data <- predata[which(predata[, 1] != predata[, 2]), 1]
print(ks.test(right_data, rong_data))
#-----------------------------removed object--------------------------------------
rm(samp, Inputmat, r, nnet_model, newdata, pred, predata, right_data, rong_data)


































