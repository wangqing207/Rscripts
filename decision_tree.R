library(rpart)
#CART决策树算法：CART算法是一种二分递归分割技术，把当前样本划分为两个子样本，使得生成的每个非叶子结点都有两个分支。
## rpart.control对树进行一些设置
## xval是10折交叉验证：将数据集分为10组，进行10次拟合，第i次拟合用除了第i组以外的数据训练，用第i组进行预测；目的是减少misclaassification rate
## minsplit是最小分支节点数，这里指大于等于20，那么该节点会继续分划下去，否则停止
## minbucket：叶子节点最小样本数
## maxdepth：树的深度
## cp全称为complexity parameter，指某个点的复杂度，对每一步拆分,模型的拟合优度必须提高的程度
ct <- rpart.control(xval=10, minsplit=20, cp=0.1)

## kyphosis是rpart这个包自带的数据集
## na.action：缺失数据的处理办法，默认为删除因变量缺失的观测而保留自变量缺失的观测。         
## method：树的末端数据类型选择相应的变量分割方法:
## 连续性method=“anova”,离散型method=“class”,计数型method=“poisson”,生存分析型method=“exp”
##因变量y有两列选“poisson”，生存分析型method=“exp”，因变量是要素类型“class”，否则“anova”
## parms 用来设置三个参数:先验概率、损失矩阵、分类纯度的度量方法。anova没有参数；poisson分割有一个参数，先验分布变异系数的比率，
##默认为1；生存分布的参数和poisson一致；对离散型，可以设置先验分布的分布的概率(prior)，损失矩阵(loss)，分类纯度(split）；
##priors必须为正值且和为1，loss必须对角为0且非对角为正数，split可以是gini（基尼系数）或者information（信息增益）
## information ：信息增益有对可取值项目多的属性有所偏好，可用增益率减少这种情况，但增益率倾向于选择分割不均匀的分裂方法
##另一种规则称为基尼指数（Gini Index）。基尼系数在节点类别分布均匀时取最大值1-1/n，在只包含一个类别时取最小值0
##基尼系数规则：选择不纯度减少量(Reduction in impurity)最大的参数进行分割。不纯度减少量是分割前的Gini index减去分割后的Gini index
## cost我觉得是损失矩阵，在剪枝的时候，叶子节点的加权误差与父节点的误差进行比较，考虑损失矩阵的时候，从将“减少-误差”调整为“减少-损失”
##  过度拟合问题(Overfitting)
##  过度拟合问题是对训练数据完全拟合的决策树对新数据的预测能力较低。为了解决这个问题，有两种解决方法。
##第一种方法是前剪枝(prepruning)，即事先设定一个分裂阈值，若分裂得到的信息增益不大于这个阈值，则停止分裂。
##第二种方法是后剪枝（postpruning），首先生成与训练集完全拟合的决策树，然后自下而上地逐层剪枝，如果一个节点的子节点被删除后，决策树的准确度没有降低，那么就将该节点设置为叶节点（基于的原则是Occam剪刀：具有相似效果的两个模型选择较简单的那个）
fit <- rpart(Kyphosis~Age + Number + Start,
	data=kyphosis, method="class",control=ct,
	parms = list(prior = c(0.65,0.35), split = "information"))

## 第一种
par(mfrow=c(1,3))
plot(fit);
text(fit,use.n=T,all=T,cex=0.9)

## 第二种，这种会更漂亮一些
library(rpart.plot)
rpart.plot(fit, branch=1, branch.type=2, type=1, extra=102,
           shadow.col="gray", box.col="green",
           border.col="blue", split.col="red",
           split.cex=1.2, main="Kyphosis决策树")

## rpart包提供了复杂度损失修剪的修剪方法，printcp会告诉分裂到每一层，cp是多少，平均相对误差是多少
## 交叉验证的估计误差（“xerror”列），以及标准误差(“xstd”列)，平均相对误差=xerror±xstd
printcp(fit)

## 通过上面的分析来确定cp的值
## 我们可以用下面的办法选择具有最小xerror的cp的办法：
## prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

fit2 <- prune(fit, cp=0.01)
rpart.plot(fit2, branch=1, branch.type=2, type=1, extra=102,
           shadow.col="gray", box.col="green",
           border.col="blue", split.col="red",
           split.cex=1.2, main="Kyphosis决策树")
#对新数据进行预测。
predict(fit, newdata,type = c("vector", "prob", "class", "matrix"),na.action = na.pass, ...)

#随机森林
library(randomForest)
rf=randomForest(Kyphosis ~ Age + Number + Start, data = kyphosis,importance = TRUE, proximity = FALSE, ntree = 10000)
##randomForset，执行建模，x参数设定自变量数据集，y参数设定因变量数据列，importance设定是否输出因变量在模型中的重要性，
##如果移除某个变量，模型方差增加的比例是它判断变量重要性的标准之一，proximity参数用于设定是否计算模型的临近矩阵，
##ntree用于设定随机森林的树数（后面单独讨论），。
varImpPlot(rf)	  
##预测功能
predict(rf, newdata, type="response", norm.votes=TRUE, predict.all=FALSE, proximity=FALSE, nodes=FALSE,  
        cutoff, ...)  
#Nodes判断是否是终点。Proximity判断是否需要进行近邻测量。predict.all判断是否保留所有的预测器 
#predict.all会输出一个150*150的字符矩阵，代表每一颗树的150个预测值（前面预设了ntree=100）；Nodes输出所有树的节点情况。
		   
		   
		   