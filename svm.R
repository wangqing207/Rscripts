final=dd[,2:12]
x=subset[final,select=-group]
y=final$group
model=svm(x,y)
pred <- predict(model, x, decision.values = TRUE)
d=attr(pred, "decision.values")[1:4,]

# visualize (classes by color, SV by crosses):
plot(cmdscale(dist(iris[,-5])),
col = as.integer(iris[,5]),
pch = c("o","+")[1:150 %in% model$index + 1])
