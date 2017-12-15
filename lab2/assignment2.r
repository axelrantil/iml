library(readxl)
creditscoring <- read_excel("~/Projects/iml/lab2/creditscoring.xls")

creditscoring$good_bad <- as.factor(creditscoring$good_bad)

n=dim(creditscoring)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=creditscoring[id,]

testval=creditscoring[-id,]
n=dim(testval)[1]
id=sample(1:n, floor(n*0.5))
test=testval[id,]
val=testval[-id,]


#install.packages("tree")
library(tree)

fit.dev = tree(good_bad~., data=train, split="deviance")
plot(fit.dev)
text(fit.dev, pretty = 0)
error.train.dev <- summary(fit.dev)$misclass[1] / summary(fit.dev)$misclass[2]

Yfit.dev <- predict(fit.dev, newdata=test, type="class")
tab.dev <- table(test$good_bad, Yfit.dev)

error.test.dev <- 1 - sum(diag(tab.dev))/sum(tab.dev)

fit.gini = tree(good_bad~., data=train, split="gini")
plot(fit.gini)
text(fit.gini, pretty = 0)
error.train.gini <- summary(fit.gini)$misclass[1] / summary(fit.gini)$misclass[2]

Yfit.gini <- predict(fit.gini, newdata=test, type="class")
tab.gini <- table(test$good_bad, Yfit.gini)

error.test.gini <- 1 - sum(diag(tab.gini))/sum(tab.gini)

res <- c(error.train.dev, error.test.dev, error.train.gini, error.test.gini)

res <- matrix(res, nrow=4, ncol=1, dimnames=list(c("Error train deviance", "Error test deviance", "Error train gini", "Error test gini"),c("Error")))

res
# Deviance is a better splitting criterion 

# Task 3

fit = tree(good_bad~., data=train, split="deviance")

trainScore = rep(0,9)
valScore = rep(0,9)

for (i in 2:9){
  prunedTree = prune.tree(fit, best=i)
  predval = predict(prunedTree, newdata=val, type="tree")
  trainScore[i] = deviance(prunedTree)
  valScore[i] = deviance(predval)
}
plot(2:9, trainScore[2:9]/2, type="b", col="red", ylim=c(250,300), ylab="Deviance", xlab="Number of terminal nodes")
points(2:9, valScore[2:9], type="b", col="blue")
legend("bottomleft", c("validation", "train"), col=c("blue", "red"), lty=c(1,1))

best.fit = tree(good_bad~., data=train, split="deviance")
pruned.best.tree = prune.tree(best.fit, best=which.min(valScore[2:9])+1)
plot(pruned.best.tree)
text(pruned.best.tree, pretty = 0) 
# Best tree, depth is 3 and 3 variables used for splitting the data
# These are savings, then duration and lastly history in order of importance which makes sence.

Yfit.best <- predict(pruned.best.tree, newdata=test, type="class")
tab.best <- table(test$good_bad, Yfit.best)

#0.248
error.test <- 1 - sum(diag(tab.best))/sum(tab.best)

# Task 4

#install.packages("e1071")
library(e1071)

fit.nb = naiveBayes(good_bad~., data=train)

Yfit.train.nb = predict(fit.nb, newdata=train)

Yfit.test.nb = predict(fit.nb, newdata=test)

tab.train.nb <- table(train$good_bad, Yfit.train.nb)

tab.test.nb <- table(test$good_bad, Yfit.test.nb)

#Error train: 0.3
error.train.nb <- 1 - sum(diag(tab.train.nb))/sum(tab.train.nb)

#Error test: 0.344
error.test.nb <- 1 - sum(diag(tab.test.nb))/sum(tab.test.nb)

#Worse result than tree

# Task 5
fit.nb = naiveBayes(good_bad~., data=train)

Yfit.train.nb = predict(fit.nb, newdata=train, type="raw")

Yfit.test.nb = predict(fit.nb, newdata=test,type="raw")

pred.train.nb <- ifelse((Yfit.train.nb[,2] / Yfit.train.nb[,1]) > 10, "pred good", "pred bad")

tab.nb.train.loss <- table(train$good_bad, pred.train.nb)

error.nb.train.loss <- 1 - sum(diag(tab.nb.train.loss))/sum(tab.nb.train.loss)

pred.test.nb <- ifelse((Yfit.test.nb[,2] / Yfit.test.nb[,1]) > 10, "pred good", "pred bad")

tab.nb.test.loss <- table(test$good_bad, pred.test.nb)

error.nb.test.loss <- 1 - sum(diag(tab.nb.test.loss))/sum(tab.nb.test.loss)

tab.nb.train.loss
error.nb.train.loss
tab.nb.test.loss
error.nb.test.loss
