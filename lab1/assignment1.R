

# Y true value
# Yfit pred
# p probabilities, m antal probabilities
ROC=function(Y, Yfit, p){
  m=length(p) 
  TPR=numeric(m)
  FPR=numeric(m)
  for(i in 1:m){
    t=table(Yfit>p[i], Y)
    TPR[i]= t[2,2]/sum(t[,2])
    FPR[i]= t[2,1]/sum(t[,1])
  }
  return (list(TPR=TPR,FPR=FPR))
}

######### TASK 1 #########

data = read.csv("spambase.csv", header = TRUE, sep=";" , dec = ",")
data[,ncol(data)] = as.factor(data[,ncol(data)])

n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]

######### TASK 2 #########

knearest=function(data,k,newdata) {
  n1=dim(data)[1] ## Number of training
  n2=dim(newdata)[1] ## Number of test
  p=dim(data)[2] ## Number of columns
  Prob=numeric(n2) ## 0 vector of length n2 (number of test)
  X=as.matrix(data[,-p]) ## X
  Xn=as.matrix(newdata[,-p]) ## Y
  X=X/matrix(sqrt(rowSums(X^2)), nrow=n1, ncol=p-1) #Normalized
  
  Xn=Xn/matrix(sqrt(rowSums(Xn^2)), nrow=n2, ncol=p-1) #Normalized
  
  C = X%*%t(Xn) 
  D = matrix(1, nrow(C), ncol(C)) - C #distance
  
  for (i in 1:n2 ){
    kNN <- which.min(D[,i])
    if (k>1){
      for (j in 2:k){
        kNN <- c(kNN, which.min(D[-kNN,i]))
      }
    }
    targets <- data[kNN, p]
    Prob[i] <- sum(targets==1)/k
  }
  return(Prob)
}

######### TASK 3 #########

prob1 <- knearest(train, k = 5, test) # change k = 1 for task 4
pred1 <- round(prob1)
acc1 <- sum(pred1==test[,ncol(test)])/length(pred1)
missclass1 = 1 - acc1
table("pred"=pred1, "true"=train[,ncol(test)])

# 63% for k = 5, 65,2% for k = 1

######### TASK 5 #########

#install.packages("kknn")
library(kknn)

pred2 <- kknn(Spam~., train, test, k=5)
pred2$prob

pred2 <- apply(pred2$prob,1, which.max) - 1

acc2 <- sum(pred2==test[,ncol(test)])/length(pred2)
missclass2 = 1 - acc2

table("pred"=pred2, "true"=train[,ncol(test)])

######### TASK 6 #########

prob1 <- knearest(train, k = 5, test)
prob2 <- kknn(Spam~., train, test, k=5)$prob[,2]

errorMatrix <- 1 - sum(as.numeric(prob1>0.05)==test[,ncol(test)])/length(prob1)
errorMatrix <- c(errorMatrix, 1 - sum(as.numeric(prob2>0.05)==test[,ncol(test)])/length(prob2))

for (pi in seq(0.1, 0.95, by=0.05)){
  errorMatrix <- c(errorMatrix, 1 - sum(as.numeric(prob1>pi)==test[,ncol(test)])/length(prob1))
  errorMatrix <- c(errorMatrix, 1 - sum(as.numeric(prob2>pi)==test[,ncol(test)])/length(prob2))
}

errorMatrix <- matrix(errorMatrix, ncol=2, byrow = TRUE)

colnames(errorMatrix) <- c("knearest", "kknn")
rownames(errorMatrix) <- seq(0.05, 0.95, by=0.05)

errorMatrix

debugonce(ROC)

rocProb1 <- ROC(test[,ncol(test)],prob1, seq(0.05, 0.95, by=0.05))

plot(rocProb1$FPR, rocProb1$TPR, type="l", xlim =c(0,1), ylim=c(0,1), col="red")

rocProb2 <- ROC(test[,ncol(test)],prob2, seq(0, 1, by=0.05))

lines(rocProb2$FPR, rocProb2$TPR, col="green") ## kkNN better

## Add legend



