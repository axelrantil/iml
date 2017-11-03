knearest=function(data,k,newdata) {
  
  n1=dim(data)[1] ## Number of training
  n2=dim(newdata)[1] ## Number of test
  p=dim(data)[2] ## Number of columns
  Prob=numeric(n2) ## 0 vector of length n2 (number of test)
  X=as.matrix(data[,-p]) ## X
  Xn=as.matrix(newdata[-p]) ## Y
  X=X/matrix(sqrt(rowSums(X^2)), nrow=n1, ncol=p-1) #Normalized
  
  Xn=Xn/matrix(sqrt(rowSums(Xn^2)), nrow=n1, ncol=p-1) #Normalized
  
  C = X%*%t(Xn) 
  
  D = matrix(1, dim(C)[1], dim(C)[2]) - C #distance
  
  for (i in 1:n2 ){
    kNN <- which.min(D[,i])
    for (j in 2:k){
      kNN <- c(kNN, which.min(D[-kNN,i]))
    }
    Prob[i] <- mode(X[kNN, ncol])
    
#MISSING: use the computed distance matrix to find 
    #which observations are the nearest neighbors to case #i
#MISSING: derive probability value 'Prob[i]' by using the
    #target values of the nearest neighbors
  }
  return(Prob)
}

ROC=function(Y, Yfit, p){
  m=length(p)
  TPR=numeric(m)
  FPR=numeric(m)
  for(i in 1:m){
    t=table(Yfit>p[i], Y)
    TPR[i]=#insert formula for TPR
    FPR[i]=#insert formula for FPR
  }
  return (list(TPR=TPR,FPR=FPR))
}

data = read.csv("spambase.csv", header = TRUE, sep=";")

data[,ncol(data)] = as.factor(data[,ncol(data)])

n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]

knearest(train, k = 5, test)
