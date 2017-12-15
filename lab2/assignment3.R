states = read.csv("State.csv", header=TRUE, sep=";", dec=",")

states <- states[order(states[,'MET']),]

### Task 2

library(tree)

plot(states$MET,states$EX)

fit = tree(EX~MET, data=states, 
           control = tree.control(nobs = dim(states)[1], minsize = 8))

set.seed(12345)
cv.res <- cv.tree(fit)

plot(cv.res$size, cv.res$dev, type="b", col="red")

plot(log(cv.res$k), cv.res$dev, type="b", col="red")

best.fit = prune.tree(fit, k = cv.res$k[which.min(cv.res$dev)], best = cv.res$size[which.min(cv.res$dev)])

plot(best.fit)
text(best.fit)

Yfit <- predict(best.fit, states)

plot(states[,'MET'],states[,'EX'])
lines(states[,'MET'], Yfit, col="red")


### Task 3
library(boot)

f1 = function(data, ind){
  data1 = data[ind,]
  res = tree(EX~MET, data=data1, control = tree.control(48,minsize = 8))
  best.fit = prune.tree(res, k = 5302.652, best = 3)
  return(predict(best.fit, newdata=states))
}

res.nparam = boot(states, f1, R=1000)

plot(res.nparam)

e1 <- envelope(res.nparam)

plot(states[,'MET'],states[,'EX'],ylab="EX", xlab="MET")
lines(states[,'MET'], Yfit, col="red")
lines(states[,'MET'],e1$point[1,], col="yellow")
lines(states[,'MET'],e1$point[2,], col="yellow")

### Task 4

rng = function(data, mle){
  data1 <- data.frame(EX=data$EX, MET=data$MET)
  head(data1)
  n <- length(data$EX)
  data1$EX = rnorm(n, predict(mle, newdata=data1), sd = sd(summary(mle)$residuals))
  return(data1)
}

res = tree(EX~MET, data=states, control = tree.control(48,minsize = 8))
best.fit = prune.tree(res, k = 5302.652, best = 3)

f2 = function(data){
  res = tree(EX~MET, data=data, control = tree.control(48,minsize = 8))
  best.fit = prune.tree(res, k = 5302.652, best = 3)
  return(predict(best.fit, newdata=states))
}

res.param = boot(states, statistic=f2, R=1000, mle=best.fit, ran.gen=rng, sim="parametric")

e2 <- envelope(res.param)

lines(states[,'MET'],e2$point[1,], col="blue")
lines(states[,'MET'],e2$point[2,], col="blue")

hist(summary(best.fit)$residuals) # looks like an exponential distribution
