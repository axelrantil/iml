states = read.csv("State.csv", header=TRUE, sep=";", dec=",")

states <- states[order(states[,'MET']),]

### Task 2

library(tree)

plot(states[,'MET'],states[,'EX'])

fit = tree(EX~MET, data=states, control = tree.control(48,minsize = 8))

set.seed(12345)
cv.res <- cv.tree(fit)

plot(cv.res$size, cv.res$dev, type="b", col="red")

plot(log(cv.res$k), cv.res$dev, type="b", col="red")

best.fit = prune.tree(fit, k = cv.res$k[which.min(cv.res$dev)], best = cv.res$size[which.min(cv.res$dev)])

plot(best.fit)
text(best.fit)

Yfit <- predict(best.fit)

plot(states[,'MET'],states[,'EX'])
lines(states[,'MET'], Yfit, col="red")

summary(best.fit)


### Task 3
library(boot)

f = function(data, ind){
  data1 = data[ind,]
  res = tree(EX~MET, data=states, control = tree.control(48,minsize = 8))
  best.fit = prune.tree(fit, k = cv.res$k[which.min(cv.res$dev)], best = cv.res$size[which.min(cv.res$dev)])
  return(predict(best.fit))
}

res = boot(states, f, R=1000)


