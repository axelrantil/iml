bank <- read.csv("bank.csv", header=TRUE, sep=";", dec=",")

fitlm = lm("Visitors~Time", data=bank)

fit <- glm("Visitors~Time", data=bank, family = poisson(link="log"))

head(bank)

noonone <- seq(12, 13, by=0.05)

targets = predict(fit, data.frame(Time=noonone), type="response")

data2 = data.frame(Visitors = targets, Time = noonone)

plot(data2$Time, data2$Visitors)

library(boot)

rng=function(data, mle){
  data1=data
  n=length(data$Time)
  pred = predict(mle, data, type="response")
  targ = rpois(n, pred)
  data1$Visitors = targ
  return(data1)
}

f1=function(indata){
  fit <- glm("Visitors~Time",data=indata, family = poisson(link="log"))
  return(predict(fit, newdata=data2, type="response"))
}

res=boot(data=data2, statistic=f1, mle=fit, R=1000, ran.gen=rng, sim="parametric")

summary(res)

env2=envelope(res)

lines(data2$Time, env2$point[2,], col="red") # Confidence bands
lines(data2$Time, env2$point[1,], col="red")
lines(data2$Time, env2$overall[2,], col="chartreuse4") # Prediction bands
lines(data2$Time, env2$overall[1,], col="chartreuse4")
