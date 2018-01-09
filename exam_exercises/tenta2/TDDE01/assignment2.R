bank <- read.csv("bank.csv", header=TRUE, sep=";", dec=",")

fitlm = lm("Visitors~Time", data=bank)

fit <- glm("Visitors~Time", data=bank, family = poisson(link="log"))

head(bank)

plot(bank$Time, exp(0.174 + 0.4*bank$Time), type="l")
points(bank$Time, bank$Visitors)

plot(bank$Time, log(bank$Visitors))


noonone <- seq(12, 13, by=0.05)

data2 = data.frame(Visitors = c(), Time = noonone)

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
  mu <- predict(fit, newdata=data2, type="response")
  mu <- rpois(length(data2$Time), mu)
  #rpois pga prediktions (lägg på brus) mu (från predikt)
  return(mu)
}

res=boot(data=bank, statistic=f1, mle=fit, R=1000, ran.gen=rng, sim="parametric")

env2=envelope(res)

lines(data2$Time, env2$point[2,], col="red") # Confidence bands
lines(data2$Time, env2$point[1,], col="red")
