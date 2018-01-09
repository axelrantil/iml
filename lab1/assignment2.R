library(readxl)
machines <- read_excel("machines.xlsx")

x <- machines$Length

hist(x) #Looks like an exponential distribution

set.seed(12345)

expDist <- function(x, theta){
  return(theta*exp(-theta*x))
}

loglikelihood <- function(x, theta){
  prob <- expDist(x, theta)
  return(sum(log(prob)))
}

### Step 2 ###

theta <- seq(0,5,by=0.01)

likelihood <- numeric(length(theta))

for (i in 1:length(theta)){
  likelihood[i] <- loglikelihood(x, theta[i])
}

plot(theta, likelihood, type="l", ylim=c(-200,0))

best_theta <- theta[which.max(likelihood)] # 1.13

### Step 3 ###

theta <- seq(0,5,by=0.01)

likelihood2 <- numeric(length(theta))

for (i in 1:length(theta)){
  likelihood2[i] <- loglikelihood(head(x), theta[i])
}

lines(theta, likelihood2, col="red")

theta[which.max(likelihood2)]

# The reliability is a lot higher for 48 data points 

### Step 4 ###
prior <- function(lambda, theta){
  return (lambda*exp(-lambda*theta))
}

theta <- seq(0,5,by=0.01)

posterior <- numeric(length(theta))
l1 <- numeric(length(theta))
l2 <- numeric(length(theta))

for (i in 1:length(theta)){
  l1[i] <- loglikelihood(x, theta[i])
  l2[i] <- log(prior(lambda=10, theta[i]))
  posterior[i] <- loglikelihood(x, theta[i]) + log(prior(lambda=10, theta[i]))
  #posterior[i] <- log(prod(expDist(x, theta[i]))*prior(lambda=10, theta[i]))
}


plot(theta, posterior, type="l")

theta[which.max(posterior)] 
# 0.91, the theta is smaller now since we have a prior which is larger for all thetas. 
# The prior is larger than the likelihood for all thetas which mean that the prior of theta is smaller. The prior "shifts" the value.

### Step 5 ###

gen <- rexp(50, rate=1.13)

p1 <- hist(gen, breaks=14)
p2 <- hist(x, breaks=14)

plot( p1, col=rgb(0,0,1,1/8), xlim=c(0,6))  # first histogram
plot( p2, col=rgb(1,0,0,1/8), xlim=c(0,6), add=T)

plot(theta, l1, ylim=c(-200, 20), col="red")
points(theta, l2)

points(theta, l1 + l2, col="green")
