library(readxl)
machines <- read_excel("~/Projects/iml/lab1/machines.xlsx")

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

likelihood <- loglikelihood(x, theta[1])

for (i in 2:length(theta)){
  likelihood <- c(likelihood, loglikelihood(x, theta[i]))
}

plot(theta, likelihood, type="l", ylim=c(-200,0))

best_theta <- theta[which.max(likelihood)] # 1.13

### Step 3 ###

theta <- seq(0,5,by=0.01)

likelihood2 <- loglikelihood(head(x), theta[1])

for (i in 2:length(theta)){
  likelihood2 <- c(likelihood2, loglikelihood(head(x), theta[i]))
}

lines(theta, likelihood2, col="red")

# The reliability is a lot higher for 48 data points 


### Step 4 ###



# The loss using posterior is used p(theta|x)

### Step 5 ###

gen <- rexp(50, rate=1.13)

par(2)

p1 <- hist(gen)
p2 <- hist(x)

plot( p1, col=rgb(0,0,1,1/8), xlim=c(0,6))  # first histogram
plot( p2, col=rgb(1,0,0,1/8), xlim=c(0,6), add=T)
