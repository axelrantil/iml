library(readxl)
spectrum <- read_excel("tecator.xlsx")

set.seed(12345)

### Task 1 ###
plot(spectrum$Moisture, spectrum$Protein)

#Yes, it looks somewhat linear

### Task 2 ###

#Theory.

### Task 3 ###

# 215 observations
n <- sample(1:215, size = round(215/2), replace = FALSE)

train <- spectrum[n,] 
test <- spectrum[-n,]
maxDegree <- 6
MSETrain <- numeric(maxDegree)
MSETest <- numeric(maxDegree)

for (i in 1:maxDegree){
  model <- lm(formula = Moisture ~ poly(Protein,degree=i, raw=TRUE), data=train)
  MSETrain[i] <- mean((summary(model)$residuals)^2)
  MSETest[i] <- mean((predict.lm(model,test)-test$Moisture)^2)
}

plot(1:maxDegree, MSETrain, col="red", type="l", ylim=c(30,37), ylab="MSE")
lines(1:maxDegree, MSETest, col="green")
legend(x="bottomright", c("Training", "Test"), lty = c(1,1), col=c("red", "green"))

### Task 4 ###

library(MASS)

data <- spectrum[, 2:101]

model <- lm(formula = spectrum$Fat ~ . , data = data)

step <- stepAIC(model, direction = "both", trace=FALSE)

names(step$coefficients)

# 64 variables 

### Task 4 ###
#install.packages("glmnet")

library(glmnet)

ridgeModel <- glmnet(as.matrix(data), spectrum$Fat, alpha=0, family = "gaussian")

plot(ridgeModel, xvar="lambda", label=TRUE)

lassoModel <- glmnet(as.matrix(data), spectrum$Fat, alpha=1, family = "gaussian")

plot(lassoModel, xvar="lambda", label=TRUE)

### Task 5 ###

installed.packages("cvTools")

lassoCV <- cv.glmnet(as.matrix(data), lambda=seq(0,7, by=0.01), type.measure = "mse", spectrum$Fat, alpha=1, family = "gaussian", nfolds = 10)

plot(lassoCV$lambda, lassoCV$cvm)


