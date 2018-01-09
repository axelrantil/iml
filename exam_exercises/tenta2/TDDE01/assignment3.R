library(kernlab)

spam <- read.csv("spam.csv")

#rbfkernel <- rbfdot(sigma= 0.05)
set.seed(1234567890)

Cpar <- c(1,10,100)

fit1 <- ksvm(type~., data=spam, kernel="rbfdot", kpar=list(sigma = 0.05), C=1, cross=2)
fit2 <- ksvm(type~., data=spam, kernel="rbfdot", kpar=list(sigma = 0.05), C=10, cross=2)
fit3 <- ksvm(type~., data=spam, kernel="rbfdot", kpar=list(sigma = 0.05), C=100, cross=2)

cross(fit1)
cross(fit2)
cross(fit3)

## If C is too low, we underfit the data, if C is too high, we overfit the data. 
## So the C that yields the lowest error is somewhere in between (not monotone)

### NEURALNET ###

library(neuralnet)
set.seed(1234567890)
Var <- runif(50, 0, 10)
tr <- data.frame(Var, Sin=sin(Var))
tr1 <- tr[1:25,] # Fold 1
tr2 <- tr[26:50,] # Fold 2

layers = c(10)
no_weights <- layers[1] + tail(layers, n=1) + sum(layers) + 1 + sum(c(0, layers) * c(layers,0))

winit <- runif(no_weights, -1, 1)


f <- as.formula("Sin ~ Var")
nn <- neuralnet(f, data=tr1, hidden=layers, threshold = 0.001, startweights = winit)
pred1 <- compute(nn, tr2$Var)
error1 <- mean((tr2$Sin-pred1$net.result)^2)

nn <- neuralnet(f, data=tr2, hidden=layers, threshold = 0.001, startweights = winit)
pred2 <- compute(nn, tr1$Var)
error2 <- mean((tr1$Sin-pred2$net.result)^2)

CV <- (error1 + error2) / 2
