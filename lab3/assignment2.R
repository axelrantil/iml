#install.packages("neuralnet")
library(neuralnet)
set.seed(1234567890)
Var <- runif(50, 0, 10)
trva <- data.frame(Var, Sin=sin(Var))
tr <- trva[1:25,] # Training
va <- trva[26:50,] # Validation
# Random initialization of the weights in the interval [-1, 1]
winit <- runif(10, -1, 1)

mse <- numeric(10)
for(i in 1:10) {
  f <- as.formula("Sin ~ Var")
  nn <- neuralnet(f, data=tr, threshold = 5/1000, linear.output=FALSE)
  print(nn)
  nn.fit <- lm(nn, tr)
  summary(nn)
  pred <- predict(nn.fit, va)
  mse[i] <- sum(1/25 * (pred-va$Sin)^2)
}

plot(mse)

plot(nn <- neuralnet(# Your code here))
# Plot of the predictions (black dots) and the data (red dots)
plot(prediction(nn)$rep1)
points(trva, col = "red")