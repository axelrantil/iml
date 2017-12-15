#install.packages("neuralnet")
library(neuralnet)
set.seed(1234567890)
Var <- runif(50, 0, 10)
trva <- data.frame(Var, Sin=sin(Var))
tr <- trva[1:25,] # Training
va <- trva[26:50,] # Validation
# Random initialization of the weights in the interval [-1, 1]
winit <- runif(31, -1, 1)


mse <- numeric(10)
for(i in 1:10) {
  f <- as.formula("Sin ~ Var")
  nn <- neuralnet(f, data=tr, hidden=c(10), threshold = i/1000, startweights = winit)
  pred <- compute(nn, va$Var)
  mse[i] <- mean((pred$net.result - va$Sin)^2)
}


plot(1:10, mse, type="b", xlab="Threshold", ylab="MSE on validation data")

best_nn <- neuralnet(f, data=tr, hidden=c(10), threshold = which.min(mse)/1000, startweights = winit)
#plot(best_nn)
best_pred <- compute(best_nn, va$Var)$net.result


plot(seq(0,10, by=0.001), sin(seq(0,10, by=0.001)), type="l", col="yellow", ylab="Sin", xlab="Var")
points(va$Var, best_pred)
points(tr, col = "red") 
legend("bottomright", c("Sinus", "Training", "Val. pred."), col=c("yellow", "red", "black"), lty=c(1,NA,NA), pch=c(NA,1,1))
