library(readxl)
spectrum <- read_excel("tecator.xlsx")

set.seed(12345)

### Task 1 ###
plot(spectrum$Moisture, spectrum$Protein)

#Yes, it looks somewhat linear

### Task 2 ###

# 215 observations
n <- sample(1:215, size = round(215/2), replace = FALSE)

train <- spectrum[n,] 
test <- spectrum[-n,]
mse <- numeric(6)

### Task 3 ###
for (i in 1:6){
  coef <- lm(formula = train$Moisture ~ poly(train$Protein,degree=i), data=train)$coefficients
  
  pred <- cbind(rep(1,length(test[,1])), poly(test$Protein,degree=i))
  
  #mse[i] <- mean((predict(model, poly(test$Protein,degree=i))-test$Moisture)^2)
}