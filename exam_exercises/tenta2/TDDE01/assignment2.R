bank <- read.csv("bank.csv", header=TRUE, sep=";", dec=",")

fit <- glm("Visitors~.",data=bank, family = poisson(link="log"))

pred <- predict(fit, bank)

head(bank)

noonone <- seq(12, 13, by=0.05)

predict(fit, noonone)
