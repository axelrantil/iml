

spectra = read.csv("NIRSpectra.csv", header=TRUE, sep=";", dec=",")

label = spectra$Viscosity

spectra$Viscosity = c()

pcd = prcomp(spectra)

lambda = pcd$sdev^2

sprintf("%2.3f", lambda/sum(lambda) * 100)

screeplot(pcd)

plot(pcd$x[,1], pcd$x[,2], main="PC1 & PC2")

# Yes there are some unusual (outliers) diesel fuels. 

U = pcd$rotation

plot(U[,1], xlab="Variable", ylab="PC1 loadings")

plot(U[,2], xlab="Variable", ylab="PC2 loadings")

# The last ten variables explains most of PC2

#install.packages("fastICA")
set.seed(12345)
library(fastICA)

ica = fastICA(spectra, 2, alg.typ="parallel", fun="logcosh", alpha=1, method="R", row.norm = FALSE, maxit = 200, tol = 0.0001, verbose = TRUE)

Wtic = ica$K %*% ica$W

# K is the loadings from original data to the PCA and the W is the loading from PCA to ICA. Wtic is the transformation from original data directly to ICA

# Loadings
plot(Wtic[,1])
plot(Wtic[,2])

# Command below projects the original data Spectra (all variables) into the ICA space.
icaproj = as.matrix(spectra) %*% as.matrix(Wtic)

# Commands below produces same plots (only rescaled)
plot(icaproj[,1], icaproj[,2])
plot(ica$S[,1], ica$S[,2])

#install.packages("pls")

library(pls)
set.seed(12345)
pcr_model <- pcr(label~., data = spectra, scale = TRUE, validation = "CV")

summary(pcr_model)

plot(pcr_model)

validationplot(pcr_model, val.type="RMSEP")

which.min(pcr_model$validation$PRESS)

