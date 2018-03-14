# Reading data: Wine_Classification 
# Describing data 
round(cov(Wine_Classification[,2:14]),3)
round(cor(Wine_Classification[,2:14]),3)

# Principal Component Analysis #
Wfit.1 <- prcomp(Wine_Classification[,2:14], scale=T) # Scaling recommended
summary(Wfit.1)
Wfit.1
W1 <- scale.default(Wine_Classification[,2:14])
cor(W1, Wfit.1$x)

plot(Wfit.1, type="l")

# Factor Analysis #
library(psych)

Wfit.5 <- principal(W1, nfactors=5, rotate="varimax")
Wfit.5

Wfit.6 <- principal(W1, nfactors=6, rotate="varimax")
Wfit.6

Wfit.7 <- principal(W1, nfactors=7, rotate="varimax")
Wfit.7
