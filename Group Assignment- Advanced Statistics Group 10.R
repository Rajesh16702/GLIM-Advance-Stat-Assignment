############### Problem No. 1 ####################
library(MASS)
View(Boston)
help(Boston)
Boston$medvcat<- cut(Boston$medv, c(0,20,30,50), labels=c(1,2,3))
library("psych")
describe(Boston)
multi.hist(Boston)

plot(Boston, pch=21, col="darkblue", cex=0.4)
round(cov(Boston[,1:14]),3)
round(cor(Boston[,1:14]),3)

############### Problem No. 2 ####################
pca.Boston <- cbind(Boston[,1:3],Boston[,5:8],Boston[,10:13])
BostonFit.1 <- prcomp(pca.Boston,scale = T)
summary(BostonFit.1)
plot(BostonFit.1, type='l',mtext(""))
title("Screeplot of Boston data", cex=0.6)

scale.pca.Boston <- scale.default(pca.Boston[,1:11])
summary(scale.pca.Boston)

cor(scale.pca.Boston,BostonFit.1$x)

############### Problem No. 3 ####################
#5 factor analysis:
 Bostonfact5 <- principal(scale.pca.Boston, nfactors=5, rotate="varimax")
 Bostonfact5 
 #6 factor analysis:
 Bostonfact6 <- principal(scale.pca.Boston, nfactors=6, rotate="varimax")
 Bostonfact6
 Bostonfact7 <- principal(scale.pca.Boston, nfactors=7, rotate="varimax")
 Bostonfact7
 # Cmmunality
 BostonCommunalityTable <- rbind(round(Bostonfact5$communality,2),round(Bostonfact6$communality,2),round(Bostonfact7$communality,2))
 BostonCommunalityTable
 
############### Problem No. 4 ####################
 library(MASS) # for Boston data
 library(ggplot2) # for visualisation
 # define medvcat
 Boston$medvcat<-cut(Boston$medv, c(0,20,30,50), labels=c(1,2,3))
 # exclude medv
 Boston.lda.data <- Boston[,c(1:13,15)]
 str((Boston.lda.data))
 
  # visualize the data
 vis1 <- Boston.lda.data
 ggplot(vis1,
        aes(x = factor(""), fill = medvcat) ) +
   geom_bar() +
   coord_polar(theta = "y") +
   scale_x_discrete("") 
# Splitting Data
#Sample Indexes
set.seed(123)
indexes = sample(1:nrow(Boston.lda.data), size=450)
# Split data
trainb = Boston.lda.data[indexes,]
testb = Boston.lda.data[-indexes,]

ldfit.b <- lda(medvcat ~ ., data=trainb)
ldfit.b

trainprdb <- predict(ldfit.b, newdata=trainb)
testprdb <- predict(ldfit.b, newdata=testb)

table(trainprdb$class, trainb$medvcat)
table(testprdb$class, testb$medvcat)

############### Problem No. 5 ####################
# visualize the data
# chad
vis2 <- Boston.lda.data
ggplot(vis1) + geom_bar(aes(chas))
# rad
ggplot(vis2) + geom_bar(aes(rad)) 

# Splitting Data
#Sample Indexes
set.seed(234)

ldfit.c <- lda(chas ~ ., data=trainb)
ldfit.c

trainprdc <- predict(ldfit.c, newdata=trainb)
testprdc <- predict(ldfit.c, newdata=testb)

table(trainprdc$class, trainb$chas)
table(testprdc$class, testb$chas)

set.seed(345)

ldfit.r <- lda(rad ~ ., data=trainb)
ldfit.r

trainprdr <- predict(ldfit.r, newdata=trainb)
testprdr <- predict(ldfit.r, newdata=testb)

table(trainprdr$class, trainb$rad)
table(testprdr$class, testb$rad)
