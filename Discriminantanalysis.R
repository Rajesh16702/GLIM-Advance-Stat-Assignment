library(car)
library(rattle)
library(MASS)
library("gdata")
library("lawstat")
library("psych")
library("Rcmdr")
library("compute.es")
library("effects")
library("ggplot2")
library("multcomp")
library("pastecs")
library("WRS")
library("nlme")
library("biotools")
library("mvoutlier")
library("mvnormtest")


setwd("F:/Sridhar/Courses/Advanced Statistics Course/Modules/Classroom illustrations/Module-V Discriminant Analysis")

statesf<-read.table("Housing.txt", header = TRUE)
attach(statesf)
head(statesf)

location1<-c(rep(1,13),rep(2,13), rep(3,9))
location2<-c(rep(1,13),rep(2,13), rep(3,9))

location1<-factor(location1, levels = c(1:3), labels = c("PA", "MP", "LA"))
statesf<-cbind(location1, statesf)

##### Box's M-Test: Equality of variance-covariance matrix. 
####Pooling can be done if the matrices are the same. Otherwise use qda

boxM(statesf[, 3:5], statesf$location1)

#scatterplotMatrix(statesf[3:5])

region.lda <- lda(location1 ~price+bedrooms+area, data=statesf)
region.lda
region.lda$svd


##### To produce standardized coefficients:

#### First Calcualte within group variances
calcWithinGroupsVariance <- function(variable,groupvariable)
{
  # find out how many values the group variable can take
  groupvariable2 <- as.factor(groupvariable[[1]])
  levels <- levels(groupvariable2)
  numlevels <- length(levels)
  # get the mean and standard deviation for each group:
  numtotal <- 0
  denomtotal <- 0
  for (i in 1:numlevels)
  {
    leveli <- levels[i]
    levelidata <- variable[groupvariable==leveli,]
    levelilength <- length(levelidata)
    # get the standard deviation for group i:
    sdi <- sd(levelidata)
    numi <- (levelilength - 1)*(sdi * sdi)
    denomi <- levelilength
    numtotal <- numtotal + numi
    denomtotal <- denomtotal + denomi
  }
  # calculate the within-groups variance
  Vw <- numtotal / (denomtotal - numlevels)
  return(Vw)
}

  
#### Calculate within group covariance
calcWithinGroupsCovariance <- function(variable1,variable2,groupvariable)
{
  # find out how many values the group variable can take
  groupvariable2 <- as.factor(groupvariable[[1]])
  levels <- levels(groupvariable2)
  numlevels <- length(levels)
  # get the covariance of variable 1 and variable 2 for each group:
  Covw <- 0
  for (i in 1:numlevels)
  {
    leveli <- levels[i]
    levelidata1 <- variable1[groupvariable==leveli,]
    levelidata2 <- variable2[groupvariable==leveli,]
    mean1 <- mean(levelidata1)
    mean2 <- mean(levelidata2)
    levelilength <- length(levelidata1)
    # get the covariance for this group:
    term1 <- 0
    for (j in 1:levelilength)
    {
      term1 <- term1 + ((levelidata1[j] - mean1)*(levelidata2[j] - mean2))
    }
    Cov_groupi <- term1 # covariance for this group
    Covw <- Covw + Cov_groupi
  }
  totallength <- nrow(variable1)
  Covw <- Covw / (totallength - numlevels)
  return(Covw)
}

groupStandardise <- function(variables, groupvariable)
{
  # find out how many variables we have
  variables <- as.data.frame(variables)
  numvariables <- length(variables)
  # find the variable names
  variablenames <- colnames(variables)
  # calculate the group-standardised version of each variable
  for (i in 1:numvariables)
  {
    variablei <- variables[i]
    variablei_name <- variablenames[i]
    variablei_Vw <- calcWithinGroupsVariance(variablei, groupvariable)
    variablei_mean <- mean(variablei)
    variablei_new <- (variablei - variablei_mean)/(sqrt(variablei_Vw))
    data_length <- nrow(variablei)
    if (i == 1) { variables_new <- data.frame(row.names=seq(1,data_length)) }
    variables_new["variablei_name"] <- variablei_new
  }
  return(variables_new)
}

    
groupstandardisedconcentrations <- groupStandardise(statesf[3:5], statesf[1])


#Prediction
region.lda.values <- predict(region.lda)
ldahist(data = region.lda.values$x[,1], g=location1)
plot(region.lda.values$x[,1],region.lda.values$x[,2]) # make a scatterplot
text(region.lda.values$x[,1],region.lda.values$x[,2],location1,cex=0.7,pos=4,col="red") # add labels

df<-predict(region.lda,statesf[,c(3,4,5)]) ### Predicting for test data#####
statesf<-cbind(statesf,df)

table(statesf$location,statesf$class)


### Classification
region1.lda <- lda(location1 ~price+bedrooms+area, data=statesf, na.action="na.omit", CV=TRUE)
region1.lda
ct <- table(statesf$location1, region1.lda$class)
diag(prop.table(ct, 1))
# total percent correct
sum(diag(prop.table(ct)))

df<-predict(region.lda,statesf[,c(3,4,5)]) ### Predicting for test data#####
statesf<-cbind(statesf,df)

table(statesf$location,statesf$class)


###################
##Book Data set
##################
setwd("F:/Sridhar/Courses/Advanced Statistics Course/SPSS_ANOVA Results")
book1<-read.table("Bookmarketing.txt", header = TRUE)
plot(book1[,-1],col=as.factor(book1[,2]))
booktrain=subset(book1,book1$id<=1000)
booktest=subset(book1,book1$id>1000)

#### Pooled COvariance####
nonbuyer=subset(booktrain,booktrain$purchase<0)
buyer=subset(booktrain,booktrain$purchase>0)
singlecov=(916*cov(nonbuyer)+82*cov(buyer))/998
singlecov

####Running LDA and Classifying points in the test data set####
book.lda=lda(booktrain[,c(3,4)],grouping=booktrain[,2])
book.lda$prior
book.lda$means
book.lda
predict(book.lda,c(5,2)) ## Predicitng for any point here 5 months and 2 purchases###
df<-predict(book.lda,booktest[,c(3,4)]) ### Predicting for test data#####
booktest<-cbind(booktest,df)

table(booktest$purchase,booktest$class)