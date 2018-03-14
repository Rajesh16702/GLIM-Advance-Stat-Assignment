###Step1: Include the following library packages: 
  
library(lawstat)
library(psych)
library(car)
library(MASS)
library(Rcmdr)
library(ggplot2)
library(graphics)

#Step 2: Load the data file into R

setwd("F:/Sridhar/Data Sets/t/Two-way ANOVA")
df<-read.table("sensexr.txt",header=TRUE)


#Step 3: Clearly identify the factors in the data

df$incdec1<-factor(df$incdec, labels=c("decrease","increase"))
df$frimon1<-factor(df$frimon,labels=c("Monday", "Friday")) 

#Step 4: Descriptive summary of the data and plots:
  
table(df$incdec1,df$frimon1)
aggregate(df[, 3], list(df$incdec1,df$frimon1), mean)

with(df,interaction.plot(incdec1,frimon1,return, fun=mean, type="b",legend = TRUE, xlab="Decrease or Increase", ylab="Monday or Friday",  main="Interaction Plot"))

par(mfrow=c(1,2))
plot(return ~ incdec1 + frimon1, data=df)


##Step 5: 
  
####Assumption 1 Testing: Levene's Test for equal variances
leveneTest(df$return, df$incdec1)
leveneTest(df$return, df$frimon1)
leveneTest(df$return, interaction(df$incdec1, df$frimon1))   ###(not required generally)


###Test of Normality
#Check the histograms 
  
increase<-subset(df$return,df$incdec1=="increase")
decrease<-subset(df$return,df$incdec1=="decrease")
friday<-subset(df$return,df$frimon1=="Friday")
monday<-subset(df$return,df$frimon1=="Monday")

hist(increase)
hist(decrease)
hist(friday)
hist(monday)

str(df)

#Shapiro-Wilk normality tests by increase decrease
cat("Normality p-values by Factor incdec1: ")
for (i in unique(factor(df$incdec1))){
  cat(shapiro.test(df[df$incdec1==i, ]$return)$p.value," ")
}
cat("Normality p-values by Factor frimon1: ")

#Shapiro-Wilk normality tests by friday monday
for (i in unique(factor(df$frimon1))){
  cat(shapiro.test(df[df$frimon1==i, ]$return)$p.value," ")
}


  
###### Step 6: ANOVA Test: It is a two-way ANOVA test:
  
  fit<-lm(return~incdec1+frimon1+incdec1*frimon1,data=df)

anova(fit)

