
#Loading data into R

library(gdata)
library(lawstat)
library(psych)
library(Rcmdr)
library(car)
library(compute.es)
library(effects)
library(ggplot2)
library(multcomp)
library(pastecs)
library(WRS)
library(mvnormtest)



getwd()
setwd("F:/Sridhar/Courses/Advanced Statistics Course/SPSS_ANOVA Results")
df<-read.table("adintensity.txt",header=TRUE) #Reading a text file

# Use Import Dataset to read .csv file
# To be shown in class

#Checking the contents of the data

ls() # lists list objects in the working environment
names(df) # list the variables in the data set adintensity

str(df) #list the structure of adintensity

#Inform Price level and adintensity are two categorical variables

df$pricelevel<-as.factor(df$pricelevel)
df$adintsty<-as.factor(df$adintsty)



dim(df) # dimensions of an adintensity

class(df) # class of adintensity (numeric, matrix, data frame, etc)

df # print mydata

head(df, n=5) # print first 10 rows of mydata

tail(df, n=5) # print last 5 rows of mydata


tapply(df$sales,list(df$pricelevel, df$adintsty), mean)
tapply(df$sales,list(df$pricelevel, df$adintsty), sd)
#ploting Box Plots

boxplot(sales~adintsty, data=df, notch=FALSE, 
        col=(c("gold","darkgreen","red","blue")),
        main="Adintensity vs Sales", xlab="Advertisement Intensity",ylab="Sales")


boxplot(sales~pricelevel, data=df, notch=FALSE, 
        col=(c("gold","darkgreen","red","blue")),
        main="Pricelevel vs Sales", xlab="Price Level",ylab="Sales")

interaction.plot(df$pricelevel,df$adintsty,df$sales)

#Tests for assumptions


# Test of Equal Variances: Levene's Test

#with(df, levene.test(sales, as.factor(adintsty), location="mean"))

#with(df, levene.test(sales, as.factor(pricelevel), center="mean"))

leveneTest(df$sales, df$pricelevel)
leveneTest(df$sales, df$adintsty)
leveneTest(df$sales, interaction(df$pricelevel, df$adintsty))   ###(not required generally)


#####################################################
#Normalty Tests
#####################################################

#Tests of normality Using Kolmogorov-Smirnov Test: ONe Sample KS Test
by(df$sales, df$adintsty, ks.test,"pnorm")
by(df$sales, df$pricelevel, ks.test,"pnorm")
ks.test(df$sales, "pnorm")

#Tests of normality Using Shapiro-Wilk's Test: 

by(df$sales, df$adintsty, shapiro.test)
by(df$sales, df$pricelevel, shapiro.test)

by(df$sales, interaction(df$pricelevel,df$adintsty), shapiro.test)

# One-Way ANOVA Analysis
anova(lm(sales~adintsty, df)) # Mean sales differs across levels of intensity
anova(lm(sales~pricelevel, df)) # Mean sales differs across pricelevel

# Two-way ANOVA without interaction
anova(lm(sales~pricelevel+adintsty, df))

# Two-Way ANOVA Analysis with interaction

df.mod<-lm(sales~pricelevel+adintsty+pricelevel:adintsty, df)
anova(df.mod)

#Determine Residuals and Predictions

summary(fm1<-aov(sales~pricelevel+adintsty+pricelevel:adintsty,df))

coef(aov(sales~pricelevel+adintsty+pricelevel:adintsty,df))

###Tukey HSD test
TukeyHSD(fm1, "pricelevel", ordered = TRUE)
plot(TukeyHSD(fm1, "pricelevel"))

TukeyHSD(fm1, "adintsty", ordered = TRUE)
plot(TukeyHSD(fm1, "adintsty"))

###################################################
####Accounting for the size of the stores -ANCOVA
###################################################
#####################################################
#Independence of Covariate and Independent Variable
#####################################################


sdd<-aov(size~adintsty,data=df)
anova(sdd)

sdd1<-aov(size~pricelevel,data=df)
anova(sdd1)

df.modfull<-lm(sales~size+pricelevel+adintsty+pricelevel:adintsty, df)
df.modrest<-lm(sales~size, df)
ancovva1<-anova(df.modfull,df.modrest)
print(ancovva1)
df.modrest<-lm(sales~size, df)
anova(df.modrest)

ancovvafull<-anova(df.modfull)
print(ancovvafull)



####Aside 
#######Testing the residuals for normality
df$resids<-residuals(ancovvafull)


#Perform a Shapiro-Wilk test for normality of residuals
shapiro.test(df$resids)