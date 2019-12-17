library(knitr)
library(markdown)
getwd()
setwd('C://TCSInternals//AmritaSUNY-Buffalo//Courses//BA-1//assignment_1')
## Data
ubank.df<-read.csv("UniversalBank.csv")
## Introduce the case by applying dim() and str().
dim(ubank.df)
#### It has 5000 rows and 14 columns

## Explain all demographic variables i.e. age, experience, and income
str(ubank.df)

#### Data snapshot
head(ubank.df)

## visualizing categorical variable education
### Cross Table
edu<- factor(ubank.df$Education, c(1,2,3),labels = c('Undergraduates','Graduate', 'Professional'))
edu_Tab<- table(edu)
addmargins( edu_Tab)

### Cross Table with Percent Data
addmargins(round(100*prop.table(edu_Tab),digits=0))

### Bar chart
barplot(edu_Tab, xlab='Education Level',ylab='Frequency',main="Education Data Distribution",
         col=c('olivedrab2', 'orange1','purple2'), ylim = c(0,2500))
#### Bar char correctly concides with data represented in cross table

## Prepare the scatter matrix between Age, Experience, Income, and CCAvg
### getting subset of data
library(dplyr)
aeic.df<-select ( ubank.df, c(Age, Experience, Income, CCAvg ))
head(aeic.df)

plot(density(aeic.df$Age), main = 'Density plot of Age')
plot(density(aeic.df$Experience), main = 'Density plot of Experience')
plot(density(aeic.df$Income), main = 'Density plot of Income')
plot(density(aeic.df$CCAvg), main = 'Density plot of CCAGV')

#pairs(~Age+Experience+Income+CCAvg, data=ubank.df)

library(GGally)
library(tidyverse)

help("ggpairs")

ggpairs(aeic.df)

## Boxplot of The Income Variable 
boxplot(ubank.df$Income, horizontal = TRUE, axes = TRUE, staplewex = 1)
text(x=fivenum(ubank.df$Income), labels =fivenum(ubank.df$Income), y=1.25)

### for drawing box plot
min(ubank.df$Income)
max(ubank.df$Income)
help()
quantile(ubank.df$Income)

## (1)  P(CC = 1 | Loan = 1) (the proportion of credit card holders among the loan acceptors)
pLoan <-which(ubank.df$Personal.Loan == 1)
### ppLoan this has all the indices where values ==1 thus n[y=1] is calculated
sum(ubank.df$CreditCard[pLoan] ==1)/length(pLoan)
### this ratio of n[cc = 1 INTERSECT Loan = 1] / n[Loan==1]

## (2)  P(Online = 1 | Loan = 1)
sum(ubank.df$Online[pLoan] ==1)/length(pLoan)
### this ratio of n[Online ==1 INTERSECT Loan = 1] / n[Loan==1]

## (3) P(Loan = 1) (the proportion of loan acceptors)
length(pLoan)/dim(ubank.df)[1]

## (4) P(CC = 1 / Loan = 0)
pLoanZero <-which(ubank.df$Personal.Loan == 0)
sum(ubank.df$CreditCard[pLoanZero] ==1)/length(pLoanZero)

## (5) P(Online = 1 / Loan = 0)
sum(ubank.df$Online[pLoanZero] ==1)/length(pLoanZero)

## (6) P(Loan = 0)
length(pLoanZero)/dim(ubank.df)[1]



