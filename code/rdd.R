# Sample code for Econometrics II at Osaka University
# By Shuhei Kitamura at OSIPP

## import packages
library(wooldridge)
library(stargazer)
library(compareGroups)
library(mmdata)
library(AER)
library(doBy)
library(rdd)
library(rdrobust)
library(ggplot2)

## clear all variables
rm(list=ls())


##### 1. mlda #####

## import data
rd_mlda <- mmdata::rd_mlda
rd_mlda <- subset(rd_mlda,(is.na(all)==FALSE))

## add variables
rd_mlda$agecell2 <- rd_mlda$agecell^2

rd_mlda$age <- rd_mlda$agecell - 21
rd_mlda$age2 <- rd_mlda$age^2

rd_mlda$over21 <- ifelse(rd_mlda$agecell>=21, 1, 0)

rd_mlda$over_agecell <- rd_mlda$over21*rd_mlda$agecell

rd_mlda$over_age <- rd_mlda$over21*rd_mlda$age
rd_mlda$over_age2 <- rd_mlda$over_age^2

## run a regression
reg1 <- lm(all ~ over21 + agecell, data=rd_mlda)



## plot a figure
rd_mlda$pred_all <- fitted(reg1) 

ggplot(data=rd_mlda,aes(x=agecell,y=all))+
  geom_point() +
  geom_line(aes(x=agecell,y=pred_all)) +
  geom_vline(xintercept=21, linetype="dotted")


## add a quadratic term
reg2 <- lm(all ~ over21 + agecell + agecell2, data=rd_mlda)

## allow change in slope
reg3 <- lm(all ~ over21 + agecell + over_agecell, data=rd_mlda) # wrong model
reg4 <- lm(all ~ over21 + age + over_age, data=rd_mlda) # age = agecell - 21

## both
reg5 <- lm(all ~ over21 + age + age2 + over_age + over_age2, data=rd_mlda)


## plot a figure
rd_mlda$pred_all_q <- fitted(reg5)

ggplot(data=rd_mlda,aes(x=agecell,y=all))+
  geom_point() +
  geom_line(aes(x=agecell,y=pred_all_q)) +
  geom_line(aes(x=agecell,y=pred_all),color="gray") +
  geom_vline(xintercept=21, linetype="dotted")


## limit sample
rd_mlda_limit <- subset(rd_mlda, (agecell >= 20 & agecell <= 22))

reg1 <- lm(all ~ over21 + agecell, data=rd_mlda)
reg2 <- lm(all ~ over21 + age + age2 + over_age + over_age2, data=rd_mlda)
reg3 <- lm(all ~ over21 + agecell, data=rd_mlda_limit)
reg4 <- lm(all ~ over21 + age + age2 + over_age + over_age2, data=rd_mlda_limit)


## optimal bandwidth
IKbandwidth(rd_mlda$agecell, rd_mlda$all, cutpoint=21)
#IKbandwidth(rd_mlda$age, rd_mlda$all, cutpoint=0)


## internal vs. motor vehicle accidents (mva)
reg1 <- lm(internal ~ over21 + age + age2 + over_age + over_age2, data=rd_mlda)
reg2 <- lm(mva ~ over21 + age + age2 + over_age + over_age2, data=rd_mlda)



## plot figures
rd_mlda$pred_internal <- fitted(reg1)
rd_mlda$pred_mva <- fitted(reg2)

# internal
ggplot(data=rd_mlda,aes(x=agecell,y=internal))+
  geom_point() +
  geom_line(aes(x=agecell,y=pred_internal)) +
  geom_vline(xintercept = 21, linetype="dotted") +
  labs(title="",x="age",y="death rate for internal causes (per 100,000)")

# mva
ggplot(data=rd_mlda,aes(x=agecell,y=mva))+
  geom_point() +
  geom_line(aes(x=agecell,y=pred_mva)) +
  geom_vline(xintercept = 21, linetype="dotted") +
  labs(title="",x="age",y="death rate for motor vehicle accidents (per 100,000)")




#### 2. Myerson (2014) ####
## set working directory
#setwd("...")

## import data
turkey <- read.csv("turkey.csv")

## compare means
res <- compareGroups(T ~ Y, data=turkey)
createTable(res)

## OLS
reg1 <- lm(Y ~ T, data=turkey) 

## RDD
turkey$X2 <- turkey$X^2
turkey$T_X <- turkey$T*turkey$X
turkey$T_X2 <- turkey$T*turkey$X2

bw_ik <- IKbandwidth(turkey$X, turkey$Y, cutpoint=0)
reg2 <- lm(Y ~ T + X + T_X, data=turkey, subset=(X >= -bw_ik & X <= bw_ik)) 


## plot the main outcome
rdplot(turkey$Y[abs(turkey$X) <= 0.5], turkey$X[abs(turkey$X) <= 0.5], col.lines="blue", 
       title="", x.label="Islamic win margin", y.label="Female high school (%)")


## plot histogram
temp <- as.data.frame(turkey$X)
colnames(temp) <- c("var")

ggplot(data=temp,aes(x=var,y=..count..)) + 
  geom_histogram(breaks=seq(-1, 1, 0.01)) +
  geom_vline(xintercept = 0) + 
  labs(x="score", y="count")


## density test
DCdensity(turkey$X, cutpoint=0)


## plot other variables
rdplot(turkey$lpop1994[abs(turkey$X) <= 0.5], turkey$X[abs(turkey$X) <= 0.5], col.lines="blue", 
       title="", x.label="Islamic win margin", y.label="log population")

rdplot(turkey$sexr[abs(turkey$X) <= 0.5], turkey$X[abs(turkey$X) <= 0.5], col.lines="blue", 
       title="", x.label="Islamic win margin", y.label="sex ratio")

rdplot(turkey$vshr_islam1994[abs(turkey$X) <= 0.5], turkey$X[abs(turkey$X) <= 0.5], col.lines="blue", 
       title="", x.label="Islamic win margin", y.label="vote share of Islamic party")
