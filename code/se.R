# Sample code for Econometrics II at Osaka University
# By Shuhei Kitamura at OSIPP

## import packages
library(wooldridge)
library(stargazer)
library(compareGroups)
library(mmdata)
library(AER)
library(doBy)
library(dplyr)
library(lfe)
library(sandwich)
library(lmtest)
library(ivpack)
library(clusterSEs)
library(ICC)

## clear all variables
rm(list=ls())

## import data
data(card.data)
card <- card.data

## regressions
reg1 <- lm(lwage ~ educ + exper + expersq + black + smsa + south, data=card)
reg2 <- lm(lwage ~ educ + exper + expersq + black + smsa + south + factor(region), data=card) 

# Breusch-Pagan test
ncvTest(lm(lwage ~ educ + exper + expersq + black + smsa + south + factor(region), data=card))

# robust std error
reg3 <- lm(lwage ~ educ + exper + expersq + black + smsa + south + factor(region), data=card) 
cov <- vcovHC(reg3, type = "HC1")
robust.se <- sqrt(diag(cov))

# cluster-robust std error
reg4 <- felm(lwage ~ educ + exper + expersq + black + smsa + south | region | 0 | region, data=card) 

stargazer(reg1, reg2, reg3, reg4, se=list(NULL, NULL, robust.se, NULL), type="text")

## compute Moulton factor
m1 <- lm(lwage ~ educ + exper + expersq + black + smsa + south, data=card) 
u <- residuals(m1)
res <- summary(m1)$coef

# intracluster correlation
rho.u <- ICCest(region, u, data=card)
rho.south <- ICCest(region, south, data=card)

# number of groups, average group size, variance of group size
by.region <- group_by(card, region)
groups <- summarize(by.region, count=n())
var.g <- var(groups[[2]])
mean.g <- mean(groups[[2]])

# scale up the std error by moulton factor
m.south <- sqrt(1 + (var.g/mean.g + mean.g - 1) * rho.south[[1]] * rho.u[[1]])
res[7,2] * m.south

# compare
m2 <- felm(lwage ~ educ + exper + expersq + black + smsa + south | 0 | 0 | region, data=card) 

stargazer(m1, m2, type="text")


## wild cluster bootstrap
reg5 <- glm(lwage ~ educ + exper + expersq + black + smsa + south, data=card)
stargazer(reg5, type="text", report=('vc*p'))

cluster.wild.glm(mod=reg5, dat=card, cluster= ~ region, ci.level = 0.95, boot.reps = 100)
