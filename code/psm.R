# Sample code for Econometrics II at Osaka University
# By Shuhei Kitamura at OSIPP


##### 1. Plot F #####

## logistic function
z <- seq(-4,4,length=1000)

logistic <- function(x){
  exp(x)/(1+exp(x))
}

## standard normal 
x <- rnorm(10000)

plot(z,logistic(z),type="l",xlab="z",ylab="F(z)",col="red")
lines(ecdf(x),col="blue")
abline(h=0.5)
legend(-3, 0.8, legend=c("Logistic", "Standard normal"), col=c("red", "blue"), lty=1:1, cex=0.8)



##### 2. Probit/logit models #####

## import library
library(wooldridge)
library(stargazer)
library(compareGroups)
library(mmdata)
library(AER)
library(doBy)
library(sandwich)
library(margins)
library(ggplot2)

## clear all variables
rm(list=ls())

## import data
mroz <- wooldridge::mroz

## regression
reg1 <- lm(inlf ~ kidslt6 + kidsge6 + age + educ + nwifeinc, data=mroz)

# robust standard errors
cov <- vcovHC(reg1, type = "HC0")
robust.se <- sqrt(diag(cov))

stargazer(reg1, reg1, se=list(NULL, robust.se), column.labels=c("default","robust"), type="text")


## count the number of observations outside [0, 1].
length(which(fitted(reg1)<0 | fitted(reg1)>1))


## probit and logit models
reg2 <- glm(inlf ~ kidslt6 + kidsge6 + age + educ + nwifeinc, family=binomial(link="logit"), data=mroz)
reg3 <- glm(inlf ~ kidslt6 + kidsge6 + age + educ + nwifeinc, family=binomial(link="probit"), data=mroz)

stargazer(reg1, reg2, reg3, se=list(robust.se, NULL, NULL), type="text")


## average marginal effects
reg2$coef * mean(reg2$fit*(1-reg2$fit))
reg3$coef * mean(dnorm(qnorm(reg3$fit)))


## marginal effects at a specific point
margins(reg2, variables="kidslt6", at=list(kidslt6=3))
margins(reg3, variables="kidslt6", at=list(kidslt6=3))


## plot response probability
reg4 <- glm(inlf ~ kidslt6, family=binomial(link="logit"), data=mroz)
            
plot(inlf ~ kidslt6, data=mroz,
     xlab="# kids less than 6 years old", ylab="Prob(labor force participation)", pch=19)             

curve(predict(reg4, data.frame(kidslt6=x), type="response"),
      lty=1, lwd=2, col="blue", add=TRUE)


## LR statistic
df <- length(reg2$coef) - 1 # df
  
LL <- reg2$dev/(-2) # residual deviance
LL0 <- reg2$null.dev/(-2) # null deviance
  
LR <- 2*(LL-LL0)
LRp <- 1-pchisq(LR, df=df)


## McFadden's pseudo R2
pR2 <- 1 - LL/LL0
  

##### 3. PSM #####
## import data
jt <- wooldridge::jtrain2

## regression
reg1 <- glm(train ~ age + educ + black + hisp + married + re74 + re75 + re78 + unem74 + unem75 + unem78, family=binomial(link="probit"), data=jt)
jt$p1 <- reg1$fit

## plot common support (experimental)
ggplot(data=jt, aes(x=p1, fill=factor(train))) +
  geom_histogram(binwidth=0.02, alpha=0.5, position='identity') + 
  scale_fill_manual(name="", values=c("blue","red"), labels=c('notrain','train')) + 
  labs(title="", x="", y="frequency")


## plot common support (non-experimental)
# import data
# setwd("...") # set working directory if necessary
jt2 <- read.csv("cps1re74.csv")

# make variables
jt2$re78 <- jt2$re78/1000
jt2$re74 <- jt2$re74/1000
jt2$re75 <- jt2$re75/1000

# get predicted probability
reg2 <- glm(treat ~ age + age2 + ed + black + hisp + nodeg + married + re74 + re75, family=binomial(link="probit"), data=jt2)
jt2$p1 <- reg2$fit

# plot common support (original)
ggplot(data=jt2,aes(x=p1, fill=factor(treat))) +
  geom_histogram(binwidth=0.02, alpha=0.5, position='identity') + 
  scale_fill_manual(name="",values=c("blue","red"), labels=c('notrain','train')) + 
  labs(title="", x="", y="percent")

# plot common support (restricted sample)
ggplot(data=jt2[(jt2$p1>0.1 & jt2$p1<0.9),], aes(x=p1, fill=factor(treat))) +
  geom_histogram(binwidth=0.02, alpha=0.5, position='identity') + 
  scale_fill_manual(name="",values=c("blue","red"), labels=c('notrain','train')) + 
  labs(title="", x="", y="percent")


## check balance
res1 <- compareGroups(train ~ age + agesq + educ + black + hisp + nodegree + married + re74 + re75, data = jt)
res2 <- compareGroups(treat ~ age + age2 + ed + black + hisp + nodeg + married + re74 + re75, data = jt2)
res3 <- compareGroups(treat ~ age + age2 + ed + black + hisp + nodeg + married + re74 + re75, data = jt2, subset=(p1>0.1 & p1<0.9))


## regression
reg1 <- lm(re78 ~ train + age + agesq + educ + black + hisp + nodegree + married + re74 + re75, data=jt)
reg2 <- lm(re78 ~ treat + age + age2 + ed + black + hisp + nodeg + married + re74 + re75, data=jt2)
reg3 <- lm(re78 ~ treat + age + age2 + ed + black + hisp + nodeg + married + re74 + re75, data=jt2, subset=(p1>0.1 & p1<0.9))


## PSM
# make variables
jt2 <- read.csv("cps1re74.csv")

# make variables
jt2$re78 <- jt2$re78/1000
jt2$re74 <- jt2$re74/1000
jt2$re75 <- jt2$re75/1000

jt2$ed2 <- jt2$ed^2
jt2$ed.re74 <- jt2$ed * jt2$re74
jt2$age3 <- jt2$age^3

jt2$u74 <- ifelse(jt2$re74==0,1,0)
jt2$u75 <- ifelse(jt2$re75==0,1,0)

# get scores
reg2 <- glm(treat ~ age + age2 + age3 + ed + ed2 + ed.re74 + black + hisp + nodeg + married + re74 + re75 + u74 + u75, family=binomial(link="logit"), data=jt2)
jt2$p1 <- reg2$fit

# take a subset
jt2.subset <- subset(jt2, p1>min(p1[treat==1]) & p1<max(p1[treat==1]))

# make blocks based on predicted probabilities
jt2.subset$pcentile <- with(jt2.subset, cut(p1, breaks=quantile(p1, probs=seq(0,1,by=0.02))))
jt2.subset$pcentile <- as.numeric(jt2.subset$pcentile)

# stratification
q = c()
n = c()
for (i in 1:50) {
  q[i] <- with(jt2.subset, mean(re78[pcentile==i & treat==1], na.rm=TRUE)) - with(jt2.subset, mean(re78[pcentile==i & treat==0], na.rm=TRUE))
  n[i] <- with(jt2.subset, length(re78[pcentile==i & treat==1]))
  #nc[i] <- with(jt2.subset, length(re78[pcentile==i & treat==0]))
  #na[i] <- with(jt2.subset, length(re78[pcentile==i]))
}

q[is.na(q)] <- 0
n[is.na(n)] <- 0

w <- n/sum(n)
stratified <- t(q)%*%w

# weighting
att <- with(jt2.subset, mean(p1/mean(treat)*(re78*treat/p1 - re78*(1-treat)/(1-p1))))
#ate <- with(jt2.subset, mean((re78*treat/p1 - re78*(1-treat)/(1-p1))))
#regress <- with(jt2.subset, mean(p1*(1-p1)/mean(p1*(1-p1))*(re78*treat/p1 - re78*(1-treat)/(1-p1))))

reg <- lm(re78 ~ treat + age + age2 + age3 + ed + ed2 + ed.re74 + black + hisp + nodeg + married + re74 + re75 + u74 + u75, data=jt2.subset)
stargazer(reg, type="text")


## MatchIt package
# #install.packages("MatchIt")
# library(MatchIt)
# 
# set.seed(1234)
# 
# jt2.subset2 <- jt2.subset[rowSums(is.na(jt2.subset)) == 0,] # take a subset to remove a row that has a missing value
# 
# m.out1 <- matchit(treat ~ age + age2 + age3 + ed + ed2 + ed.re74 + black + hisp + nodeg + married + re74 + re75 + u74 + u75, data = jt2.subset2, method="nearest", distance="probit", replacement=TRUE)
# summary(m.out1)
# 
# m.data1 <- match.data(m.out1, distance="pscore") # create ps matched data set from previous output
# with(m.data1, t.test(re78[treat==1], re78[treat==0], paired=TRUE))

