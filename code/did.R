# Sample code for Econometrics II at Osaka University
# By Shuhei Kitamura at OSIPP

# import packages
library(wooldridge)
library(stargazer)
library(compareGroups)
library(mmdata)
library(AER)
library(doBy)
library(dplyr)
library(lfe)
library(ggplot2)

# clear all variables
rm(list=ls())

##### 1. union membership and log wage #####
## import data
union <- wooldridge::wagepan

## regression
reg1 <- lm(lwage ~ union, data=union)
reg2 <- lm(lwage ~ union + educ + black + hisp + exper + expersq + married, data=union)
reg3 <- felm(lwage ~ union + expersq + married | nr + year, data=union)

## demeaned regression
# make averages
union_mean <- summaryBy(lwage + union + educ + black + hisp + exper + expersq + married + d81 + d82 + d83 + d84 + d85 + d86 + d87 ~ nr, data=union)
union <- merge(union, union_mean, by="nr")

myvec <- c("lwage", "union", "educ", "black", "hisp", "exper", "expersq", "married", "d81", "d82", "d83", "d84", "d85", "d86", "d87")

for(i in myvec){
  union[paste0(i,".diff")] <- union[i] - union[paste0(i,".mean")]
}

reg4 <- felm(lwage ~ union + expersq + married + factor(year) | nr, data=union)
reg5 <- lm(lwage.diff ~ union.diff + expersq.diff + married.diff + d81.diff + d82.diff + d83.diff + d84.diff + d85.diff + d86.diff + d87.diff + 0, data=union)


## recover the estimates on individual fixed effects
# take a subset (in order to reduce the number of individual FEs)
union_subset <- subset(union, nr <= 150)

# make individual FEs
n <- factor(union_subset$nr)
nr <- model.matrix(~ n + 0)
union_subset <- cbind(union_subset,nr)

# get estimates
reg6 <- lm(lwage.diff ~ union.diff + expersq.diff + married.diff + d81.diff + d82.diff + d83.diff + d84.diff + d85.diff + d86.diff + d87.diff + 0, data=union_subset)
est <- reg6$coefficients
est

# compute individual fixed effects
union_subset$indiv <- with(union_subset, lwage.mean - est[1]*union.mean - est[2]*expersq.mean - est[3]*married.mean
                           - est[4]*d81.mean - est[5]*d82.mean - est[6]*d83.mean - est[7]*d84.mean 
                           - est[8]*d85.mean - est[9]*d86.mean - est[10]*d87.mean)

# check the outcome (indiv variable)
View(union_subset)

# compare it with the regression estimates
reg7 <- lm(lwage ~ union + expersq + married + d81 +d82 + d83 + d84 + d85 + d86 + d87 + n13 + n17 + n18 + n45 + n110 + n120 + n126 + n150 + 0, data=union_subset)




##### 2. Bank failures in Mississippi #####
## import data
banks <- as.data.frame(mmdata::banks) # changed
banks$date <- as.Date(with(banks,paste(year,month,day,sep="-")),"%Y-%m-%d")

banks.lmt <- subset(banks,(month==7 & day==1))

diff_sixth  <- with(banks.lmt, bib6[year==1931]) - with(banks.lmt, bib6[year==1930])
diff_eighth <- with(banks.lmt, bib8[year==1931]) - with(banks.lmt, bib8[year==1930])

## BA


## DID


## use 7/1 data
# reshape from wide to long
banks.rshp <- reshape(banks.lmt, direction="long", 
                      varying=c("bib6","bio6","bib8","bio8"), 
                      v.names=c("bib","bio"),
                      timevar="d6", times=c(1,0))

banks.rshp$post <- as.numeric(banks.rshp$year>=1931)

# regression
reg1 <- lm(bib ~ d6*post, data=banks.rshp)
reg2 <- felm(bib ~ d6*post | year, data=banks.rshp)
reg3 <- felm(bib ~ d6*post + d6*year | year, data=banks.rshp)


## use original data
# reshape from wide to long
banks.rshp2 <- reshape(banks, direction="long", 
                       varying=c("bib6","bio6","bib8","bio8"), 
                       v.names=c("bib","bio"),
                       timevar="d6", times=c(1,0))

banks.rshp2$post <- as.numeric(banks.rshp2$year>=1931)

# regression
reg4 <- lm(bib ~ d6*post, data=banks.rshp2)
reg5 <- felm(bib ~ d6*post | year, data=banks.rshp2)
reg6 <- lm(bib ~ d6*factor(year), data=banks.rshp2)


## plot
ggplot(data=banks, aes(x=date, y=bib6)) +
  geom_line(color='blue') +
  geom_line(aes(x=date, y=bib8), color='red') +  
  labs(title="", x="date", y="banks in business") +
  geom_vline(xintercept=as.Date("1930-12-01"), linetype="dotted") +
  annotate(geom="text", x=as.Date("1931-06-01"), y=140, label="8th dist.") +
  annotate(geom="text", x=as.Date("1931-06-01"), y=110, label="6th dist.")
