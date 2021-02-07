# Sample code for Econometrics II at Osaka University
# By Shuhei Kitamura at OSIPP

## import packages
library(wooldridge)
library(stargazer)
library(compareGroups)
library(mmdata)
library(AER)
library(doBy)
library(gmodels)
library(dplyr)
library(ggplot2)

## clear all variables
rm(list=ls())

##### 1. MDVE #####
## import data
mdve <- mmdata::mdve
mdve <- mdve[mdve$T_FINAL != 4,] # remove T_FINAL = other

## cross tabulation
with(mdve, CrossTable(T_RANDOM, T_FINAL, prop.c=FALSE, prop.t=FALSE, prop.chisq=FALSE))


## make coddle varables
coddle <- function(x){
  if(x==2 | x==3) 1 else 0
}

mdve$z_coddle <- sapply(mdve$T_RANDOM, coddle)
mdve$d_coddle <- sapply(mdve$T_FINAL, coddle)

arrest <- function(x){
  if(x==1) 1 else 0
}
advise <- function(x){
  if(x==2) 1 else 0
}
separate <- function(x){
  if(x==3) 1 else 0
}

mdve$z_arrest <- sapply(mdve$T_RANDOM, arrest)
mdve$z_advise <- sapply(mdve$T_RANDOM, advise)
mdve$z_separate <- sapply(mdve$T_RANDOM, separate)

with(mdve, CrossTable(z_coddle, d_coddle, prop.c=FALSE, prop.t=FALSE, prop.chisq=FALSE))


## 1st stage
exp_d_z1 <- with(mdve, length(d_coddle[z_coddle==1 & d_coddle==1])/length(d_coddle[z_coddle==1]))
exp_d_z0 <- with(mdve, length(d_coddle[z_coddle==0 & d_coddle==1])/length(d_coddle[z_coddle==0]))

first_stage <- exp_d_z1 - exp_d_z0


## ITT
# make outcome variable y
mdve$pz_separ <- 1/(1+exp(1.21)); mdve$pz_arrest <- 1/(1+exp(1.21+.9)); mdve$pz_advise <- 1/(1+exp(1.21+.21)) # from Table 4; Berk and Sherman, 1988 
mdve$pd_separ <- 1/(1+exp(1.05)); mdve$pd_arrest <- 1/(1+exp(1.05+.82)); mdve$pd_advise <- 1/(1+exp(1.05+.46)) # from Table 6; Berk and Sherman, 1988 

mdve$ranking <- 1:nrow(mdve)

mdve <- mdve[order(mdve$T_RANDOM),]

mdve <- mdve %>%
  group_by(T_RANDOM) %>%
  mutate(z_rank=order(ranking)) %>%
  add_tally() %>%
  mutate(z_rank2=z_rank/n)

mdve$y <- 0
mdve$y[mdve$z_arrest==1 & mdve$z_rank2 < mdve$pz_arrest] <- 1
mdve$y[mdve$z_advise==1 & mdve$z_rank2 < mdve$pz_advise] <- 1
mdve$y[mdve$z_separate==1 & mdve$z_rank2 < mdve$pz_separ]  <- 1

# cross tabulation
with(mdve, CrossTable(z_coddle, y, prop.c=FALSE, prop.t=FALSE, prop.chisq=FALSE))

# ITT
exp_y_z1 <- with(mdve, length(y[z_coddle==1 & y==1])/length(y[z_coddle == 1]))
exp_y_z0 <- with(mdve, length(y[z_coddle==0 & y==1])/length(y[z_coddle == 0]))

itt <- exp_y_z1 - exp_y_z0


## LATE


## regression version (2SLS)

# first stage
reg1 <- lm(d_coddle ~ z_coddle, data=mdve) 


# ITT
reg2 <- lm(y ~ z_coddle, data=mdve) 

# LATE (or the IV estimator)
reg3 <- ivreg(y ~ d_coddle | z_coddle, data=mdve)



##### 2. Angrist and Krueger (1991) #####
## import data
qob <- mmdata::qob

## make qob dummies
qob.f <- factor(qob$qob)
q <- model.matrix(~qob.f + 0)
qob <- cbind(qob,q)

## compute age when 1980q1 (measured in quarters)
qob$age <- ((79 - qob$yob)*4 + 5 - qob$qob)/4
# e.g. those who born in 1930q2 -> age: ((79-30)*4 + 5 - 2)/4 = 49.75 when 1980q1

## LATE
diff_school <- with(qob, mean(s[qob.f4==1])) - with(qob, mean(s[qob.f4!=1]))
diff_lnw <- with(qob, mean(lnw[qob.f4==1])) - with(qob, mean(lnw[qob.f4!=1]))


## regressions
reg1 <- lm(lnw ~ qob.f4, data=qob) 
reg2 <- lm(s ~ qob.f4, data=qob) 
reg3 <- ivreg(lnw ~ s | qob.f4, data=qob)
reg4 <- lm(lnw ~ s, data=qob)
reg5 <- ivreg(lnw ~ s + yob | qob.f4 + yob, data=qob)


## plots

# collapse for plots
qob_collapse <- summaryBy(s + lnw + qob + qob.f1 + qob.f2 + qob.f3 + qob.f4 ~ age, data=qob, keep.names = TRUE)

qob_collapse$yob <- 80 - qob_collapse$age

# plot first-stage
ggplot(data=qob_collapse,aes(x=yob,y=s,label=qob))+
  geom_line(color='blue') +
  labs(title="",x="year of birth",y="years of education") +
  geom_point(aes(x=yob,y=s),color='red') +
  scale_x_continuous(breaks=c(30,31,32,33,34,35,36,37,38,39,40)) +
  geom_text(vjust=-1) +
  theme(
    panel.background = element_rect(fill=NA),
    panel.border = element_rect(fill=NA, color="grey75"),
    axis.ticks = element_line(color="grey85"),
    legend.position = "none",
    plot.title = element_text(hjust=0.5,size=9),
    axis.title = element_text(size=9),
    axis.text = element_text(size=9))

# plot reduced-form
ggplot(data=qob_collapse,aes(x=yob,y=lnw,label=qob))+
  geom_line(color='blue') +
  labs(title="",x="year of birth",y="log weekly wages") +
  geom_point(aes(x=yob,y=lnw),color='red') +
  scale_x_continuous(breaks=c(30,31,32,33,34,35,36,37,38,39,40)) +
  geom_text(vjust=-1) +
  theme(
    panel.background = element_rect(fill=NA),
    panel.border = element_rect(fill=NA, color="grey75"),
    axis.ticks = element_line(color="grey85"),
    legend.position = "none",
    plot.title = element_text(hjust=0.5,size=9),
    axis.title = element_text(size=9),
    axis.text = element_text(size=9))


