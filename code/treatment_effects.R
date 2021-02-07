# Sample code for Econometrics II at Osaka University
# By Shuhei Kitamura


##### 1. sample average and population average (dice) #####
die <- 1:6
roll <- function(n) {
  mean(sample(die,size=n,replace=TRUE))
}

plot(sapply(1:200,roll),type="l",xlab="# of rolls",ylab="average",ylim=c(1,6))
abline(h=3.5,col="red")



##### 2. distribution of t-statistic #####
n <- 5000
data <- replicate(10000, {
  x <- sample(c(0,1),n,replace=TRUE)
  t.value <- (mean(x)-0.5)/(sd(x)/sqrt(length(x))) # compute t-statistic
})


## plot histogram
h <- hist(data,breaks=40,xlab="t-statistic",main="",xlim=c(-4,4),ylim=c(0,1500))


## plot std normal distribution
xfit <- seq(min(data),max(data),length=40)
yfit <- dnorm(xfit,mean=mean(data),sd=sd(data))
yfit <- yfit*diff(h$mids[1:2])*length(data)

lines(xfit,yfit,col="blue",lwd=1)
abline(v=c(-1.96,1.96), col="red")



##### 3. Lalonde (1986) #####
## read packages
library(wooldridge)
library(compareGroups)
library(stargazer)
library(mmdata)
library(AER)
library(doBy)


## clear all variables
rm(list=ls())


## import data
jt <- wooldridge::jtrain2


## difference in means
re78_t <- with(jt, mean(re78[train == 1]))
re78_c <- with(jt, mean(re78[train == 0]))


## get standard errors
n1 <- with(jt, length(re78[train == 1]))
n2 <- with(jt, length(re78[train == 0]))
sd1 <- with(jt, sd(re78[train == 1]))
sd2 <- with(jt, sd(re78[train == 0]))
sd_p <- sqrt(((n1-1)*sd1^2+(n2-1)*sd2^2)/(n1+n2-2))
sd <- sd_p*sqrt(1/n1+1/n2)


## t-statistic


## Cohen's d


## test statistical difference
res <- t.test(re78 ~ train, data=jt, var.equal=TRUE)

## power
alpha <- 0.05
qu <- qt(alpha/2,df=n1+n2-2,lower=FALSE)
power <- 1-pt(qu,df=n1+n2-2,ncp=diff_mean/sd) +pt(-qu,df=n1+n2-2,ncp=diff_mean/sd)


## balance table
res1 <- compareGroups(train ~ age + educ + black + hisp + married + re74 + re75 + re78 + unem74 + unem75 + unem78, data = jt)
res2 <- compareGroups(train ~ age + educ + black + hisp + married + re74 + re75 + re78 + unem74 + unem75 + unem78, data = jtrain3)


## regression
reg1 <- lm(re78 ~ train, data = jt) 
reg2 <- lm(re78 ~ train + age + educ + black + hisp + married + re74 + re75, data = jt) 
reg3 <- lm(re78 ~ train, data = jtrain3) 
reg4 <- lm(re78 ~ train + age + educ + black + hisp + married + re74 + re75, data = jtrain3) 
# restrict to unemployed in 1974 and 1975
reg5 <- lm(re78 ~ train, data = jtrain3, subset = c(unem75 == 1 & unem74 == 1)) 
reg6 <- lm(re78 ~ train + age + educ + black + hisp + married + re74 + re75, data = jtrain3, subset = c(unem75 == 1 & unem74 == 1)) 


## matching estimator
# regression
reg <- lm(re78 ~ train + black, data = jt) 

# matching
diff_black <- with(jt, mean(re78[black==1 & train==1])) - with(jt, mean(re78[black==1 & train==0]))
diff_noblack <- with(jt, mean(re78[black==0 & train==1])) - with(jt, mean(re78[black==0 & train==0]))

n_black_train <- with(jt, length(black[black==1 & train==1]))
n_noblack_train <- with(jt, length(black[black==0 & train==1]))
n_black_notrain <- with(jt, length(black[black==1 & train==0]))
n_noblack_notrain <- with(jt, length(black[black==0 & train==0]))

n_train <- n_black_train + n_noblack_train

w_black <- n_black_train/n_train
w_noblack <- n_noblack_train/n_train



##### 4. regression anatomy & OVB #####
## regression anatomy
reg1 <- lm(re78 ~ educ + age, data = jt) 
reg2 <- lm(educ ~ age, data = jt)
resid <- resid(reg2)
reg3 <- lm(re78 ~ resid, data = jt)


## Frisch-Waugh
reg0 <- lm(re78 ~ educ + age + hisp + black, data = jt)
reg1 <- lm(re78 ~ age + hisp + black, data = jt)
resid1 <- resid(reg1)
reg2 <- lm(educ ~ age + hisp + black, data = jt) 
resid2 <- resid(reg2) 
reg3 <- lm(resid1 ~ resid2, data = jt)


## OVB
reg1 <- lm(re78 ~ educ, data = jt) 
reg2 <- lm(re78 ~ educ + age, data = jt) 



##### 5. confidence intervals #####
CI_upper <- numeric(10000)
CI_lower <- numeric(10000)
pvalue <- numeric(10000)

n <- 5000

for (j in 1:10000) {
  x <- sample(c(0,1),n,replace=TRUE)
  ttest <- t.test(x,mu=0.5)
  
  CI_lower[j] <- ttest$conf.int[1]
  CI_upper[j] <- ttest$conf.int[2]
  pvalue[j] <- ttest$p.value
}

reject <- pvalue <= 0.05

table(reject)


## plot only 100 CIs
color <- rep(gray(.5),100)
color[reject[1:100]] <- "red"

plot(-5,xlim=c(0.45,0.55),ylim=c(0,100),xlab="",ylab="sample")
abline(v=0.5,lty=2)

for(j in 1:100){
  lines(c(CI_lower[j],CI_upper[j]),c(j,j),col=color[j],lwd=2)
}



##### 6. power calculation #####

## power
n1 <- 100; n2 <- 100 
sd1 <- 0.6; sd2 <- 0.5 
alpha <- 0.05
delta = 0.2 # MDE

p.body <- quote({
  nu <- n1 + n2 - 2
  qu <- qt(alpha/2, nu, lower = FALSE)
  sd_p <- sqrt(((n1-1)*sd1^2+(n2-1)*sd2^2)/(n1+n2-2))
  sd <- sd_p*sqrt(1/n1+1/n2)
  1 - pt(qu, nu, ncp = delta/sd) + pt(-qu, nu, ncp = delta/sd)
})

eval(p.body)


## sample size
sd1 <- 0.5; sd2 <- 0.6
alpha <- 0.05
power = 0.8 
delta = 0.2

p.body <- quote({
  nu <- n-2
  qu <- qt(alpha/2, nu, lower = FALSE)
  sd_p <- sqrt(((n/2-1)*sd1^2+(n/2-1)*sd2^2)/(n-2))
  sd <- sd_p*2/sqrt(n)
# one mean  
#  sd_p <- sqrt((n*sd^2)/(n-1))
#  sd <- sd_p/sqrt(n)
  1 - pt(qu, nu, ncp = delta/sd) + pt(-qu, nu, ncp = delta/sd)
})

uniroot(function(n) eval(p.body) - power, c(10,10000))$root # find n such that p.body = power


## MDE
n1 <- 100; n2 <- 100 # sample size
sd1 <- 0.6; sd2 <- 0.5 # sample std deviation
alpha <- 0.05
power = 0.8

p.body <- quote({
  nu <- n1 + n2 - 2
  qu <- qt(alpha/2, nu, lower = FALSE) # t_{alpha/2} 
  sd_p <- sqrt(((n1-1)*sd1^2+(n2-1)*sd2^2)/(n1+n2-2))
  sd <- sd_p*sqrt(1/n1+1/n2)
  1 - pt(qu, nu, ncp = delta/sd) + pt(-qu, nu, ncp = delta/sd)
})

uniroot(function(delta) eval(p.body) - power, c(1e-07, 10))$root # find delta such that p.body = power

