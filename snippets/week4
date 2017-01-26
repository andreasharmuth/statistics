################################
## Simulate the sampling distribution of the mean of a random normal variabel

## Mean
mu <- 178
## Standard deviation
sigma <- 12
## Sample size
n <- 10
## Simulate  normally distributed  X_i
x <- rnorm(n=n, mean=mu, sd=sigma)
## See the realizations
x
## Empirical density
hist(x, prob=TRUE, col='blue')
## Find the sample mean
mean(x)
##  Find the sample variance
var(x)
## Repeat the simulated sampling many times
mat <- replicate(100, rnorm(n=n, mean=mu, sd=sigma))
## Find the sample mean for each of them
xbar <- apply(mat, 2, mean)
## Now we have many realizations of the sample mean
xbar
## See their distribution
hist(xbar, prob=TRUE, col='blue')
## There mean
mean(xbar)
## and sample variance
var(xbar)
sd(xbar)

## R-code for lecture 4

qt(0.975,9)
qt(0.975,99)
qnorm(0.975)

## HÃ¸jde-eksempel / height example:
 x <- c(168,161,167,179,184,166,198,187,191,179)

  ## CI for mu:
t.test(x)
t.test(x, conf.level=0.99)

## CI for sigma:
 sqrt(c(9*var(x)/qchisq(0.975,9),9*var(x)/qchisq(0.025,9)))

## Normal and t-distribution, Plot 1:
 x=seq(-4,4, by=0.01)
 plot(x,dt(x,9), type="l", col=2)
 lines(x,dnorm(x), type="l")
 text(2.5,0.3,"Black: standard normal")
 text(3,0.1,"Red: t(9)", col = 2)
 polygon(c(2,seq(2,4, by=0.01),4,2),c(0,dt(seq(2,4, by=0.01),9),0, 0),col="pink")
 text(3.5,0.04, "P(T>2)=0.038",col=2)

 ## Normal and t-distribution, Plot 2:
 x=seq(-4,4, by=0.01)
 plot(x,dt(x,9), type="l", col=2)
 lines(x,dnorm(x), type="l")
 text(2.5,0.3,"Black: standard normal")
 text(2.8,0.1,"Red: t(9)", col = 2)
 polygon(c(2,seq(2,4, by=0.01),4,2),c(0,dnorm(seq(2,4, by=0.01)),0, 0),col="grey")
 text(3.3,0.04, "P(Z>2)=0.023")

 
 ## Normal and t-distribution, Example from eNote:
 
## The P(T>1.96) probability  for n=10:
1-pt(1.96,df=9)
## The P(Z>1.96) probability:
1-pnorm(1.96)
## The P(T>1.96) probability  for n-values, 10, 20, ... ,50:
1-pt(1.96,df=seq(9,49,by=10))
## The P(T>1.96) probability  for n-values, 100, 200, ... ,500:
1-pt(1.96,df=seq(99,499,by=100))


## The standard normal 97.5% quantile:
 qnorm(0.975)
## The t-quantiles for n-values, 10, 20, ... ,50, rounded to 3 decimal points:
 round(qt(0.975,df=seq(9,49,by=10)),3)
## The t-quantiles for n-values, 100, 200, ... ,500 rounded to 3 decimal points:
 round(qt(0.975,df=seq(99,499,by=100)),3)

## The t-quantiles for n=10:
 qt(0.975,9)

## The t-quantiles for n=10:
 qt(0.995,9)


## The 99% confidence interval for the mean
x <- c(168,161,167,179,184,166,198,187,191,179)
n <- length(x)
mean(x) - qt(0.995, df=9) * sd(x) / sqrt(n)
mean(x) + qt(0.995, df=9) * sd(x) / sqrt(n)

## The 99% confidence interval for the mean
t.test(x,conf.level=0.99)

par(mar = c(4.5, 3.5, 3.5, 0.5))

 # Central Limit Theorem (For uniform distribution)
par(mfrow=c(2,2))
n=1
k=1000
u=matrix(runif(k*n),ncol=n)
hist(apply(u,1,mean),col="blue",main="n=1",xlab="Means",freq=F)
lines(seq(0,1,0.01),dnorm(seq(0,1,0.01),mean(apply(u,1,mean)),sd(apply(u,1,mean))))
 
 n=2
k=1000
u=matrix(runif(k*n),ncol=n)
hist(apply(u,1,mean),col="blue",main="n=2",xlab="Means",freq=F)
lines(seq(0,1,0.01),dnorm(seq(0,1,0.01),mean(apply(u,1,mean)),sd(apply(u,1,mean))))
 
 n=6
k=1000
u=matrix(runif(k*n),ncol=n)
hist(apply(u,1,mean),col="blue",main="n=6",xlab="Means",freq=F)
lines(seq(0,1,0.01),dnorm(seq(0,1,0.01),mean(apply(u,1,mean)),sd(apply(u,1,mean))))
 
n=30
k=1000
u=matrix(runif(k*n),ncol=n)
hist(apply(u,1,mean),nclass=15,col="blue",main="n=30",xlab="Means",freq=F)
lines(seq(0,1,0.01),dnorm(seq(0,1,0.01),mean(apply(u,1,mean)),sd(apply(u,1,mean))))
 
par(mfrow=c(1,1))

## We set a seed to be able get the same results:
set.seed(12345.6789)

thousandCIs <- replicate(1000,t.test(rnorm(50, 1, 1))$conf.int)

## Counting how often the interval is to the right of 1:
sum(thousandCIs[1,]>1)
## Counting how often the interval is to the left of 1:
sum(thousandCIs[2,]<1)
## So how often is 1 covered:
1000-sum(thousandCIs[1,]>1)-sum(thousandCIs[2,]<1)


## The chisquare-distribution with df=9: (the density)
 x <- seq(0, 20, by = 0.1)
 plot(x, dchisq(x, df = 9), type = "l")


## Quantiles of the chi-square distribution:
qchisq(c(0.025, 0.975), df = 19)
