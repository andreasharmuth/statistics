
################################
## Motivating example - sleeping medicine, manuel computation

## Read the data
x <- c(1.2, 2.4, 1.3, 1.3, 0.9, 1.0, 1.8, 0.8, 4.6, 1.4) 
n <- length(x)
## Fid the observed t statistic
tobs <- (mean(x) - 0) / (sd(x) / sqrt(n))
## Find the p-value, as the probability of getting tobs or something 
## even more extreme
pvalue <- 2 * (1 - pt(abs(tobs), df=n-1))
pvalue


################################
## Motivating example - sleeping medicine Hypothesis test
## Using the R function

## Call the function with the data x
t.test(x)


################################
## Example - PC screens: Reject the claim of the producer

## Data and the call to the function using the 
## proper options
x <- c(82, 86, 84, 84, 92, 83, 93, 80, 83, 84, 85, 86)
t.test(x, mu = 83, alt = "greater")


################################
## Power computation

## Find power for t-test
power.t.test(n = 40, delta = 4, sd = 12.21,
      type = "one.sample", alternative= "one.sided")


################################
## Sample size for given power

## Sample size for t-test
power.t.test(power = .80, delta = 4, sd = 12.21, 
      type = "one.sample", alternative= "one.sided")


################################
## Check whether data is normally distributed

## Histogram heights example
x <- c(168,161,167,179,184,166,198,187,191,179)
hist(x, xlab="Height", main="", freq = FALSE)
lines(seq(160, 200, 1), dnorm(seq(160, 200, 1), mean(x), sd(x)))

## 100 observations from normal distribution
xr <- rnorm(100, mean(x), sd(x))
hist(xr, xlab="Height", main="", freq = FALSE)
lines(seq(130, 230, 1), dnorm(seq(130, 230, 1), mean(x), sd(x)))
par(mar = c(3.5, 3.5, 1.5, 0.5))

## QQ-plot
qqnorm(x)
qqline(x)

## The Wally plot: using package MESS - install that first!
fit1 <- lm(x ~ 1)
residualer <- resid(fit1)
require(MESS)
qqwrap <- function(x, y, ...) {qqnorm(y, main="",...); abline(a = 0, b = 1)}
wallyplot(residualer, FUN = qqwrap)


################################
## Example radon data

## READING IN THE DATA
radon<-c(2.4, 4.2, 1.8, 2.5, 5.4, 2.2, 4.0, 1.1, 1.5, 5.4, 6.3,
        1.9, 1.7, 1.1, 6.6, 3.1, 2.3, 1.4, 2.9, 2.9)
##A HISTOGRAM AND A QQ-PLOT
par(mfrow=c(1,2), mar = c(3.5, 3.5, 1.5, 0.5))
hist(radon)
qqnorm(radon,ylab = 'Sample quantiles',xlab = "Normal quantiles")
qqline(radon)
par(mfrow=c(1,2), mar = c(3.5, 3.5, 1.5, 0.5))

##TRANSFORM USING NATURAL LOGARITHM
logRadon<-log(radon)

hist(logRadon)
qqnorm(logRadon,ylab = 'Sample quantiles',xlab = "Normal quantiles")
qqline(logRadon)
