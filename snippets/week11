# Getting the Bang and Olufsen data from the lmerTest-package:
library(lmerTest) # (Udviklet af os)
data(TVbo)

# Each of 8 assessors scored each of 12 combinations 2 times
# Let's look at only a single picture and one of the two reps:
# And let us look at the sharpness
TVbosubset <- subset(TVbo,Picture==1 & Repeat==1)[,c(1, 2, 9)]

TVbosubset
sharp <- matrix(TVbosubset$Sharpness, nrow=8, byrow=T)
colnames(sharp) <- c("TV3", "TV2", "TV1")
rownames(sharp) <- c("Person 1", "Person 2", "Person 3", 
                     "Person 4", "Person 5", "Person 6", 
                     "Person 7", "Person 8")
sharp
# library(xtable)
# xtable(sharp)
par(mfrow=c(1,2))
with(TVbosubset, plot(Sharpness~TVset))
with(TVbosubset, plot(Sharpness~Assessor))

anova(lm(Sharpness ~ Assessor + TVset, data = TVbosubset))

####################################
## Input data and plot
## Observations
y <- c(2.8, 3.6, 3.4, 2.3,
       5.5, 6.3, 6.1, 5.7,
       5.8, 8.3, 6.9, 6.1)
## treatments (Groups, varieties)
treatm <- factor(c(1, 1, 1, 1,
                   2, 2, 2, 2,
                   3, 3, 3, 3))
## blocks (persons, fields)
block <- factor(c(1, 2, 3, 4, 
                  1, 2, 3, 4,
                  1, 2, 3, 4))
## for later formulas
(k <- length(unique(treatm)))
(l <- length(unique(block)))

y
treatm
block
cbind(y, treatm, block)

## Plots
par(mfrow=c(1,2))
## Plot histogramms by treatments
plot(treatm, y, xlab="Treatments", ylab="y")
## Plot histograms by  blocks
plot(block, y, xlab="Blocks", ylab="y")
####################################
## Compute estimates of parameters in the model

## Sample mean
(muHat <- mean(y))
## Sample mean for each treatment
(alphaHat <- tapply(y, treatm, mean) - muHat)
## Sample mean for each  Block
(betaHat <- tapply(y, block, mean) - muHat)
################################
## Find the total variation, Sum of Squares,  SST

## SST for the example
(SST <- sum( (y - muHat)^2 ))
################################
## Find the variability explained by the treatments

## The Sum of Squares  for treatment SS(Tr) for the example
(SSTr <- l * sum(alphaHat^2))
################################
## Find the variability explained by the blocks

## The Sum of Squares  for the blocks  SS(Bl) for the example
(SSBl <- k * sum(betaHat^2))
################################
## Find the variability left after model fit, SSE

##  The Sum of Squares  for the residuals for the example
(SSE <- SST - SSTr - SSBl)
################################
## Plot the F distribution and see the critical value for treatments

par(mfrow=c(1,1))
## Remember, this is "under H0" (that is we compute as if H0 is true):
## Sequence for plot
xseq <- seq(0, 10, by=0.1)
## Plot the density of the  F distribution
plot(xseq, df(xseq, df1=k-1, df2=(k-1)*(l-1)), type="l")
##The critical value for significance level 5 %
cr <- qf(0.95, df1=k-1, df2=(k-1)*(l-1))
## Mark it in the plot 
abline(v=cr, col="red") 
## The value of the  test statistic
(Ftr <- (SSTr/(k-1)) / (SSE/((k-1)*(l-1))))
## The p-value hence is:
(1 - pf(Ftr, df1=k-1, df2=(k-1)*(l-1)))

################################
## Plot the F distribution  and see the critical value

## Remember, this is "under H0" (that is we compute as if H0 is true):
## Sequence for plot
xseq <- seq(0, 10, by=0.1)
## Plot the density of the  F distribution
plot(xseq, df(xseq, df1=l-1, df2=(k-1)*(l-1)), type="l")
##The critical value for significance level 5 %
cr <- qf(0.95, df1=l-1, df2=(k-1)*(l-1))
## Mark it in the plot 
abline(v=cr, col="red") 
## The value of the  test statistic
(Fbl <- (SSBl/(l-1)) / (SSE/((k-1)*(l-1))))
## The p-value hence is:
(1 - pf(Fbl, df1=l-1, df2=(k-1)*(l-1)))

################################
## All this can be found using anova() and lm()


anova(lm(y ~ treatm + block))

#### Simulate the  sampling-distribution  under the  null hypothesis:
## Eg without  the TVset effect: (but WITH  person-effect)

## Sample mean
(muHat <- mean(TVbosubset$Sharpness))
## Sample mean for each  assessor(blok)
(betaHat <- tapply(TVbosubset$Sharpness, TVbosubset$Assessor, mean) - muHat)

Sblock <- factor(rep(1:8, 3))
Streatm <- factor(rep(1:3, c(8, 8, 8)))

n <- 1000
F_sims <- rep(0, n)
for (i in 1:n){
ysim <- muHat + rep(betaHat, 3) + rnorm(24)
F_sims[i] <- anova(lm(ysim ~ Streatm + Sblock))[1, 4]
}

par(mfrow=c(1,1))
## Plot the simulated F-statistics AND the real F-distribution:
hist(F_sims, freq=FALSE)
lines(seq(0, 10, by=0.01), df(seq(0, 10, by=0.01), 2, 14))

################################
## Check assumption of  homogeneous variance


## Save the  fit
fit <- lm(y ~ treatm + block)

fit <- lm(Sharpness ~ Assessor + TVset, data = TVbosubset)

## Box plot
par(mfrow=c(1,2))
with(TVbosubset, plot(TVset, fit$residuals, y, xlab="Treatment"))
## Box plot
with(TVbosubset, plot(Assessor, fit$residuals, xlab="Block"))
################################
## Check the assumption  of  normality of residuals

## qq-normal plot of residuals
qqnorm(fit$residuals)
qqline(fit$residuals)

## Or with a  Wally plot
require(MESS)
qqwrap <- function(x, y, ...) {qqnorm(y, main="",...);
  qqline(y)}
## Can we see a deviating qq-norm plot?
wallyplot(fit$residuals, FUN = qqwrap)
