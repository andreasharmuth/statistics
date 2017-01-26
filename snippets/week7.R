




## Three equivalent ways of simulating the exponential distribution with lambda=1/2:
re1 <- -2*log(1-runif(10000))

re2 <- qexp(runif(10000), 1/2)

re3 <- rexp(10000, 1/2)

## Check the means and variances of each: 
c(mean(re1), mean(re2), mean(re3)) 

c(var(re1), var(re2), var(re3)) 



xseq <- seq(-5, 10, len=200)
##
plot(xseq, pexp(xseq, 1/2), type="l", xlab="Exponential outcomes", ylab="Uniform outcomes")
set.seed(123)
us <- runif(5)
arrows(rep(-5,5), us, qexp(us, 1/2), us, lty=3, length=0.2)
arrows(qexp(us, 1/2), us, qexp(us, 1/2), 0, lty=3, length=0.2)



set.seed(345)
k = 10000 # Number of simulations 
X = rnorm(k, 2, 0.01) 
Y = rnorm(k, 3, 0.02) 
A = X*Y 



mean(A) 
sd(A) 



mean(abs(A-6)>0.1)



x <- c(32.6, 1.6, 42.1, 29.2, 53.4, 79.3, 2.3 , 4.7, 13.6, 2.0)
n <- length(x)



## Set the number of simulations:
k <- 100000
## 1. Simulate 10 exponentials with the right mean k times:
set.seed(9876.543)
simsamples <- replicate(k, rexp(10, 1/26.08))
## 2. Compute the mean of the 10 simulated observations k times:
simmeans <- apply(simsamples, 2, mean)
## 3. Find the two relevant quantiles of the k simulated means:
quantile(simmeans, c(0.025, 0.975)) 



hist(simmeans, col="blue", nclass=30)



x <- c(32.6, 1.6, 42.1, 29.2, 53.4, 79.3, 2.3 , 4.7, 13.6, 2.0)
n <- length(x)
## Set the number of simulations:
k <- 100000
## 1. Simulate k samples of n=10 exponentials with the right mean:
set.seed(9876.543)
simsamples <- replicate(k, rexp(n, 1/26.08))
## 2. Compute the median of the n=1010 simulated observations k times:
simmedians <- apply(simsamples, 2, median)
## 3. Find the two relevant quantiles of the k simulated medians:
quantile(simmedians, c(0.025, 0.975)) 



hist(simmedians, col="blue", nclass=30)



Q3 <- function(x){ quantile(x, 0.75)}



## Read in the data:
x <- c(168, 161, 167, 179, 184, 166, 198, 187, 191, 179)
n <- length(x)
## Set the number of simulations:
k <- 100000
## 1. Simulate k samples of n=10 normals with the right mean and variance:
set.seed(9876.543)
simsamples <- replicate(k, rnorm(n, mean(x), sd(x)))
## 2. Compute the Q3 of the n=10 simulated observations k times:
simQ3s <- apply(simsamples, 2, Q3)
## 3. Find the two relevant quantiles of the k simulated medians:
quantile(simQ3s, c(0.005, 0.995)) 



hist(simQ3s, col="blue")



## round(rexp(12,1/45), 1)



x <- c(32.6, 1.6, 42.1, 29.2, 53.4, 79.3, 2.3 , 4.7, 13.6, 2.0)
y <- c(9.6, 22.2, 52.5, 12.6, 33.0, 15.2, 76.6, 36.3, 110.2, 18.0, 62.4, 10.3)
n1 <- length(x)
n2 <- length(y)
## Set the number of simulations:
k <- 100000
## 1. Simulate k samples of each n1=10 and n2=12 exponentials with the right means:
set.seed(9876.543)
simXsamples <- replicate(k, rexp(n1, 1/mean(x)))
simYsamples <- replicate(k, rexp(n2, 1/mean(y)))

## 2. Compute the difference between the simulated means k times:
simDifmeans <- apply(simXsamples, 2, mean) - apply(simYsamples, 2, mean) 
## 3. Find the two relevant quantiles of the k simulated differences of means:
quantile(simDifmeans, c(0.025, 0.975)) 



hist(simDifmeans, col="blue", nclass=25)



xA <- c(7.53, 7.48, 8.08, 8.09, 10.15, 8.4, 10.88, 6.13, 7.9)
xB <- c(9.21, 11.51, 12.79, 11.85, 9.97, 8.79, 9.69, 9.68, 9.19)
nA <- length(xA)
nB <- length(xB)



## Set the number of simulations:
k <- 100000
## 1. Simulate k samples of each nA=9 and nB=9 exponentials with the 
## right means and standard deviations:
set.seed(9843)
simAsamples <- replicate(k, rnorm(nA, mean(xA), sd(xA)))
simBsamples <- replicate(k, rnorm(nB, mean(xB), sd(xB)))

## 2. Compute the difference between the simulated medians k times:
simDifmedians <- apply(simAsamples, 2, median) - apply(simBsamples, 2, median) 
## 3. Find the two relevant quantiles of the k simulated differences of means:
quantile(simDifmedians, c(0.025, 0.975)) 



x1 <-  c(8, 24, 7, 20, 6, 20, 13, 15, 11, 22, 15) 
x2 <-  c(5, 11, 0, 15, 0, 20, 15, 19, 12, 0, 6) 
dif <- x1-x2 
dif 



t(replicate(5, sample(dif, replace = TRUE)))



k = 100000 
simsamples = replicate(k, sample(dif, replace = TRUE)) 
simmeans = apply(simsamples, 2, mean) 
quantile(simmeans, c(0.025,0.975)) 



k = 100000 
simsamples = replicate(k, sample(dif, replace = TRUE)) 
simmedians = apply(simsamples, 2, median) 
quantile(simmedians, c(0.025,0.975)) 



## Reading in no group: 
 x <- c(9, 10, 12, 6, 10, 8, 6, 20, 12) 
## Reading in yes group: 
 y <- c(14,15,19,12,13,13,16,14,9,12) 

k <- 100000 
simxsamples <- replicate(k, sample(x, replace = TRUE))
simysamples <- replicate(k, sample(y, replace = TRUE)) 
simmeandifs <- apply(simxsamples, 2, mean)-
                       apply(simysamples, 2, mean)  
quantile(simmeandifs, c(0.025,0.975)) 



k <- 100000 
simxsamples <- replicate(k, sample(x, replace = TRUE))
simysamples <- replicate(k, sample(y, replace = TRUE)) 
simmediandifs <- apply(simxsamples, 2, median)-apply(simysamples, 2, median)  
quantile(simmediandifs, c(0.005,0.995)) 



## install.packages("bootstrap")



library(bootstrap) 
quantile(bootstrap(dif,k,mean)$thetastar,c(0.025,0.975)) 




k=10000   # Number of simulations
xA=rexp(k,1/2) # generating k component A lifetimes
mean(xA) # Checking the mean of these

xB=rexp(k,1/3) # generating k component B lifetimes
mean(xB) # Checking the mean of these

xC=rexp(k,1/5) # generating k component C lifetimes
mean(xC) # Checking the mean of these

# Putting these three sets of k lifetimes together into a 
# single k-by-3 matrix:
x=cbind(xA,xB,xC)

# Finding the minimum value of the three components 
# in each of the k situations:
lifetimes=apply(x,1,min)



hist(lifetimes, col = "blue", nclass = 30)



mean(lifetimes)



sd(lifetimes)



mean(lifetimes<=1)



median(lifetimes)



quantile(lifetimes, 0.10)







x <- c(38.43, 38.43, 38.39, 38.83, 38.45, 38.35, 
       38.43, 38.31, 38.32, 38.48, 38.50)
k <- 10000
simsamples <- replicate(k, sample(x, replace = TRUE)) 
simmeans <- apply(simsamples, 2, mean) 
quantile(simmeans, c(0.025, 0.975)) 



hist(simmeans, col="blue", nclass=30)



k <- 10000
n <- length(x)
simsamples <- replicate(k, rnorm(n, mean(x), sd(x))) 
simmeans <- apply(simsamples, 2, mean) 
quantile(simmeans, c(0.025, 0.975)) 
hist(simmeans, col="blue", nclass=30)



t.test(x)



k <- 10000
simsamples <- replicate(k, rlnorm(n, mean(log(x)), sd(log(x)))) 
simmeans <- apply(simsamples, 2, mean) 
quantile(simmeans, c(0.025, 0.975)) 
hist(simmeans, col="blue", nclass=30)



Q1 <- function(x){ quantile(x, 0.25) }
k <- 10000
simsamples <- replicate(k, rnorm(n, mean(x), sd(x))) 
simQ1s <- apply(simsamples, 2, Q1) 
quantile(simQ1s, c(0.025, 0.975)) 
hist(simQ1s, col="blue", nclass=30)



par(mar=c(1,1,5,3))
k <- 10000
simsamples <- replicate(k, sample(x, replace = TRUE)) 
simQ1s <- apply(simsamples, 2, Q1) 
quantile(simQ1s, c(0.025, 0.975)) 





x1 <- c(1, 2, 1, 3, 2, 1, 2, 3, 1, 1)
x2 <- c(3, 4, 2, 4, 2, 3, 2, 4, 3, 2)
k = 10000 # Number of bootstrap samples 
simx1samples = replicate(k, sample(x1, replace = TRUE)) # Sample of TV1 group 
simx2samples = replicate(k, sample(x2, replace = TRUE)) # Sample of TV2 group 
simmeandifs = apply(simx1samples, 2, mean) - apply(simx2samples, 2, mean) 
quantile(simmeandifs, c(0.025,0.975)) # percentiles 



t.test(x1, x2)



simx1samples <- replicate(k, rlnorm(n, mean(log(x1)), sd(log(x1)))) 
simx2samples <- replicate(k, rlnorm(n, mean(log(x2)), sd(log(x2)))) 
simmeandifs = apply(simx1samples, 2, mean) - apply(simx2samples, 2, mean) 
quantile(simmeandifs, c(0.025,0.975)) # percentiles 





k <- 10000
Vs <- rnorm(k, 9.987, sd = 0.002)
Ts <- rnorm(k, 289.12, sd = 0.02)
Ps <- 8.31*Ts/Vs
sd(Ps)



k <- 10000
Ps <- rnorm(k, 240.28, sd = 0.03)
Ts <- rnorm(k, 289.12, sd = 0.02)
Vs <- 8.31*Ts/Ps
sd(Vs)


