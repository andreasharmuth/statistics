## R code from Chapter 1 made ready for lecture

## Adding numbers in the console
2+3
y <- 3

x <- c(1, 4, 6, 2)
x

x <- 1:10
x

x <- seq( 0, 1, by=0.1)
x

## Sample Mean and Median

x <- c(168,161,167,179,184,166,198,187,191,179)
length(x)
mean(x)
median(x)

## Sample variance and standard deviation
var(x)
sqrt(var(x))
sd(x)

## Sample quartiles
quantile(x, type = 2)

## Sample quantiles 0%, 10%,..,90%, 100%:
quantile(x, probs = seq(0, 1, by = 0.10), type = 2)

## A histogram of the heights:
hist(x)

## A density histogram of the heights:
hist(x,freq=FALSE,col="red",nclass=8)


## Empirical Cumulative Distributions Functions of the heights:
plot(ecdf(x),verticals=TRUE)
boxplot(x)

## A basic boxplot of the heights: (range=0 makes it "basic")
boxplot(x,range=0,col="red",main="Basic boxplot")
text(1.3,quantile(x),c("Minimum","Q1","Median","Q3","Maximum"),col="blue")

## A modified boxplot of the heights with an extreme observation, 235cm added:
## The modified version is the default
boxplot(c(x,235),col="red",main="Modified boxplot")
text(1.3,quantile(c(x,235)),c("Minimum","Q1","Median","Q3","Maximum"),
     col="blue")


## 2 Group Example
Males <-  c(152, 171, 173, 173, 178, 179, 180, 180, 182, 182, 182, 185,
            185 ,185, 185, 185 ,186 ,187 ,190 ,190, 192, 192, 197)
Females <-c(159, 166, 168 ,168 ,171 ,171 ,172, 172, 173, 174 ,175 ,175,
            175, 175, 175, 177, 178)
boxplot(list(Males, Females), col=2:3, names=c("Males", "Females"))


## Or read from data file:
studentheights <- read.table("studentheights.csv", sep=";", dec=".",
                             header=TRUE)

## Have a look at the first 6 rows of the data:
head(studentheights)
## Get a summary of each column/variable in the data:
summary(studentheights)

boxplot(Height ~ Gender, data=studentheights, col=2:3)

## To make 2 plots on a single plot-region:
par(mfrow=c(1,2))


## First the default version:
plot(mtcars$wt, mtcars$mpg)
## Then a nicer version:
plot(mpg ~ wt, xlab="Car Weight (1000lbs)", data=mtcars,
     ylab="Miles pr. Gallon", col=factor(am),
     sub="Red: manual transmission", main="Inverse fuel usage vs. size")

## The sample covariance and correlation 

attach(mtcars)
length(wt)
sd(wt)
sd(mpg)

sum((wt-mean(wt))*(mpg-mean(mpg)))/31
cov(mtcars$wt,mtcars$mpg)

cov(mtcars$wt,mtcars$mpg)/(sd(mtcars$wt)*sd(mtcars$mpg))

cor(mtcars$wt,mtcars$mpg)

detach(mtcars)


## Barplot:
barplot(table(studentheights$Gender), col=2:3)


## Pie chart:
pie(table(studentheights$Gender), cex=1, radius=1)


# Example from Slides:
x=c(185, 184, 194, 180, 182)


# Example from Socrative quiz 1/9 2015:
heights <- read.table("heights.csv", sep=";", dec=",",
                      header=FALSE)
heights <- heights$V1
length(heights)
hist(heights)
boxplot(heights)
summary(heights)

