
## UPdated 3/11 2015, 10:10


################################
## Read the data, plot and fit a simple linear regression model

## Read the data
Air <- read.table(file="air.txt", sep=",", header=TRUE)
## What is in Air?
str(Air)
Air
head(Air)

## Let us start with only 20 observations
## We skip this and go directly to the full data set
##Air <- Air[1:20,]

## See the relation between ozone and temperature
plot(Air$temperature, Air$ozone, xlab="Temperature", ylab="Ozon")

## Correlation
cor(Air$ozone, Air$temperature)

## Fit a simple linear regression model
summary(lm(ozone ~ temperature, data=Air))

## Add a vector with random values, is there a significant linear relation?
## ONLY for illustration purposes - this is NOT a part of the "standard procedure" for MLR

Air$noise <- rnorm(nrow(Air))
plot(Air$ozone, Air$noise, xlab="Noise", ylab="Ozon")
cor(Air$ozone, Air$noise)
summary(lm(ozone ~ noise, data=Air))



################################
## With each of the other two independent variables

## Simple linear regression model with the wind speed
plot(Air$wind, Air$ozone, ylab="Ozone", xlab="Wind speed")
cor(Air$ozone, Air$wind)
summary(lm(ozone ~ wind, data=Air))

## Simple linear regression model with the radiation
plot(Air$radiation, Air$ozone, ylab="Ozone", xlab="Radiation")
cor(Air$ozone, Air$radiation)
summary(lm(ozone ~ radiation, data=Air))



################################
## Extend the model

## Forward selection:
## Add wind to the model
summary(lm(ozone ~ temperature + wind, data=Air))
## Add radiation to the model
summary(lm(ozone ~ temperature + wind + radiation, data=Air))



################################
## Backward selection

## Fit the full model
summary(lm(ozone ~ temperature + wind + radiation + noise, data=Air))
## Remove the most non-significant input, are all now significant?
summary(lm(ozone ~ temperature + wind + radiation, data=Air))



################################
## Assumption of normal distributed residuals

## Save the selected fit
fitSel <- lm(ozone ~ temperature + wind + radiation, data=Air)

## qq-normalplot
qqnorm(fitSel$residuals)
qqline(fitSel$residuals)



################################
## Plot the residuals vs. predicted values

plot(fitSel$fitted.values, fitSel$residuals, xlab="Predicted values", ylab="Residuals")



################################
## Plot the residuals vs. the independent variables

par(mfrow=c(1,3))
plot(Air$temperature, fitSel$residuals, xlab="Temperature", ylab="Residuals")
plot(Air$wind, fitSel$residuals, xlab="Wind speed", ylab="Residuals")
plot(Air$radiation, fitSel$residuals, xlab="Radiation", ylab="Residuals")



################################
## Extend the ozone model with appropriate curvilinear regression

## Plot the residuals vs. the independent variables
par(mfrow=c(1,3))
plot(Air$wind, fitSel$residuals, pch=19)
plot(Air$temperature, fitSel$residuals, pch=19)
plot(Air$radiation, fitSel$residuals, pch=19)

## Make the squared wind speed
Air$windSq <- Air$wind^2
## Add it to the model
fitWindSq <- lm(ozone ~ temperature + wind + windSq + radiation, data=Air)
summary(fitWindSq)

## Equivalently for the temperature
Air$temperature2 <- Air$temperature^2
## Add it
fitTemperatureSq <- lm(ozone ~ temperature + temperature2 + wind + radiation, data=Air)
summary(fitTemperatureSq)

## Equivalently for the radiation
Air$radiation2 <- Air$radiation^2
## Add it
fitRadiationSq <- lm(ozone ~ temperature + wind + radiation + radiation2, data=Air)
summary(fitRadiationSq)

## Which one was best?
summary(fitWindSq)
summary(fitTemperatureSq)

## One could try to extend the model further
fitWindSqTemperaturSq <- lm(ozone ~ temperature + temperature2 + wind + windSq + radiation, data=Air)
summary(fitWindSqTemperaturSq)

## Model validation
qqnorm(fitWindSq$residuals)
qqline(fitWindSq$residuals)
plot(fitWindSq$residuals, fitWindSq$fitted.values, pch=19)



################################
## Confidence and prediction intervals for the curvilinear model

## Generate a new data.frame with constant temperature and radiation, but with varying wind speed
wind<-seq(1,20.3,by=0.1)
AirForPred <- data.frame(temperature=mean(Air$temperature), wind=wind, windSq=wind^2, radiation=mean(Air$radiation))

## Calculate confidence and prediction intervals (actually bands)
CI <- predict(fitWindSq, newdata=AirForPred, interval="confidence", level=0.95)
PI <- predict(fitWindSq, newdata=AirForPred, interval="prediction", level=0.95)

## Plot them
plot(wind, CI[,"fit"], ylim=range(CI,PI), type="l", main=paste("Ved temperatur =",format(mean(Air$temperature),digits=3), "og instraaling =", format(mean(Air$radiation),digits=3)))
lines(wind, CI[,"lwr"], lty=2, col=2)
lines(wind, CI[,"upr"], lty=2, col=2)  
lines(wind, PI[,"lwr"], lty=2, col=3)
lines(wind, PI[,"upr"], lty=2, col=3)
## legend
legend("topright", c("PrÃ¦ediktion","95% konfidensbÃ¥nd","95% prÃ¦diktionsbÃ¥nd"), lty=c(1,2,2), col=1:3)



################################
## See problems with highly correlated inputs

## Generate values for MLR
n <- 100
## First variable
x1 <- sin(0:(n-1)/(n-1)*2*2*pi) + rnorm(n, 0, 0.1)
plot(x1, type="b")
## The second variable is the first plus a little noise
x2 <- x1 + rnorm(n, 0, 0.1)
plot(x2, type="b")

## x1 and x2 are highly correlated
plot(x1,x2)
cor(x1,x2)
## Simulate an MLR
beta0=20; beta1=1; beta2=1; sigma=1
y <- beta0 + beta1 * x1 + beta2 * x2 + rnorm(n,0,sigma)
## See scatter plots for y vs. x1, and y vs. x2
par(mfrow=c(1,2))
plot(x1,y)
plot(x2,y)
## Fit an MLR
summary(lm(y ~ x1 + x2))

## If it was an experiment and the effects could be separated in the design
x1[1:(n/2)] <- 0
x2[(n/2):n] <- 0
## Plot them
plot(x1, type="b")
lines(x2, type="b", col="red")
## Now very low correlation
cor(x1,x2)
## Simulate MLR again
y <- beta0 + beta1 * x1 + beta2 * x2 + rnorm(n,0,sigma)
## and fit MLR
summary(lm(y ~ x1 + x2))
