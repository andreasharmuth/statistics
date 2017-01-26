
################################
## Single proportion

# TESTING THE PROBABILITY = 0.5 WITH A TWO-SIDED ALTERNATIVE
# WE HAVE OBSERVED 518 OUT OF 1154
# WITHOUT CONTINUITY CORRECTIONS

prop.test(x = 10, n =  100, p = 0.5, correct = FALSE)

prop.test(518, 1154, p = 0.5, correct = FALSE)

################################
## Pill study: two proportions

#READING THE TABLE INTO R 
pill.study <- matrix(c(23, 34, 35, 132), ncol = 2, byrow = TRUE)
colnames(pill.study) <- c("Blood Clot", "No Clot")
rownames(pill.study) <- c("Pill", "No pill")
       
# TESTING THAT THE PROBABILITIES FOR THE TWO GROUPS ARE EQUAL
prop.test(x = pill.study, correct = FALSE)



################################
## Pill study: two proportions, chi-square test
qchisq(0.95,2-1)

# CHI2 TEST FOR TESTING THE PROBABILITIES FOR THE TWO GROUPS ARE EQUAL
chisq.test(pill.study, correct = FALSE)
#IF WE WANT THE EXPECTED NUMBERS SAVE THE TEST IN AN OBJECT
chi <- chisq.test(pill.study, correct = FALSE)
#THE EXPECTED VALUES
chi$expected


################################
## Poll study: Multiple proportions, chi-square test

#READING THE TABLE INTO  R
poll <- matrix(c(79, 91, 93, 84, 66, 60, 37, 43, 47), ncol = 3, byrow = TRUE)
colnames(poll) <- c("4 weeks", "2 weeks", "1 week")
rownames(poll) <- c("Cand1", "Cand2", "Undecided")

#COLUMN PERCENTAGES
colpercent <- prop.table(poll, 2)
colpercent

# Plotting percentages 
par(mar=c(5,4,4.1,2)+0.1)
barplot(t(colpercent), beside = TRUE, col = 2:4, las = 1, 
        ylab = "Percent each week", xlab = "Candidate", 
        main = "Distribution of Votes")
legend( legend = colnames(poll), fill = 2:4,"topright", cex = 0.5)
par(mar=c(5,4,4,2)+0.1)


#TESTING SAME DISTRIBUTION IN THE THREE POPULATIONS
chi <- chisq.test(poll, correct = FALSE)
chi

1 - pchisq(q = 6.962, df = 4)

#EXPECTED VALUES
chi$expected



# Malformations data from the Danish "Sundhedstyrelsen"
# Read raw data from Table 1:
malf <- read.table("malformationsdata.txt", sep=" ", dec=",")
malf

# The 12 x's: (observed number of malformation births)
malfxs <- malf$V4

# The 12 proportions:
malfprops <- malf$V8/1000

# The 12 ns:
malfns <- round(malfxs/malfprops)

# The 12 (n-x)'s:
malfnxs <- malfns-malfxs

years <- 1994:2005

plot(years, malfprops, main="Proportions of malformation births in Denmark")
lines(years, malfprops)

plot(years, malfprops, ylim=c(0.035, 0.055), main="Proportions of malformation births in Denmark")
lines(years, malfprops)

# Finding the 12 95 CIs:
lower <- malfprops - 1.96*sqrt(malfprops*(1-malfprops)/malfns)
upper <- malfprops + 1.96*sqrt(malfprops*(1-malfprops)/malfns)

# Adding them to the plot:
lines(years, lower, lty=2)
lines(years, upper, lty=2)
abline(h=sum(malfxs)/sum(malfns), col="blue", lwd=2)

# Testing for 12 equal proportions:
# By prop.test:
prop.test(malfxs, malfns)

# By chisq.test:
malfmatrix <- cbind(malfxs, malfnxs)
chisq.test(malfmatrix)
