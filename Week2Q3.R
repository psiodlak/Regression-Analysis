# Week 2 Question 3

# Tractor maintenance

trac <- read.csv("/Users/salmansikandar/Documents/Regresion_Analysis/tractor.csv")

trac
summary(trac)
age <- trac$age
cost <- trac$cost


plot(age, cost, pch=20, cex.lab=1.4, ylab="Cost of Repair", xlab="Age of tractors")


# running regression

lm(cost~age)
abline(lm(cost~age), col="red", lty=2, lwd=2)

# correlation

correlation <- cor(age,cost)
correlation
sd_x <- sd(age)
sd_x 
sd_y <- sd(cost)
sd_y

b1 <- correlation*(sd_y/sd_x)

# b1 = covariance (x,y) / sample variance of x

covv <- cov(age,cost)
var_x <- sd_x^2

b1 <- covv/var_x
b1

#b0 = Mean of Y - b1*Mean of X

x_mean = mean(age)
y_mean = mean (cost)

b0 = y_mean - b1*x_mean


abline(b0, b1, col="blue", lwd=1.5)

legend("bottomright", c("lm line", "Calculated line"), 
       col=c("red", "blue"), lwd=1, lty=c(2,1), bty="n", cex=0.7)
