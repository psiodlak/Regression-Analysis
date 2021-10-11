# Week 2 HW Question2 

help(rnorm)
x=rnorm(100,sd=2)
## first plot
b0=2.5
b1=1.0
y <- b0 - b1*x + rnorm(sd=5)
?qnorm

?runif

summary(y)

#set the true slope and intercept. remember, as the data analyst you *never* know these
#we only do here because we are pretending.
beta.0 <- 2.5
beta.1 <- -1.0

ns <- 100 # sample size
sigma2 <- 2 # variation
# par(mfrow=c(2,2), mai=c(.5,.5,.1,.1))
x <- rnorm(ns,sd=2)
y <- beta.0 + beta.1*x + rnorm(ns, sd=sqrt(sigma2))
plot(x, y, pch=20, xlim=c(-3,3), ylim=c(-5,5))
abline(beta.0, beta.1, col="blue", lwd=1.5)

# for part a
nc <- paste("n=", ns, sep="")
sc <- paste("var=", sigma2, sep="")
legend("bottomright", c(nc, sc,  "true model"), 
       col=c(0, 0, "blue"), lwd=2, lty=0:3, bty="n", cex=0.7)

# can be used
abline(lm(y~x), col="red", lty=2, lwd=2)
nc <- paste("n=", ns, sep="")
sc <- paste("var=", sigma2, sep="")
legend("bottomright", c(nc, sc,  "true model", "LS line"), 
       col=c(0, 0, "blue", "red"), lwd=2, lty=0:3, bty="n", cex=0.7)



# 2 b
ns <- 25 # sample size
sigma2 <- 2 # variation
x <- rnorm(ns,sd=2)
y <- beta.0 + beta.1*x + rnorm(ns, sd=sqrt(sigma2))
abline(lm(y~x), col="red", lty=2, lwd=2)

summary(y)


# 2 b
ns <- 75 # sample size
sigma2 <- 2 # variation
x <- rnorm(ns,sd=2)
y <- beta.0 + beta.1*x + rnorm(ns, sd=sqrt(sigma2))
abline(lm(y~x), col="green", lty=2, lwd=2)
legend("bottomright", c(nc, sc,  "true model", "25 size LS line", "75 size LS line"), 
       col=c(0, 0, "blue", "red", "green"), lwd=1, lty=c(0,0,1,2,2), bty="n", cex=0.7)

summary(y)

#2 c
# Marginal True Mean
summary(y)
# Marginal predicted mean
summary(lm(y~x))

#Paulina comment - I calculated the summary(y) for n=100, n=25, and n=75

