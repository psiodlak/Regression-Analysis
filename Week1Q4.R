mkt <- read.csv("/Users/salmansikandar/Documents/Regresion_Analysis/mktmodel.csv")
SP500 <- mkt$SP500
stocks <- mkt[,-1]

mreg <- lm(as.matrix(stocks) ~ SP500)
plot(mreg$coeff[2,], mreg$coeff[1,], col=10, pch=20)
text(x=mreg$coeff[2,], y=mreg$coeff[1,], labels=names(stocks), col=4, pos=1)

# AA Slope
mreg$coeff[2,1]
# All the slopes
mreg$coeff[2,]

# AA Intercept
mreg$coeff[1,1]
# All the Intercepts
mreg$coeff[1,]
mreg

plot(c(-0.002,0.2), c(-0.02,0.2), type = "n", xlab = "x", ylab = "y", asp = 1)
abline(h = 0, v = 0, col = "gray60")

colors <- rainbow(30)
for(i in 1:30){
  abline(a = mreg$coeff[1,i], b = mreg$coeff[2,i], col = colors[i])
}
