# HW 1 Q5 Teachers salaries

teachers <- read.csv("/Users/salmansikandar/Documents/Regresion_Analysis/teach.csv")

teachers$salary

# Convert sex into categorical variable
teachers$sex = factor(teachers$sex)

# 5a. Make a plot of salary versus the number of months in service using color, or otherwise, to indicate the sex of each teacher on the plot
plot (teachers$salary, teachers$months, pch=20, col=teachers$sex)
legend ("topleft", levels(teachers$sex), fill=1:2)

# 5b. Boxplots - one for each factor
par(mfrow=c(2,3))
boxplot(teachers$salary ~ teachers$sex, main="Sex", ylab="1970 Salary in Pound Sterling (£)", cex.main=1.3, cex.lab=1.3, cex.axis=1.3, xlab="sex")
boxplot(teachers$salary ~ teachers$marry, main="Marrital Status", ylab="1970 Salary in Pound Sterling (£)", cex.main=1.3, cex.lab=1.3, cex.axis=1.3,xlab="marry")
boxplot(teachers$salary ~ teachers$degree, main="Degree Level", ylab="1970 Salary in Pound Sterling (£)", cex.main=1.3, cex.lab=1.3, cex.axis=1.3,xlab="degree")
boxplot(teachers$salary ~ teachers$type, main="Type of School", ylab="1970 Salary in Pound Sterling (£)", cex.main=1.3, cex.lab=1.3, cex.axis=1.3,xlab="type")
boxplot(teachers$salary ~ teachers$train, main="Special Training", ylab="1970 Salary in Pound Sterling (£)", cex.main=1.3, cex.lab=1.3, cex.axis=1.3,xlab="train")
boxplot(teachers$salary ~ teachers$brk, main="Break in service for two or more years", ylab="1970 Salary in Pound Sterling (£)", cex.main=1.3, cex.lab=1.3, cex.axis=1.3,xlab="brk")

names(teachers)



# 5c. Using color, or otherwise, plot salary versus months in service [similar to (a)] with indications for the levels your chosen factor [from (b)] for each teacher.

# Convert degree into categorical variable
#teachers$degree = factor(teachers$degree)

plot(teachers$months, teachers$salary)
plot (teachers$months, teachers$salary, pch=20, col=teachers$degree, Main="Months in Service vs. Salaries", xlab="Months in service", ylab="1970 Salary in Pound Sterling (£)")
legend ("topleft", levels(teachers$degree), fill=1:4, title="Degree Levels")

#5d. Run regressions
teachers$sex = factor(teachers$sex)
plot (teachers$sex, teachers$salary, xlab="Sex",ylab="Salaries in Pound Sterling")
reg1=lm(teachers$salary ~ teachers$sex)
reg1
abline(reg1,lwd=1.5)

plot (teachers$degree, teachers$salary, xlab="Degree Level",ylab="Salaries in Pound Sterling", pch=20)
reg1=lm(teachers$salary ~ teachers$degree)
reg1
abline(reg1,lwd=1.5)


#5e teachers_new is a subset of teachers where degree == 0
teachers_new <- teachers[ which(teachers$degree=='0'), ]
teachers_new
plot (teachers_new$months, teachers_new$salary, pch=20, col=teachers_new$degree, main="Months vs. Salary Regression where degree == 0", ylab="Salaries in Pound Sterling", xlab="Months")
legend ("topleft", levels(teachers_new$degree), fill=1:4)

# regression line
reg2=lm(teachers_new$salary ~ teachers_new$months)
reg2
abline(reg2,lwd=1.5, col="red")

ress=resid(reg2)
ress
plot (teachers_new$months, ress, pch=20, main="Residuals against Months", ylab="Residuals", xlab="Months")
plot (teachers_new$months, ress, ylab="Residuals")
abline(c(0,0),lwd=1)
abline(reg2,lwd=1.5, col="red")
hist(ress, main="Histogram of Residuals",xlab="Residuals")

# residuals
#plot (teachers_new$months, resid(teachers_new), pch=20, col=teachers_new$degree, main="Residuals against Months", ylab="Residuals", xlab="Months")
teachers_new
residuals(teachers_new)
hist(resid(teachers_new))




help("resid")

teachers$degree = factor(teachers$degree)
plot (teachers$salary, teachers$months, pch=20, col=teachers$degree)
legend ("topleft", levels(teachers$degree), fill=1:4)
