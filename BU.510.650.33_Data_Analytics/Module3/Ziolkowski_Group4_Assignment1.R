# Question 1
Auto <- read.csv('Auto.csv', na.strings='?')
summary(Auto)


# Question 2
require(psych)
d_auto = describe(Auto)
q2 = data.frame(cbind(rownames(d_auto), d_auto$min, d_auto$max, d_auto$median))
colnames(q2) = c("Variable", "Min", "Max", "Median")
q2


# Question 3
q3 = data.frame(cbind(rownames(d_auto), d_auto$mean, d_auto$sd))
colnames(q3) = c("Variable", "Mean", "Std")
q3                


# Question 4
Auto2 <- Auto[-c(25:115),]
da2 = describe(Auto2)
q4 = data.frame(cbind(rownames(da2), da2$min, da2$max, da2$median, da2$mean, da2$sd))
colnames(q4) = c("Variable", "Min", "Max", "Median", "Mean", "Std")
q4


# Question 5
Auto3 <- Auto
Auto3$name <- NULL
regfit_fwd=regsubsets(mpg~.,data=Auto3, method="forward")
reg_summary = summary(regfit_fwd)
reg_summary$which

full_model <- lm(mpg ~ ., data = Auto3)
summary(full_model)

plot(reg_summary$bic,xlab="Number of Variables",type="l")


# Question 6
gmat <- c(560,540,520,580,520,620,660,630,550,550,600,537,610,570,590,650)
gpa <- c(3.2,3.44,3.7,3.1,3,4,3.38,3.83,2.67,2.75,2.33,3.75,3.85,3.3,3.5,3.65)
data6 <- data.frame(cbind(gmat, gpa))
lm6 <- lm(gpa ~ gmat, data=data6)
summary(lm6)


# Question 7
predict(lm6, data.frame(gmat=580))


# Question 9
set.seed(0) # For reproducability
x <- rnorm(150)
error <- rnorm(150, 0, .2)
y = -1.5 + 0.8*x + error

lm9 <- lm(y~x)
summary(lm9)


# Question 10
confint(lm9)

predict(lm9, data.frame(x=1), interval="prediction")
