## Best subsect selection
library(ISLR)
names(Hitters)
Hitters2=na.omit(Hitters)
library(leaps)
regfit.full=regsubsets(Salary~.,data=Hitters2,nvmax=15)
reg.summary=summary(regfit.full); reg.summary
### use different approaches: R^2, adjusted R^2, CP, BIC
names(reg.summary)
reg.summary$rsq
which.max(reg.summary$adjr2)
coef(regfit.full,11)
which.min(reg.summary$cp)
which.min(reg.summary$bic)
plot(reg.summary$bic,xlab="Number of Variables",type="l")#### stepwise selection: forward or backward
regfit.fwd=regsubsets(Salary~.,data=Hitters,nvmax=19, method="forward")
summary(regfit.fwd)
regfit.bwd=regsubsets(Salary~.,data=Hitters,nvmax=19, method="backward")
summary(regfit.bwd)
coef(regfit.full,7)
coef(regfit.fwd,7)
coef(regfit.bwd,7)

regfwd_summary = summary(regfit.fwd)
which.max(regfwd_summary$adjr2)
coef(regfit.fwd, 11)
coef(regfit.full, 6)
regbwd_summary = summary(regfit.bwd)
which.min(regbwd_summary$bic)
coef(regfit.bwd, 8)

#### in-class exercise
library(leaps)
Auto=read.csv("Auto.csv",header=T,na.strings="?")
Auto2=na.omit(Auto)

head(Auto2)

my.auto=regsubsets(mpg~.-name, data=Auto2)
coef(my.auto,3)

my.auto2=regsubsets(mpg~.-name, data=Auto2, method="forward")
coef(my.auto2,3)

my.auto3=regsubsets(mpg~.-name, data=Auto2, method="backward")
coef(my.auto3,3)

##### LASSO
grid=10^seq(10,-2,length=100)
names(Hitters2)
x=model.matrix(Salary~.,Hitters2)[,-1]
y=Hitters2$Salary
library(glmnet)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
dim(coef(ridge.mod))

ridge.mod$lambda[50]
coef(ridge.mod)[,50]

ridge.mod$lambda[60]
coef(ridge.mod)[,60]

set.seed(1)
train=sample(1:nrow(x), nrow(x)/2) # split data into two subsets
test=(-train)
y.test=y[test]

ridge.mod=glmnet(x[train,], y[train], alpha=0, lambda=grid,thresh=1e-12)
ridge.pred = predict(ridge.mod, s=4, newx=x[test,])
mean((ridge.pred - y.test)^2)

## fit a ridge regression model with a very large value of lambda
ridge.pred = predict(ridge.mod, s=1e10, newx=x[test,])
mean((ridge.pred - y.test)^2)

## compare to least squares regression/linear regression
ridge.pred = predict(ridge.mod, s=0, newx=x[test,])
mean((ridge.pred - y.test)^2)

lm(y~x, subset=train)
predict(ridge.mod, s=0, exact=T, type="coefficients")[1:20,]


## lasso fit
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)

#### cross validation
Auto=read.csv("Auto.csv",header=T,na.strings="?")
Auto2=na.omit(Auto)

library(ISLR)
set.seed(1) ## generate same random variables
train = sample(392,196) ## divide into two equal subsets
## same as train = sample(1:392,196)

lm.fit = lm(mpg~horsepower, data=Auto2, subset=train)

## subset option: only use the training set to fit model

attach(Auto2)
mean((mpg - predict(lm.fit, Auto2))[-train]^2) # obtain estimated test MSE
# [-train] only select observations that are not in the training set

lm.fit2 = lm(mpg~poly(horsepower, 2), data=Auto2, subset=train)
summary(lm.fit2)
mean((mpg - predict(lm.fit2, Auto2))[-train]^2)

lm.fit3 = lm(mpg~poly(horsepower, 3), data=Auto2, subset=train)
#summary(lm.fit3)
mean((mpg - predict(lm.fit3, Auto2))[-train]^2)

## different training sets give different results
set.seed(2)
train = sample(392,196)

lm.fit = lm(mpg~horsepower, data=Auto2, subset=train)
mean((mpg - predict(lm.fit, Auto2))[-train]^2)

lm.fit2 = lm(mpg~poly(horsepower, 2), data=Auto2, subset=train)
mean((mpg - predict(lm.fit2, Auto2))[-train]^2)

lm.fit3 = lm(mpg~poly(horsepower, 3), data=Auto2, subset=train)
mean((mpg - predict(lm.fit3, Auto2))[-train]^2)
# insights: quadratic performs better than linear; little evidence for cubic model


#### k-fold cross-validation
library(boot) # use package boot, which has function cv.glm() to perform cross-validation
set.seed(17)

glm.fit=glm(mpg~poly(horsepower,2), data=Auto2)
my.cv = cv.glm(Auto2, glm.fit, K=10)

summary(my.cv) ## my.cv$delta[1] is the cross-validation estimate

cv.error.10=rep(0,10)
for (i in 1:10)
{
  glm.fit=glm(mpg~poly(horsepower,i), data=Auto2)
  
  cv.error.10[i] = cv.glm(Auto2, glm.fit, K=10)$delta[1]
}
cv.error.10
plot(cv.error.10, type="b")

which.min(cv.error.10)

?nvmax
?nvmax=15
?head()
Auto=read.csv("Auto.csv",header=T,na.strings="?")
View(Auto)
sum(is.na(Auto))
Auto2=na.omit(Auto)
View(grid)
?seq(10,-2,length=100)
?model.matrix(Salary~.,Hitters2)[,-1]
?glmnet(x,y,alpha=0,lambda=grid)
?ridge.mod$lambda

names(Hitters)
library(ISLR)
sum(is.na(Hitters))
regfit.full=regsubsets(Salary~.,data=Hitters2,nvmax=15)
summary(regfit.full)
View(grid)
View(x)
which.min(ridge.mod)
require(psych)
Auto2=Auto[-c(10:85),]
describe(Auto2)
Auto2=Auto[-c(25:115),]
describe(Auto2)
GMAT=c(560,540,520,580,520,620,660,630,550,550,600,537,610,570,590,650) 
GPA=c(3.2,3.44,3.7,3.1,3.0,4.0,3.38,3.83,2.67,2.75,2.33,3.75,3.85,3.30,3.50,3.65)
mod2=lm(GPA~GMAT); summary(mod2)
predict(mod2,  data.frame(GMAT=580))
set.seed(150)
x=rnorm(150)
Error=rnorm(150,0,0.2)
y=-1.5+0.8 * x + Error 
mod3=lm(y~x);summary(mod3)
mod3$coefficients
predict(mod3, newdata=data.frame(x=1.0), interval="prediction")
