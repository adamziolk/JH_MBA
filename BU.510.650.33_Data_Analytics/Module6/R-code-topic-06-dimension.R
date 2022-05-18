###########################################################
#### dimension reduction
###########################################################

## PCA sample code
states=row.names(USArrests)
states
names(USArrests)
tail(USArrests)
apply(USArrests,2,mean)
apply(USArrests,2,var)

pr.out=prcomp(USArrests,scale=T)
names(pr.out)
pr.out$center
pr.out$scale
pr.out$rotation
summary(pr.out)
dim(pr.out$x)

##### plot the first two principal components
biplot(pr.out, scale=0)
?biplot

#### mirror image
pr.out$rotation = -pr.out$rotation
pr.out$x = -pr.out$x
biplot(pr.out, scale=0)

#### The prcomp() function also outputs the standard deviation of each principal component.
pr.out$sdev
pr.var=pr.out$sdev^2
pr.var

pve=pr.var/sum(pr.var)
pve

#plot(cumsum(pve))

##### plots
par(mfrow=c(1,2))
plot(pve , xlab=" Principal Component ", ylab=" Proportion of Variance Explained ", 
     ylim=c(0,1) ,type="b")
plot(cumsum (pve ), xlab=" Principal Component ", ylab ="Cumulative Proportion of Variance Explained ", 
     ylim=c(0,1), type="b")

par(mfrow=c(1,1))


##### in-class exercise: Boston data set
library(MASS)
attach(Boston)
boston.pca=prcomp(Boston,scale=T)
summary(boston.pca)

#####


library(pls)
library(ISLR)
pcr.fit=pcr(Salary~.,data=Hitters,scale=T,validation="CV")
summary(pcr.fit)

# plot cross-validation scores
validationplot(pcr.fit,val.type="MSEP")

pcr.fit2=pcr(Salary~.,data=Hitters,scale=T,validation="CV",ncomp=7)
summary(pcr.fit2)

# regression on subset, make predictions
set.seed(1)
Hitters=na.omit(Hitters)
x = model.matrix(Salary~., Hitters)[,-1]
y=Hitters$Salary

train = sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

pcr.fit = pcr(Salary~., data= Hitters, subset=train, scale=T, validation = "CV")
pcr.pred = predict(pcr.fit, x[test,], ncomp=7)

# calculate prediction errors
mean((pcr.pred - y.test)^2)


#### Partial Least Squares
set.seed(1)
Hitters=na.omit(Hitters)
x = model.matrix(Salary~., Hitters)[,-1]
y=Hitters$Salary

train = sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

pls.fit = plsr(Salary ~., data=Hitters, subset=train, scale=TRUE, validation ="CV")
summary(pls.fit)

validationplot(pls.fit,val.type="MSEP")

## evaluate test set MSE
pls.pred = predict(pls.fit,x[test,],ncomp=2)
mean((pls.pred - y.test)^2)

# perform PLS using M=2
pls.fit = plsr(Salary ~., data=Hitters, scale=TRUE, ncomp=2)
summary(pls.fit)




