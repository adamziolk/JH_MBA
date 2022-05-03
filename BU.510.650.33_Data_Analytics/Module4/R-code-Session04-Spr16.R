# Q2
library(ISLR)
attach(Smarket)

train=(Year<2005)
Smarket.2005=Smarket[!train,]
Direction.2005=Direction[!train]

glm.fit=glm(Direction~Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
            family=binomial,data=Smarket,subset=train)
glm.probs=predict(glm.fit,newdata=Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
mean(glm.pred==Direction.2005)



glm.fit2=glm(Direction~Lag1 + Lag2,
            family=binomial,data=Smarket,subset=train)
glm.probs2=predict(glm.fit2,newdata=Smarket.2005,type="response")
glm.pred2=rep("Down",252)
glm.pred2[glm.probs2>.5]="Up"
mean(glm.pred2==Direction.2005)







# logistic regression & classification

getwd()

################################################
#### Stock Market Data
################################################

library(ISLR)
attach(Smarket)
head(Smarket)
tail(Smarket)
summary(Smarket)

cor(Smarket)
cor(Smarket[,-9])

#### plots
plot(Smarket)
plot(Smarket$Today~Smarket$Lag1)
plot(Smarket$Lag1,Smarket$Today)
hist(Smarket$Today)
hist(Smarket$Today,breaks=5)
hist(Today,breaks=5)
hist(Today,breaks=10)
hist(Today,breaks=100)

#### logistic regression
glm.fit=glm(Direction~Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
            family=binomial,data=Smarket)
summary(glm.fit)

predict(glm.fit,type="response")->glm.probs
head(glm.probs)
options("digits"=2)
glm.probs[1:6]
contrasts(Direction)

glm.pred=rep("Down",1250)
glm.pred[glm.probs>.5]="Up"
head(glm.pred)
head(Direction)
table(glm.pred,Direction)
(145+507)/1250
mean(glm.pred==Direction)

levels(Smarket$Year)
class(Smarket$Year)

train=(Year<2005)
Smarket.2005=Smarket[!train,]
dim(Smarket.2005)
Direction.2005=Direction[!train]

head(Direction.2005)
length(Direction.2005)
glm.fit=glm(Direction~Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
            family=binomial,data=Smarket,subset=train)
glm.probs=predict(glm.fit,newdata=Smarket.2005,type="response")
glm.pred[glm.probs>.5]="Up"
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
mean(glm.pred==Direction.2005)

# improve prediction accuracy
glm.fit=glm(Direction ~ Lag1+Lag2, data=Smarket, family=binomial,subset=train)
glm.probs=predict(glm.fit,Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)


############## Auto Data Set
Auto=read.csv("Auto.csv",header=T,na.strings="?")
Auto2=na.omit(Auto) # remove missing values

mpg.median=median(Auto2$mpg)
mpg01 <- ifelse(Auto2$mpg > mpg.median, 1, 0)

Auto3=data.frame(mpg01,Auto2[,-1]) # create a new data frame
head(Auto3)

# explore the data w.r.t. mgp01
boxplot(mpg01~cylinders, data=Auto3)

plot(mpg01~weight, data=Auto3)
plot(mpg01~horsepower, data=Auto3)
plot(mpg01~acceleration, data=Auto3)

## It seems that cylinders,weight,horsepower and acceleration are associated with mpg01.

train= seq(1,nrow(Auto3)/2) # create indeces for training data set
train

test= seq(nrow(Auto3)/2+1, nrow(Auto3)) 
test

# perform logistic regression
logit.auto=glm(mpg01~ cylinders+weight+horsepower +acceleration, family="binomial",data=Auto3)
summary(logit.auto)

# p-value for acceleration is not significant, re-perform logistic regression without it
logit.auto=glm(mpg01~ cylinders+weight+horsepower, family="binomial",data=Auto3)
summary(logit.auto)

# you may also want to remove cylinders
logit.auto=glm(mpg01~ weight+horsepower, family="binomial",data=Auto3)
summary(logit.auto)

# perform logistic regression with training data, use testing data for accuracy
logit.auto2=glm(mpg01~ weight+horsepower, family="binomial",data=Auto3,subset=train)
auto.probs=predict(logit.auto2,newdata=Auto3[test,],type="response")
auto.pred=rep("Down",length(test))
auto.pred[auto.probs>.5]="Up"
table(auto.pred,Auto3$mpg01[test]) ->test.table
prediction.accuracy <- (test.table[1,1]+test.table[2,2])/sum(test.table)
prediction.accuracy

#################################################
#### multinomial logistic regression
#################################################
library(VGAM) ## VGAM to estimate multinomial logistic regression
library(textir)## to standardize the features
library(MASS)## a library of example datasets
data(fgl)## loads the data into R; see help(fgl)
fgl[1:3,]
gg <- vglm(type ~ Na+Mg+Al,multinomial,data=fgl)
summary(gg)

dWinF=fgl$type=="WinF"
dWinNF=fgl$type=="WinNF"
dVeh=fgl$type=="Veh"
dCon=fgl$type=="Con"
dTable=fgl$type=="Tabl"
dHead=fgl$type=="Head"
yy1=c(fitted(gg)[dWinF,1],fitted(gg)[dWinNF,2],fitted(gg)[dVeh,3], fitted(gg)[dCon,4],fitted(gg)[dTable,5],fitted(gg)[dHead,6])
xx1=c(fgl$type[dWinF],fgl$type[dWinNF],fgl$type[dVeh], fgl$type[dCon], fgl$type[dTable], fgl$type[dHead])
boxplot(yy1~xx1,ylim=c(0,1),xlab="1=WinF,2=WinNF,3=Veh, 4=Con,5=Table,6=Head")

# more boxplots
par(mfrow=c(2,3))
plot(RI ~ type, data=fgl, col=c(grey(.2),2:6))
plot(Al ~ type, data=fgl, col=c(grey(.2),2:6))
plot(Na ~ type, data=fgl, col=c(grey(.2),2:6))
plot(Mg ~ type, data=fgl, col=c(grey(.2),2:6))
plot(Ba ~ type, data=fgl, col=c(grey(.2),2:6))
plot(Si ~ type, data=fgl, col=c(grey(.2),2:6))
par(mfrow=c(1,1))
#####################################################
#### K-Nearest Neighbors: Example
#####################################################
library(ISLR)
attach(Smarket)
data(Smarket)

train=(Year<2005)
Smarket.2005=Smarket[!train,]
dim(Smarket.2005)

###### k-nearest neighbor
library(class)
train.X=cbind(Lag1,Lag2)[train,]
test.X=cbind(Lag1,Lag2)[!train,]
train.Direction=Direction[train]
Direction.2005=Direction[!train]
set.seed(1)
knn.pred=knn(train.X,test.X,train.Direction,k=1)
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)

knn.pred=knn(train.X,test.X,train.Direction,k=3)
mean(knn.pred==Direction.2005)






