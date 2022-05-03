#######################################################
# We can analyze th Smarket data in many ways.
# Here we present 2 examples
require(ISLR)
Smarket=Smarket
names(Smarket)
head(Smarket)
attach(Smarket)
#####################################################
# Logistic Model
logistic.fit=glm(Direction~Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family = binomial, data=Smarket)
summary(logistic.fit)
pchisq(1731.2-1727.6, 1249-1243)
names(logistic.fit)
pchisq(logistic.fit$null.deviance-logistic.fit$deviance, logistic.fit$df.null-logistic.fit$df.residual)
train.data=subset(Smarket, Year<2005)
test.data=subset(Smarket, Year==2005)
train.mod=glm(Direction~Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family = binomial, data=train.data)
test.probs=predict(train.mod, test.data, type="response")
head(test.probs)
require(psych)
describe(test.probs)
pred.directions.test=ifelse(test.probs<=0.5, "Down", "Up")
head(pred.directions.test)
mean(pred.directions.test==test.data$Direction)
table(pred.directions.test, test.data$Direction)
######################################################
# KNN Approach
require(class)
knn1.fit=knn(train.data[,c(2:7)], test.data[,c(2:7)], train.data$Direction, 1)
head(knn1.fit)
OneNN.error = 1 - mean(knn1.fit==test.data$Direction);OneNN.error
# Consider higher values of k
knn3.fit=knn(train.data[,c(2:7)], test.data[,c(2:7)], train.data$Direction, 3)
ThreeNN.error = 1 - mean(knn3.fit==test.data$Direction); ThreeNN.error
knn5.fit = knn(train.data[,c(2:7)], test.data[,c(2:7)], train.data$Direction, 5)
FiveNN.error = 1 - mean(knn5.fit==test.data$Direction); FiveNN.error
knn7.fit = knn(train.data[,c(2:7)], test.data[,c(2:7)], train.data$Direction, 7)
SevenNN.error = 1 - mean(knn7.fit==test.data$Direction); SevenNN.error
plot(c(1,3,5,7), c(OneNN.error, ThreeNN.error, FiveNN.error, SevenNN.error), type="b")
######################################################
# K-means clusters with simulated data
set.seed(1)
x.coords=(rnorm(50))
y.coords=(rnorm(50))
group.num=c(1,2)
points=data.frame(cbind(group.num, x.coords, y.coords))
View(points)
plot(points$x.coords, points$y.coords, col=group.num, pch=16)
points$x.coords[c(1:25)] = points$x.coords[c(1:25)]+3
points$y.coords[c(1:25)] = points$y.coords[c(1:25)]-4
plot(points$x.coords, points$y.coords, col=points$group.num, pch=16)
points$group.num[c(1:25)]=1
points$group.num[c(26:50)]=2
plot(points$x.coords, points$y.coords, col=points$group.num, pch=16)
km.out=kmeans(points, 2, nstart=20)
plot(points$x.coord, points$y.coord, col=km.out$cluster, pch=16, main="New Plot")
names(km.out)
km.out$cluster
km.out$totss
km.out$withinss
km.out$tot.withinss
km.out$betweenss
right = mean(points$group.num==km.out$cluster); c(right, 1-right)
# Consider 3 clusters
km3.out=kmeans(points, 3, nstart=20)
plot(points$x.coord, points$y.coord, col=km3.out$cluster, pch=16)
c(km.out$totss, km.out$tot.withinss, km.out$betweenss)
c(km3.out$totss, km3.out$tot.withinss, km3.out$betweenss)
##################################################
# Another clustering example: Red and White Wines
##################################################
wine <- read.csv("wine.csv")
require(psych)
describe(wine)
summary(wine$color)
# Question: can we tell the difference between Red wines and White wines without looking at the color?
# Fit wines into 2 clusters based on quantitative differences and see if clusters match colors.
wine.data=wine[,1:11]
km2.out=kmeans(wine.data, 2, nstart = 10)
table(km2.out$cluster, wine$color)
color.code=ifelse(wine$color=="red", 1, 2)
table(km2.out$cluster, color.code)
wrong=mean(km2.out$cluster==color.code); c(wrong, 1-wrong)
km2.out$centers
# When some predictors have a big range (max - min), they dominate the calculations.
# This may lead to misleading results.
# We can scale the values and see if it works better.
xwine <- scale(wine[,1:11])
# Scale finds mean and sd of each variable.
# Then for each point it subtracts the mean and divides by sd.
describe(xwine)

## fit two clusters
two <- kmeans(xwine,2,nstart=10)
two$centers # big differences on all accounts
# what is the color distribution in each?
table(two$cluster, wine$color)
correct=mean(two$cluster==color.code); c(correct, 1-correct)
# the two clusters are essentially red v. white!

# randomize order in plot, just so its not all white on top of red
# note that my cluster 1 was red; this could be flipped for you.
i <- sample(1:nrow(xwine))  
plot(wine$fixed.acidity[i], wine$volatile.acidity[i],
     pch=21, cex=1.5, bty="n",
     xlab="fixed acidity",
     ylab="volatile acidity",
     bg=c("maroon","gold")[two$cluster[i]],
     col=c("maroon","gold")[wine$color[i]])

###############################################################
# Hierarchical clusters using Smarket data
hc.complete=hclust(dist(test.data[,2:7]),method="complete")
plot(hc.complete)
#######################################################
#### example: USArrests data
#######################################################
USArrests = USArrests
View(USArrests)
states=rownames(USArrests)
arrest.reasons=colnames(USArrests)
hc.complete=hclust(dist(USArrests),method="complete")
plot(hc.complete)

cutree(hc.complete,3)

USArrests2=scale(USArrests)
hc.complete.2=hclust(dist(USArrests2),method="complete")
plot(hc.complete.2 ,main="Hierachical Clustering with Standardization")
cutree(hc.complete.2,4)
