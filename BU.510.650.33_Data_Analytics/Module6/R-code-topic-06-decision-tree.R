########################################
#### Decision Tree
########################################
library(tree)
library(ISLR)
attach(Carseats)
# data(Carseats)
High=ifelse(Sales <= 8, "No", "Yes")

Carseats2 = data.frame(Carseats, High)
tree.carseats = tree(High~.-Sales, Carseats2)
summary(tree.carseats)

#### plot() display tree structure
plot(tree.carseats)
#### text() display the node labels
## pretty=0 includes the category names
text(tree.carseats,pretty=0)
tree.carseats

#### calcuate training error and testing error
set.seed(2)
train = sample(1:nrow(Carseats2), 200)
Carseats2.test = Carseats2[-train,]
High.test = High[-train]
tree.carseats = tree(High~.-Sales, Carseats2, subset=train)
tree.pred = predict(tree.carseats, Carseats2.test, type="class")
table(tree.pred, High.test)
(86+57)/200


#### prune the tree
set.seed(3)
cv.carseats = cv.tree(tree.carseats, FUN=prune.misclass)
names(cv.carseats)
cv.carseats

par(mfrow=c(1,2))
plot(cv.carseats$size,cv.carseats$dev,type="b")
plot(cv.carseats$k,cv.carseats$dev,type="b")
          
prune.carseats = prune.misclass(tree.carseats,best=9)
par(mfrow=c(1,1))
plot(prune.carseats)
text(prune.carseats,pretty=0)
## pretty=0 instructs R to include category names for qualitative predictors


#### Fitting regression trees
library(MASS)
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2) # split into to equal subsets
tree.boston = tree(medv~., data=Boston, subset=train)
summary(tree.boston)

plot(tree.boston)
text(tree.boston,pretty=0)

## use function cv.tree() function to see whether prunning the tree will improve performance
cv.boston = cv.tree(tree.boston)
names(cv.boston)
summary(cv.boston)
plot(cv.boston$size, cv.boston$dev, type='b')
cv.boston$dev

## prune tree
prune.boston = prune.tree(tree.boston, best=5)
plot(prune.boston)
text(prune.boston, pretty=0)

## make prediction
yhat = predict(tree.boston, newdata=Boston[-train,])
#attach(Boston)
boston.test= Boston[-train,"medv"]
plot(yhat, boston.test)
abline(0,1)
mean((yhat - boston.test)^2)
# the MSE is around 25



#### in-class exercise
lm.boston = lm(medv~lstat + rm + dis, data=Boston, subset=train)
pred.boston = predict(lm.boston, newdata= Boston[-train,])
mean((pred.boston- Boston$medv[-train])^2)
