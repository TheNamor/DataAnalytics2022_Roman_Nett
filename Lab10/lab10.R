library(MASS)
library(tree)
set.seed(1)
str(Boston)
summary(Boston)

train = sample(1:nrow(Boston), nrow(Boston)/2)
tree_boston = tree(medv~., Boston, subset=train)
summary(tree_boston)

plot(tree_boston)
text(tree_boston, pretty=0)

cv_boston = cv.tree(tree_boston)
plot(cv_boston$size, cv_boston$dev, typ="b")

prune_boston = prune.tree(tree_boston, best=5)
plot(prune_boston)
text(prune_boston, pretty=0)

yhat = predict(tree_boston, newdata=Boston[-train,])
boston_test = Boston[-train, "medv"]
plot(yhat, boston_test)
abline(0,1)

mean((yhat-boston_test)^2)

library(randomForest)
set.seed(1)

bag_boston = randomForest(medv~., data=Boston, subset=train, mtry=13, importance=T)
bag_boston

yhat_bag = predict(bag_boston, newdata=Boston[-train,])
plot(yhat_bag, boston_test)
abline(0,1)

mean((yhat_bag-boston_test)^2)

bag_boston = randomForest(medv~., data=Boston, subset=train, mtry=13, ntree=25)
yhat_bag = predict(bag_boston, newdata=Boston[-train,])
mean((yhat_bag-boston_test)^2)

set.seed(1)
rf_boston = randomForest(medv~., data=Boston, subset=train, mtry=6, importance=T)
yhat_rf = predict(rf_boston, newdata=Boston[-train,])
mean((yhat_rf-boston_test)^2)

importance(rf_boston)

varImpPlot(rf_boston)
