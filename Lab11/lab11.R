
data(economics, package="ggplot2")

economics$index = 1:nrow(economics)
economics = economics[1:80,]

loess10 = loess(uempmed ~ index, data=economics, span=0.10)
loess25 = loess(uempmed ~ index, data=economics, span=0.25)
loess50 = loess(uempmed ~ index, data=economics, span=0.50)

smoothed10 = predict(loess10)
smoothed25 = predict(loess25)
smoothed50 = predict(loess50)

plot(economics$uempmed, x=economics$date, type="l", main="Loess Smoothing and Prediction", xlab="Date", ylab="Unemployment (Median)")
lines(smoothed10, x=economics$date, col="red")
lines(smoothed25, x=economics$date, col="green")
lines(smoothed50, x=economics$date, col="blue")

data("cars")

str(cars)

plot(speed ~ dist, data=cars)
plot(dist ~ speed, data=cars)

?lowess

lowess(cars$speed ~ cars$dist)

lines(lowess(cars$speed ~ cars$dist, f=2/3), col="blue")

lines(lowess(cars$speed ~ cars$dist, f=0.8), col="red")
lines(lowess(cars$speed ~ cars$dist, f=0.9), col="green")
lines(lowess(cars$speed ~ cars$dist, f=0.1), col=5)
lines(lowess(cars$speed ~ cars$dist, f=0.01), col=6)

library(MASS)

names(iris)
dim(iris)
head(iris)

set.seed(555)

train = sample(1:nrow(iris), nrow(iris)/2)
iris_train = iris[train,]
iris_test = iris[-train,]

fit1 = lda(Species ~ ., data=iris_train)

predict1 = predict(fit1, iris_train)
predict1_class = predict1$class

table1 = table(predict1_class, iris_train$Species)
table1

sum(diag(table1))/sum(table1)

predict2 = predict(fit1, iris_test)
predict2_class = predict2$class

table2 = table(predict2_class, iris_test$Species)
table2

sum(diag(table2))/sum(table2)
