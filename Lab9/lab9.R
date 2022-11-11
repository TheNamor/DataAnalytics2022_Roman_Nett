library("e1071")
set.seed(1)

x = matrix(rnorm(20*2), ncol=2)
y = c(rep(-1, 10), rep(1, 10))
x[y==1,] = x[y==1,] + 1
x
y

plot(x, col=(3-y))

dat = data.frame(x=x, y=as.factor(y))
svmfit = svm(y~., data=dat, kernel="linear", cost=10, scale=F)

plot(svmfit, dat)

svmfit$index
summary(svmfit)

svmfit = svm(y~., data=dat, kernel="linear", cost=0.1, scale=F)

plot(svmfit, dat)
svmfit$index
summary(svmfit)

set.seed(1)

tune_out = tune(svm, y~., data=dat, kernel="linear", ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune_out)

bestmod = tune_out$best.model
summary(bestmod)

xtest = matrix(rnorm(20*2), ncol=2)
ytest = sample(c(-1,1), 20, rep=T)
xtest[ytest==1,] = xtest[ytest==1,] + 1
testdata = data.frame(x=xtest, y=as.factor(ytest))

ypred = predict(bestmod, testdata)
table(predict=ypred, truth=testdata$y)

svmfit = svm(y~., data=dat, kernel="linear", cost=0.01, scale=F)
ypred = predict(svmfit, testdata)
table(predict=ypred, truth=testdata$y)

x[y==1,] = x[y==1,] + 0.5
plot(x, col=(y+5)/2, pch=19)

dat = data.frame(x=x, y=as.factor(y))
svmfit = svm(y~., data=dat, kernel="linear", cost=1e5)
summary(svmfit)
plot(svmfit, dat)

svmfit = svm(y~., data=dat, kernel="linear", cost=1)
summary(svmfit)
plot(svmfit, dat)
