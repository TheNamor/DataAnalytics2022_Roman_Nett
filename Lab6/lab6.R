
mtcars

dim(mtcars)

head(mtcars)
str(mtcars)

model1 = lm(mpg ~ cyl + wt, data=mtcars)
model1

plot(model1, pch=18, col="red", which=c(4))

cooks.distance(model1)

dist = cooks.distance(model1)

round(dist, 5)
sort(round(dist, 5))

library(ISLR)
library(dplyr)

head(Hitters)
dim(Hitters)
is.na(Hitters)

HittersData = na.omit(Hitters)

dim(HittersData)
str(HittersData)

salaryprediction = lm(Salary ~., data=HittersData)
summary(salaryprediction)

cooksD = cooks.distance(salaryprediction)
influential = cooksD[cooksD > (3 * mean(cooksD, na.rm=T))]
influential

influential_names = names(influential)

outliers = HittersData[influential_names,]

without_outliers = HittersData %>% anti_join(outliers)

model2 = lm(Salary ~., data=without_outliers)
summary(model2)
