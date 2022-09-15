multivariate = read.csv(file.choose(), header=T)

head(multivariate)
attach(multivariate)

mm = lm(Homeowners~Immigrants)

summary(mm)$coef

plot(mm)

plot(Homeowners~Immigrants)
abline(mm)

library(magrittr)

new_immigrant = data.frame(Immigrants=c(0, 20))
mm %>% predict(new_immigrant)
mm$coefficients
