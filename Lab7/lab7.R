
library(rpart)
require(party)

df = as.data.frame(Titanic)
Titanic_df = df[rep(1:nrow(df), df$Freq), -5]

fit_rpart = rpart(Survived ~ ., data=Titanic_df)

printcp(fit_rpart) # display the results
plotcp(fit_rpart) # visualize cross-validation results
summary(fit_rpart) # detailed summary of splits

plot(fit_rpart, uniform=TRUE, main="Classification Tree for Titanic")
text(fit_rpart, use.n=TRUE, all=TRUE, cex=.8)

fit_ctree = ctree(Survived ~ ., data=Titanic_df)

summary(fit_ctree)
plot(fit_ctree, uniform=TRUE, main="CI Tree for Titanic")
text(fit_ctree, use.n=TRUE, all=TRUE, cex=.8)

dist_Titanic = Titanic_df[c("Sex", "Age", "Class")]
dist_Titanic$Sex = gsub("Male", 0, dist_Titanic$Sex)
dist_Titanic$Sex = gsub("Female", 1, dist_Titanic$Sex)
dist_Titanic$Age = gsub("Child", 0, dist_Titanic$Age)
dist_Titanic$Age = gsub("Adult", 1, dist_Titanic$Age)
dist_Titanic$Class = gsub("1st", 0, dist_Titanic$Class)
dist_Titanic$Class = gsub("2nd", 0.33, dist_Titanic$Class)
dist_Titanic$Class = gsub("3rd", 0.67, dist_Titanic$Class)
dist_Titanic$Class = gsub("Crew", 1, dist_Titanic$Class)

dist_Titanic$Sex = as.numeric(dist_Titanic$Sex)
dist_Titanic$Age = as.numeric(dist_Titanic$Age)
dist_Titanic$Class = as.numeric(dist_Titanic$Class)

fit_hclust = hclust(dist(dist_Titanic))

plot(fit_hclust)
text(fit_hclust, use.n=T, all=T, cex=.8)
