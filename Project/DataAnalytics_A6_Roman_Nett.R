library("readxl")
library("ggpubr")
library("ggplot2")
library("plotly")
library("reshape2")
library("randomForest")

EPI_full = read_excel("Data/2020-epi.xlsx",col_names=T)
sanitation_full = read.csv("Data/sanitation.csv")

total = merge(EPI_full, sanitation_full, by="iso")

hist(total$SCORE, breaks=60, xlab="Score")

ggplot(total, aes(SCORE)) + geom_histogram(binwidth=1, fill="#02bd21") + ggtitle("Score Distribution")

plot(total$SCORE, total$UWD.new)

cor(total$SCORE, total$UWD.new)
cor(total$SCORE, total$USD.new)
cor(total$SCORE, total$H2O.new)

total_filtered = total[grepl("change", colnames(total)) == F & grepl("rnk", colnames(total)) == F]

# HLT PMD HAD HMT PBD WMG MSW ECO BDH TBN SHI SPI BHV ECS TCL GRL WTL FSH FSS RMS APE AGR SNM WWT

total_filtered[colnames(total_filtered)[-c(96, 1, 2, 3)]] = sapply(total_filtered[colnames(total_filtered)[-c(96, 1, 2, 3)]], as.numeric)

correlations = cor(total_filtered[colnames(total_filtered)[-c(96, 1, 2, 3)]], total_filtered$SCORE)

colnames(total_filtered)[-c(96, 1, 2, 3)][is.na(correlations)==F][which.min(abs(correlations[is.na(correlations)==F]))]

plot(total_filtered$SCORE, total_filtered$GIB.new)

corframe = data.frame(melt(as.matrix(abs(correlations))))

sorted = corframe[order(corframe$value),]
sorted

cor(total_filtered$SCORE[!is.na(total_filtered$MPA.new)], total_filtered$MPA.new[!is.na(total_filtered$MPA.new)])
cor(total_filtered$SCORE[!is.na(total_filtered$SHI.new)], total_filtered$SHI.new[!is.na(total_filtered$SHI.new)])
cor(total_filtered$SCORE[!is.na(total_filtered$SPI.new)], total_filtered$SPI.new[!is.na(total_filtered$SPI.new)])
cor(total_filtered$SCORE[!is.na(total_filtered$ECS.new)], total_filtered$ECS.new[!is.na(total_filtered$ECS.new)])
cor(total_filtered$SCORE[!is.na(total_filtered$TCL.new)], total_filtered$TCL.new[!is.na(total_filtered$TCL.new)])
cor(total_filtered$SCORE[!is.na(total_filtered$GRL.new)], total_filtered$GRL.new[!is.na(total_filtered$GRL.new)])
cor(total_filtered$SCORE[!is.na(total_filtered$WTL.new)], total_filtered$WTL.new[!is.na(total_filtered$WTL.new)])
cor(total_filtered$SCORE[!is.na(total_filtered$FSH.new)], total_filtered$FSH.new[!is.na(total_filtered$FSH.new)])
cor(total_filtered$SCORE[!is.na(total_filtered$FSS.new)], total_filtered$FSS.new[!is.na(total_filtered$FSS.new)])
cor(total_filtered$SCORE[!is.na(total_filtered$RMS.new)], total_filtered$RMS.new[!is.na(total_filtered$RMS.new)])
cor(total_filtered$SCORE[!is.na(total_filtered$FGT.new)], total_filtered$FGT.new[!is.na(total_filtered$FGT.new)])
cor(total_filtered$SCORE[!is.na(total_filtered$FGA.new)], total_filtered$FGA.new[!is.na(total_filtered$FGA.new)])
cor(total_filtered$SCORE[!is.na(total_filtered$LCB.new)], total_filtered$LCB.new[!is.na(total_filtered$LCB.new)])

# FSH, TCL, GIB, ECS, SHI
sum(is.na(total_filtered$FSH.new))
sum(is.na(total_filtered$GRL.new))

plot(total_filtered$SCORE, total_filtered$TCL.new, xlab="Greehouse Gas Intensity Growth", ylab="Score")
title("Greehouse Gas Intensity Growth vs Score")

plot(no_nas$FSH.new, no_nas$GIB.new)

no_nas = total_filtered[!is.na(total_filtered$FSH.new) & !is.na(total_filtered$TCL.new),]
cols = grepl("new", colnames(no_nas))
cols[length(cols)] = T
no_nas = no_nas[cols]
no_nas = select(no_nas, -c("UWD.new", "USD.new", "H2O.new"))

cor(no_nas$FSH.new, no_nas$TCL.new)
cor(no_nas$FSH.new, no_nas$GIB.new)
cor(no_nas$GIB.new, no_nas$TCL.new)

boxplot(no_nas$FSH.new)

cluster_model = kmeans(no_nas[c("FSH.new", "TCL.new", "GIB.new")], 3)

ggplot(no_nas[c("FSH.new", "TCL.new", "GIB.new")], aes(x=FSH.new, y=TCL.new)) + geom_point(data=no_nas[c("FSH.new", "TCL.new", "GIB.new")], color=cluster_model$cluster)

#KDD acm conference
withinsss = data.frame(matrix(ncol=2, nrow=0))
colnames(withinsss) = c("Clusters", "withinss")
for (i in 2:10) {
  model = kmeans(no_nas[c("FSH.new", "TCL.new", "GIB.new")], i)
  withinsss[nrow(withinsss)+1,] = c(i, model$tot.withinss)
}

ggplot(data=withinsss, aes(x=Clusters, y=withinss)) + geom_line() + ggtitle("Within Cluster Sum of Squares over Cluster Number")

set.seed(1000)
best_clusters = kmeans(no_nas[c("FSH.new", "TCL.new", "GIB.new")], 3)

plot_ly(x=no_nas$FSH.new, 
        y=no_nas$TCL.new, 
        z=no_nas$GIB.new, 
        type="scatter3d", mode="markers", color=best_clusters$cluster)%>%
  layout( scene= list(
    xaxis=list(title="Fisheries"),
    yaxis=list(title="Tree Cover Loss"),
    zaxis=list(title="Greenhouse Gas")
  ))

plot_ly(x=no_nas$FSH.new, 
        y=no_nas$TCL.new, 
        z=no_nas$GIB.new, 
        type="scatter3d", mode="markers", color=no_nas$SCORE)%>%
  layout( scene= list(
    xaxis=list(title="Fisheries"),
    yaxis=list(title="Tree Cover Loss"),
    zaxis=list(title="Greenhouse Gas")
  ))

# Cluster data
cluster1 = no_nas[best_clusters$cluster == 1,]
cluster2 = no_nas[best_clusters$cluster == 2,]
cluster3 = no_nas[best_clusters$cluster == 3,]

par(mfcol=c(1, 3))
boxplot(cluster1[c("FSH.new", "TCL.new", "GIB.new")])
title("Cluster 1")
boxplot(cluster2[c("FSH.new", "TCL.new", "GIB.new")], ylim=c(1,100))
title("Cluster 2")
boxplot(cluster3[c("FSH.new", "TCL.new", "GIB.new")])
title("Cluster 3")

sd(cluster1$SCORE)
sd(cluster2$SCORE)
sd(cluster3$SCORE)
sd(no_nas$SCORE)

mean(no_nas$SCORE)
mean(cluster1$SCORE)
mean(cluster2$SCORE)
mean(cluster3$SCORE)

colMeans(cluster1[c("FSH.new", "TCL.new", "GIB.new")])
colMeans(cluster2[c("FSH.new", "TCL.new", "GIB.new")])
colMeans(cluster3[c("FSH.new", "TCL.new", "GIB.new")])

apply(cluster1[c("FSH.new", "TCL.new", "GIB.new")], 2, sd)
apply(cluster2[c("FSH.new", "TCL.new", "GIB.new")], 2, sd)
apply(cluster3[c("FSH.new", "TCL.new", "GIB.new")], 2, sd)

#Cluster 1
cluster1_nonas = cluster1[, colSums(is.na(cluster1))==0]

ggplot(cluster1_nonas, aes(SCORE)) + geom_histogram(binwidth=1, fill="#02bd21") + ggtitle("Cluster 1 Score Distribution")

mses = c()
for (i in 3:35) {
  rf = randomForest(SCORE ~ ., cluster1_nonas, mtry = i)
  predicted = predict(rf, cluster1_nonas)
  mses[i-2] = mean((predicted - cluster1_nonas$SCORE)^2)
}

which.min(mses) + 2

mses = c()
for (i in 1:7) {
  ntree = c(100, 200, 300, 400, 500, 600, 700)[i]
  rf = randomForest(SCORE ~ ., cluster1_nonas, mtry = 30, ntree = ntree)
  predicted = predict(rf, cluster1_nonas)
  mses[i] = mean((predicted - cluster1_nonas$SCORE)^2)
}

mses

mses = c()
for (i in 1:23) {
  rf = randomForest(SCORE ~ ., cluster1_nonas[-i,], mtry = 30, ntree = 400)
  predicted = predict(rf, cluster1_nonas[i,])
  mses[i] = mean((predicted - cluster1_nonas$SCORE[i])^2)
}

mean(mses)

rf1 = randomForest(SCORE ~ ., cluster1_nonas, mtry = 30, ntree = 400)
predicted = predict(rf1, cluster1_nonas)
plot(predicted, cluster1_nonas$SCORE, xlim=c(60, 100), xlab="Predicted Score", ylab="Actual Score")
abline(0,1)
title("Cluster 1 Fit")
mean((predicted - cluster1_nonas$SCORE)^2)
cor(predicted, cluster1_nonas$SCORE)

rf1

importance(rf1)
# CHA >>> HLT > CCH
varImpPlot(rf1, main="Variable importance for Cluster 1")

plot(cluster1_nonas$CCH.new, cluster1_nonas$SCORE)

#Cluster 2
cluster2_nonas = cluster2[, colSums(is.na(cluster2))==0]

ggplot(cluster2_nonas, aes(SCORE)) + geom_histogram(binwidth=1, fill="#02bd21") + ggtitle("Cluster 2 Score Distribution")

mses = c()
for (i in 3:35) {
  rf = randomForest(SCORE ~ ., cluster2_nonas, mtry = i)
  predicted = predict(rf, cluster2_nonas)
  mses[i-2] = mean((predicted - cluster2_nonas$SCORE)^2)
}

which.min(mses) + 2

mses = c()
for (i in 1:7) {
  ntree = c(100, 200, 300, 400, 500, 600, 700)[i]
  rf = randomForest(SCORE ~ ., cluster2_nonas, mtry = 19, ntree = ntree)
  predicted = predict(rf, cluster2_nonas)
  mses[i] = mean((predicted - cluster2_nonas$SCORE)^2)
}

which.min(mses)*100

mses = c()
for (i in 1:64) {
  rf = randomForest(SCORE ~ ., cluster2_nonas[-i,], mtry = 19, ntree = 400)
  predicted = predict(rf, cluster2_nonas[i,])
  mses[i] = mean((predicted - cluster2_nonas$SCORE[i])^2)
}

mean(mses)

rf2 = randomForest(SCORE ~ ., cluster2_nonas, mtry = 19, ntree = 400)
predicted = predict(rf2, cluster2_nonas)
plot(predicted, cluster2_nonas$SCORE, xlim=c(47.7, 100), xlab="Predicted Score", ylab="Actual Score")
abline(0,1)
title("Cluster 2 Fit")
mean((predicted - cluster2_nonas$SCORE)^2)
cor(predicted, cluster2_nonas$SCORE)

rf2

# HAD HLT >>> EPI > WWT WRS
varImpPlot(rf2)

plot(cluster2_nonas$EPI.new, cluster2_nonas$SCORE)

#Cluster 3
cluster3_nonas = cluster3[, colSums(is.na(cluster3))==0]

ggplot(cluster3_nonas, aes(SCORE)) + geom_histogram(binwidth=1, fill="#02bd21") + ggtitle("Cluster 3 Score Distribution")

mses = c()
for (i in 3:34) {
  rf = randomForest(SCORE ~ ., cluster3_nonas, mtry = i)
  predicted = predict(rf, cluster3_nonas)
  mses[i-2] = mean((predicted - cluster3_nonas$SCORE)^2)
}

which.min(mses) + 2

mses = c()
for (i in 1:7) {
  ntree = c(100, 200, 300, 400, 500, 600, 700)[i]
  rf = randomForest(SCORE ~ ., cluster3_nonas, mtry = 26, ntree = ntree)
  predicted = predict(rf, cluster3_nonas)
  mses[i] = mean((predicted - cluster3_nonas$SCORE)^2)
}

which.min(mses)*100

mses = c()
for (i in 1:24) {
  rf = randomForest(SCORE ~ ., cluster3_nonas[-i,], mtry = 26, ntree = 600)
  predicted = predict(rf, cluster3_nonas[i,])
  mses[i] = mean((predicted - cluster3_nonas$SCORE[i])^2)
}

mean(mses)

rf3 = randomForest(SCORE ~ ., cluster3_nonas, mtry = 26, ntree = 600)
predicted = predict(rf3, cluster3_nonas)
plot(predicted, cluster3_nonas$SCORE, xlim=c(41.55, 100), xlab="Predicted Score", ylab="Actual Score")
abline(0,1)
title("Cluster 3 Fit")
mean((predicted - cluster3_nonas$SCORE)^2)
cor(predicted, cluster3_nonas$SCORE)

rf3

# EPI >>> HAD SDA GHP
varImpPlot(rf3)

cor(cluster3_nonas$GHP.new, cluster3_nonas$SCORE)

# kdd 4 different regression types (svm, xboost, multivariate)
# clustering (mean shift, compare clusters - adjusted rand index)