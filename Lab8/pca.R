
data("iris")
head(iris)

iris1 = iris[,1:4]
iris1
head(iris1)

?princomp
princomponents = princomp(iris1, cor=T, score=T)
summary(princomponents)

plot(princomponents)

?biplot
biplot(princomponents)

wine_data = read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", sep = ",")
head(wine_data)

nrow(wine_data)
dim(wine_data)
str(wine_data)

colnames(wine_data) = c("Cvs", "Alcohol",
                        "Malic_Acid", "Ash", "Alkalinity_of_Ash",
                        "Magnesium", "Total_Phenols", "Flavanoids", "NonFlavanoid_Phenols",
                        "Proanthocyanins", "Color_Intensity", "Hue", "OD280/OD315_of_Diluted_Wine",
                        "Proline")
head(wine_data)
summary(wine_data)

?heatmap
?cor
heatmap(cor(wine_data), Rowv=NA, Colv=NA)
cor(wine_data)

?factor

cultivar_classes = factor(wine_data$Cvs)
cultivar_classes

?prcomp
?scale

wine_pca = prcomp(scale(wine_data[,-1]))
summary(wine_pca)

plot(wine_pca)
biplot(wine_pca)
