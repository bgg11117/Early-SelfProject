library("car")
wine <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data",
                   sep=",")
standardisedconcentrations <- as.data.frame(scale(wine[2:14]))
wine.pca <- prcomp(standardisedconcentrations) 
summary(wine.pca)
wine.pca$sdev
sum((wine.pca$sdev)^2)
screeplot(wine.pca, type="lines")
#Loadings for the Principal Components
wine.pca$rotation[,1]
sum((wine.pca$rotation[,1])^2)
wine.pca$x[,1]
plot(wine.pca$x[,1],wine.pca$x[,2])
text(wine.pca$x[,1],wine.pca$x[,2], wine$V1, cex=0.7, pos=4, col="red") 
