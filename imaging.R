flower = read.csv("flower.csv", header=FALSE)
str(flower)
flowerMatrix = as.matrix(flower)
str(flowerMatrix)
flowerVector = as.vector(flowerMatrix)
str(flowerVector)
distance = dist(flowerVector, method = "euclidian")
clusterIntensity = hclust(distance, method="ward")
plot(clusterIntensity)
rect.hclust(clusterIntensity, k=3, boarder="red")
flowerClusters = cutree(clusterIntensity, k=3)
flowerClusters
tapply(flowerVector, flowerClusters, mean)
dim(flowerClusters) = c(50,50)
image(flowerClusters, axes=FALSE)
image(flowerMatrix, axes=FALSE, col=grey(seq(0,1,length=256)))


healthy = read.csv("healthy.csv", header=FALSE)
healthyMatrix = as.matrix(healthy)
image(healthyMatrix, axes=FALSE, col = grey(seq(0,1,length=256)))
healthyVector = as.vector(healthyMatrix)
distance = dist(healthyVector, method="euclidian")
str(healthyVector)

k=5
set.seed(1)
KMC = kmeans(healthyVector, centers=k, iter.max=1000)
str(KMC)
healthyClusters = KMC$cluster
KMC$centers[2]
dim(healthyClusters) = c(nrow(healthyMatrix), ncol(healthyMatrix))
image(healthyClusters, axes=FALSE, col = rainbow(k))

tumor = read.csv("tumor.csv", header=FALSE)
tumorMatrix = as.matrix(tumor)
tumorVector = as.vector(tumorMatrix)
install.packages("flexclust")
library(flexclust)
KMC.kcca = as.kcca(KMC, healthyVector)
tumorClusters = predict(KMC.kcca, newdata=tumorVector)
dim(tumorClusters) = c(nrow(tumorMatrix), ncol(tumorMatrix))
image(tumorClusters, axes=FALSE, col=rainbow(k))
