library(cluster)
library(HSAUR)
library(ade4)
library("fpc")
library("factoextra")

clusteringFunctionKmean<-function(name){
df <- read.table(name, header = FALSE)

dimA<-as.numeric(df$V1)
dimB<-as.numeric(df$V2)
myData<-data.frame(dimA,dimB)



kres<-kmeans(myData,3)

plot(myData)

kmeansRes<-factor(kres$cluster)
s.class(myData,fac=kmeansRes, add.plot=TRUE, col=rainbow(nlevels(kmeansRes)))

db <- fpc::dbscan(myData, eps =20, MinPts = 5)

fviz_cluster(db, data = myData, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())

clusters <- hclust(dist(myData[, 1:2]))
plot(clusters)

}


