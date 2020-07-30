BiocManager::install("multtest")
library("multtest")
data(golub)
rowName<-golub.gnames[9,]
rowName
rowName[3]
golub[9,]
averageExpression <-mean(golub[9,])
averageExpression
categorize <- factor(golub.cl, levels=0:1, labels = c("class 0","class 1"))
golub[9, categorize =="class 0"]
golub[9, categorize =="class 1"]
boxplot(golub[9,] ~ categorize)
myList<-c()
i<-1
while (i<=3051){
if(i==9) c<-1
else myList<-c(myList,cor(golub[9,],golub[i,]))
i<-i+1
}
maximum<-sort(myList,decreasing = TRUE)
maximum[1:10]
minimum<-sort(myList,decreasing = FALSE)
minimum[1:10]
frameNew<-data.frame(maximum[1:10],minimum[1:10])
frameNew
d1<-dist(frameNew,method=“euclidean")
d1<-dist(frameNew,method="euclidean")
dendogram<hclust(d1,method="average",member=NULL)
dendogram<-hclust(d1,method = "average",members = NULL)
plot( dendogram)
km<-kmeans(frameNew,centers = 3 )
boxplot (t(frameNew),col=km$cluster)
km<-kmeans(frameNew,centers = 3 )
boxplot (frameNew,col=km$cluster)
km<-kmeans(frameNew,centers =4  )
boxplot (frameNew,col=km$cluster)
heatmap(as.matrix(t(frameNew)))
heatmap(as.matrix(frameNew))
result<-prcomp(frameNew,center=FALSE)
scores<-result$rotation
plot(scores[,1],scores[,2] ,xlab=“pc1",ylab="pc2")
plot(scores[,1],scores[,2] ,xlab= "pc1" ,ylab="pc2")
result<-prcomp(t(frameNew),center=FALSE)
scores<-result$rotation
plot(scores[,1],scores[,2] ,xlab=“pc1",ylab="pc2")
plot(scores[,1],scores[,2] ,xlab= "pc1" ,ylab="pc2")
result2<-prcomp( golub,center=FALSE)
scores<-result2$rotation
lot(scores[,1],scores[,2] ,xlab= "pc1" ,ylab="pc2")
plot(scores[,1],scores[,2] ,xlab= "pc1" ,ylab="pc2")
result2<-prcomp( t(golub),center=FALSE)
scores<-result2$rotation
plot(scores[,1],scores[,2] ,xlab= "pc1" ,ylab="pc2")
savehistory("~/Desktop/RhistoryTermProject.Rhistory")
