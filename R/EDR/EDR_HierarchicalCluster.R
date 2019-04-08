set.seed(1234)
x <- rnorm(12, rep(1:3, each = 4), 0.2)
y <- rnorm(12, rep(c(1, 2, 1), each = 4), 0.2)
plot(x, y, col = "blue", pch = 19, cex = 2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))

df <- data.frame(x=x,y=y)
dist(df) #Euclidean Distance

rdistxy<-as.matrix(dist(df))
## Remove the diagonal from consideration
diag(rdistxy)<-diag(rdistxy)+100000
# Find the index of the points with minimum distance
ind<-which(rdistxy==min(rdistxy),arr.ind = TRUE)

plot(x,y,col='blue',pch=19,cex=2)
text(x+0.05,y+0.05,labels = as.character(1:12))
points(x[ind[1,]],y[ind[1,]],col='orange',pch=19,cex=2)

par(mfrow = c(1, 2))
plot(x, y, col = "blue", pch = 19, cex = 2, main = "Data")
text(x + 0.05, y + 0.05, labels = as.character(1:12))
points(x[ind[1, ]], y[ind[1, ]], col = "orange", pch = 19, cex = 2)
# Make a cluster and cut it at the right height
library(dplyr)
hcluster <- dist(df) %>% hclust
dendro <- as.dendrogram(hcluster)
cutDendro <- cut(dendro, h = (hcluster$height[1] + 0.00001))
plot(cutDendro$lower[[11]], yaxt = "n", main = "Begin building tree")

nextmin <- rdistxy[order(rdistxy)][3]
ind <- which(rdistxy == nextmin,arr.ind=TRUE)

hClustering <- data.frame(x=x,y=y) %>% dist %>% hclust
plot(hClustering)

myplclust <-
  function(hclust,
           lab = hclust$labels,
           lab.col = rep(1, length(hclust$labels)),
           hang = 0.1,
           ...) {
    ## modifiction of plclust for plotting hclust objects *in colour*! Copyright
    ## Eva KF Chan 2009 Arguments: hclust: hclust object lab: a character vector
    ## of labels of the leaves of the tree lab.col: colour for the labels;
    ## NA=default device foreground colour hang: as in hclust & plclust Side
    ## effect: A display of hierarchical cluster with coloured leaf labels.
    y <- rep(hclust$height, 2)
    x <- as.numeric(hclust$merge)
    y <- y[which(x < 0)]
    x <- x[which(x < 0)]
    x <- abs(x)
    y <- y[order(x)]
    x <- x[order(x)]
    plot(hclust, labels = FALSE, hang = hang, ...)
    text(
      x = x,
      y = y[hclust$order] - (max(hclust$height) * hang),
      labels = lab[hclust$order],
      col = lab.col[hclust$order],
      srt = 90,
      adj = c(1, 0.5),
      xpd = NA,
      ...
    )
  }

hClustering <- data.frame(x = x, y = y) %>% dist %>% hclust
myplclust(hClustering, lab = rep(1:3, each = 4), lab.col = rep(1:3, each = 4))

dataMatrix <- data.frame(x=x,y=y) %>% data.matrix
heatmap(dataMatrix)


# K-Means
df2<-data.frame(x,y)
kmeansObj<-kmeans(df2,centers = 3)
dm<-as.matrix(df2)[sample(1:12),]
keansObj<-kmeans(dm,centers = 3)
par(mfrow=c(1,2))
image(t(dm)[,nrow(dm):1],yaxt = 'n',main='Original Data')
image(t(dm)[,order(keansObj$cluster)],yaxt='n',main='Clustered Data')