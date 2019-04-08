# simulate some random Normal data in a matrix that has 40 rows and 10 columns
set.seed(12345)
dm1<-matrix(rnorm(400),nrow = 40)
image(1:10,1:40,t(dm1)[,nrow(dm1):1])
heatmap(dm1)

set.seed(678910)
for (i in 1:40){
  coinFlip <- rbinom(1,size = 1,prob = 0.5)
  if (coinFlip){
    dm1[i,]<-dm1[i,]+rep(c(0,3),each=5)
  }
}
heatmap(dm1)

library(dplyr)
hh <- dist(dm1) %>% hclust
dmOrdered<-dm1[hh$order,]
par(mfrow=c(1,3))
image(t(dmOrdered)[,nrow(dmOrdered):1])
plot(rowMeans(dmOrdered),40:1,xlab = 'Row Mean',ylab = 'Row',pch=19)
plot(colMeans(dmOrdered),xlab = 'Column',ylab = 'Column Mean',pch=19)

svd1 <- svd(scale(dmOrdered))
par(mfrow=c(1,3))
image(t(dmOrdered)[,nrow(dmOrdered):1],main='original data')
plot(svd1$u[,1],40:1,ylab = 'Row',xlab = 'First left singular vector',pch=19)
plot(svd1$v[,1],xlab = 'Column',ylab = 'First right singular vector',pch=19)

approx<-with(svd1,outer(u[,1],v[,1]))
par(mfrow=c(1,2))
image(t(dmOrdered)[,nrow(dmOrdered):1],main='original matrix')
image(t(approx)[,nrow(approx):1],main='Approximated Matrix')

