#R code to supplement WS 2 - K-means

A<-c(5,3)
B<-c(-1,1)
C<-c(1,-2)
D<-c(-3,-2)
data4pt<-rbind(A, B, C, D)
data4pt
kclusters<-kmeans(data4pt, 2, nstart=25)
kclusters

#check total within ss 
kclusters$tot.withinss

