#One of many ways to implement Worksheet 1 (similarity coefficient) using R.

shopper1<-c(0,0,0,1,1,1)
shopper2<-c(1,1,1,0,1,0)
shopper3<-c(0,1,0,1,1,0)
shopper4<-c(0,0,1,0,1,1)
shopper5<-c(1,1,1,0,0,0)

shoppers<-rbind(shopper1, shopper2, shopper3, shopper4, shopper5)
shoppers
str(shoppers)
class(shoppers)

#the documentation did not explicitly say and it appears method=binary
#will produce jaccard distances (but I avoid using dist() to compute
#simple matching/jaccard/dice since there are alternatives 
mydist<-dist(shoppers, method="binary")
mydist
str(mydist)
class(mydist)

mydist<-dist(shoppers, method="binary", diag=T)
mydist
str(mydist)
class(mydist)


#if you search the internet for packages, there are various packages that
#are created to calculate binary distances.  An example is package proxyC
#The simil() function in proxyC can calculate simple matching, jaccard and dice
#distances 

library(proxyC)
#simple matching
simpledist<-simil(shoppers,method="simple matching")
simpledist
#note the answer in simpledist agrees with what we did by hand in WS1
str(simpledist)
class(simpledist)

jdist<-simil(shoppers,method="jaccard")
jdist

dicedist<-simil(shoppers,method="dice")
dicedist


