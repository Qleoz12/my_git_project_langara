## Clusters with AU >= 95% are considered to be strongly supported by data.

## ---- eval = FALSE-------------------------------------------------------
## install.packages("pvclust")

## ------------------------------------------------------------------------
library(pvclust)

## ---- fig.show = "asis"--------------------------------------------------
library(pvclust)
# Load the data
data("lung")
head(lung[, 1:4])
# Dimension of the data
dim(lung)

## ------------------------------------------------------------------------
set.seed(123)
ss <- sample(1:73, 30) # extract 20 samples out of
df <- lung[, ss]

## ---- eval = FALSE-------------------------------------------------------
## pvclust(data, method.hclust = "average",
##         method.dist = "correlation", nboot = 1000)

## ---- eval = FALSE-------------------------------------------------------
## parPvclust(cl=NULL, data, method.hclust = "average",
##            method.dist = "correlation", nboot = 1000,
##            iseed = NULL)

## - **data**: numeric data matrix or data frame.

## ---- results='hide'-----------------------------------------------------
library(pvclust)
set.seed(123)
res.pv <- pvclust(df, method.dist="cor", 
                  method.hclust="average", nboot = 10)

## ---- pvclust-p-value-hierarchical-clustering, fig.height=4.5------------
# Default plot
plot(res.pv, hang = -1, cex = 0.5)
pvrect(res.pv)

## Values on the dendrogram are *AU p-values* (Red, left), *BP values* (green, right), and $cluster labels$ (grey, bottom). Clusters with AU > = 95% are indicated by the rectangles and are considered to be strongly supported by data.

## ---- eval = FALSE-------------------------------------------------------
## clusters <- pvpick(res.pv)
## clusters

## ---- eval = FALSE-------------------------------------------------------
## # Create a parallel socket cluster
## library(parallel)
## cl <- makeCluster(2, type = "PSOCK")
## # parallel version of pvclust
## res.pv <- parPvclust(cl, df, nboot=1000)
## stopCluster(cl)

