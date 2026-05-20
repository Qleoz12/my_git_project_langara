## In this chapter, we'll describe the DBSCAN algorithm and demonstrate how to compute DBSCAN using the *fpc* R package.

## ----data-dbscan, echo = FALSE, fig.height=3.5, fig.width=3.5------------
# Load the dataset
library("factoextra")
data("multishapes")
df <- multishapes[, 1:2]
# Visualize the data
ggplot(df, aes(x, y)) + geom_point()

## ----k-means-multishapes,  fig.height=3.5, fig.width=3.5-----------------
library(factoextra)
data("multishapes")
df <- multishapes[, 1:2]
set.seed(123)
km.res <- kmeans(df, 5, nstart = 25)
fviz_cluster(km.res, df,  geom = "point", 
             ellipse= FALSE, show.clust.cent = FALSE,
             palette = "jco", ggtheme = theme_classic())

## We know there are 5 five clusters in the data, but it can be seen that k-means method inaccurately identify the 5 clusters.

## 1. For each point $x_i$, compute the distance between $x_i$ and the other points. Finds all neighbor points within distance *eps* of the starting point ($x_i$). Each point, with a neighbor count greater than or equal to *MinPts*, is marked as *core point* or *visited*.

## ---- eval = FALSE-------------------------------------------------------
## install.packages("fpc")
## install.packages("dbscan")
## install.packages("factoextra")

## ----density-based-clustering, fig.width=3.5, fig.height=3.5-------------
# Load the data 
data("multishapes", package = "factoextra")
df <- multishapes[, 1:2]

# Compute DBSCAN using fpc package
library("fpc")
set.seed(123)
db <- fpc::dbscan(df, eps = 0.15, MinPts = 5)

# Plot DBSCAN results
library("factoextra")
fviz_cluster(db, data = df, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())

## Note that, the function *fviz_cluster*() uses different point symbols for core points (i.e, seed points) and border points. Black points correspond to outliers. You can play with *eps* and *MinPts* for changing cluster configurations.

## It can be seen that DBSCAN performs better for these data sets and can identify the correct set of clusters compared to k-means algorithms.

## ------------------------------------------------------------------------
print(db)

## ------------------------------------------------------------------------
# Cluster membership. Noise/outlier observations are coded as 0
# A random subset is shown
db$cluster[sample(1:1089, 20)]

## How to define the optimal value of \textit{eps}?

## ----k-nearest-neighbor-distance, fig.width = 4, fig.height=4------------
dbscan::kNNdistplot(df, k =  5)
abline(h = 0.15, lty = 2)

## It can be seen that the optimal *eps* value is around a distance of 0.15.

