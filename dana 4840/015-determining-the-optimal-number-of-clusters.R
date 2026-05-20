## In this chapter, we'll describe different methods for determining the optimal number of clusters for  k-means, k-medoids (PAM) and hierarchical clustering.

## 1. Compute clustering algorithm (e.g., k-means clustering) for different values of k. For instance, by varying k from 1 to 10 clusters.

## Note that, the elbow method is sometimes ambiguous. An alternative is the average silhouette method  (Kaufman and Rousseeuw [1990]) which can be also used with any clustering approach.

## 1. Compute clustering algorithm (e.g., k-means clustering) for different values of k. For instance, by varying k from 1 to 10 clusters.

## 1. Cluster the observed data, varying the number of clusters from k = 1, ..., $k_{max}$, and compute the corresponding total within intra-cluster variation $W_k$.

## Note that, using B = 500 gives quite precise results so that the gap plot is basically unchanged after an another run.

## ---- eval = FALSE-------------------------------------------------------
## pkgs <- c("factoextra",  "NbClust")
## install.packages(pkgs)

## ------------------------------------------------------------------------
library(factoextra)
library(NbClust)

## ------------------------------------------------------------------------
# Standardize the data
df <- scale(USArrests)
head(df)

## ---- eval = FALSE-------------------------------------------------------
## fviz_nbclust(x, FUNcluster, method = c("silhouette", "wss", "gap_stat"))

## - **x**: numeric matrix or data frame

## ----k-means-optimal-clusters-wss-silhouette, fig.width=3, fig.height=3----
# Elbow method
fviz_nbclust(df, kmeans, method = "wss") +
    geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(df, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(df, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

## - Elbow method: 4 clusters solution suggested

## The disadvantage of elbow and average silhouette methods is that, they measure a global clustering characteristic only. A more sophisticated method is to use the gap statistic which provides a statistical procedure to formalize the elbow/silhouette heuristic in order to estimate the optimal number of clusters.

## ---- eval = FALSE-------------------------------------------------------
## NbClust(data = NULL, diss = NULL, distance = "euclidean",
##         min.nc = 2, max.nc = 15, method = NULL)

## - **data**: matrix

## ----compute-nbclust, fig.show='hide', results="hide"--------------------
library("NbClust")
nb <- NbClust(df, distance = "euclidean", min.nc = 2,
        max.nc = 10, method = "kmeans")

## ----nbclust, fig.width=4, fig.height=2.7--------------------------------
library("factoextra")
fviz_nbclust(nb)

## - ....

