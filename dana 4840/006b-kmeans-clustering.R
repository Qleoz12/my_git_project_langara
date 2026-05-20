## In this article, we'll describe the **k-means algorithm** and provide practical examples using **R** software.

## 1. Specify the number of clusters (K) to be created (by the analyst)

## ------------------------------------------------------------------------
data("USArrests")      # Loading the data set
df <- scale(USArrests) # Scaling the data

# View the firt 3 rows of the data
head(df, n = 3)

## ---- eval = FALSE-------------------------------------------------------
## kmeans(x, centers, iter.max = 10, nstart = 1)

## - **x**: numeric matrix, numeric data frame or a numeric vector

## ---- eval = FALSE-------------------------------------------------------
## install.packages("factoextra")

## ------------------------------------------------------------------------
library(factoextra)

## ----k-means-optimal-clusters-wss, fig.height=3--------------------------
library(factoextra)
fviz_nbclust(df, kmeans, method = "wss") +
    geom_vline(xintercept = 4, linetype = 2)

## The plot above represents the variance within the clusters. It decreases as k increases, but it can be seen a bend (or "elbow") at k = 4. This bend indicates that additional clusters beyond the fourth have little value.. In the next section, we'll classify the observations into 4 clusters.

## ------------------------------------------------------------------------
# Compute k-means with k = 4
set.seed(123)
km.res <- kmeans(df, 4, nstart = 25)

## As the final result of k-means clustering result is sensitive to the random starting assignments, we specify *nstart = 25*. This means that R will try 25 different random starting assignments and then select the best results corresponding to the one with the lowest within cluster variation. The default value of *nstart* in R is one. But, it's strongly recommended to compute *k-means clustering* with a large value of *nstart* such as 25 or 50, in order to have a more stable result.

## ------------------------------------------------------------------------
# Print the results
print(km.res)

## The printed output displays:

## ------------------------------------------------------------------------
aggregate(USArrests, by=list(cluster=km.res$cluster), mean)

## ------------------------------------------------------------------------
dd <- cbind(USArrests, cluster = km.res$cluster)
head(dd)

## ---- eval = FALSE-------------------------------------------------------
## # Cluster number for each of the observations
## km.res$cluster

## ------------------------------------------------------------------------
head(km.res$cluster, 4)

## ------------------------------------------------------------------------
# Cluster size
km.res$size
# Cluster means
km.res$centers

## In other words, if we have a multi-dimensional data set, a solution is to perform Principal Component Analysis (PCA) and to plot data points according to the first two principal components coordinates.

## ----k-means-plot-ggplot2-factoextra, fig.width=7, fig.height=6----------
fviz_cluster(km.res, data = df,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"), 
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
             )

## 1. It assumes prior knowledge of the data and requires the analyst to choose the appropriate number of cluster (k) in advance

## 1. Solution to issue 1: Compute k-means for a range of k values, for example by varying k between 2 and 10. Then, choose the best k by comparing the clustering results obtained for the different k values.

