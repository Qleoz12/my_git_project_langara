## In this chapter, we start by describing why we should evaluate the clustering tendency before applying any clustering method on a data. Next, we provide statistical and visual methods for assessing the clustering tendency.

## ---- eval = FALSE-------------------------------------------------------
## install.packages(c("factoextra", "clustertend"))

## ------------------------------------------------------------------------
head(iris, 3)

## ------------------------------------------------------------------------
# Iris data set
df <- iris[, -5]
# Random data generated from the iris data set
random_df <- apply(df, 2, 
                function(x){runif(length(x), min(x), (max(x)))})
random_df <- as.data.frame(random_df)
# Standardize the data sets
df <- iris.scaled <- scale(df)
random_df <- scale(random_df)

## ----principal-component-analysis, fig.width=3, fig.height=3-------------
library("factoextra")
# Plot faithful data set
fviz_pca_ind(prcomp(df), title = "PCA - Iris data", 
             habillage = iris$Species,  palette = "jco",
             geom = "point", ggtheme = theme_classic(),
             legend = "bottom")

# Plot the random df
fviz_pca_ind(prcomp(random_df), title = "PCA - Random data", 
             geom = "point", ggtheme = theme_classic())

## It can be seen that the iris data set contains 3 real clusters. However the randomly generated data set doesn't contain any meaningful clusters.

## ----k-means-real-data, fig.show="asis", fig.height=3, fig.width=3-------
library(factoextra)
set.seed(123)
# K-means on iris dataset
km.res1 <- kmeans(df, 3)
fviz_cluster(list(data = df, cluster = km.res1$cluster),
             ellipse.type = "norm", geom = "point", stand = FALSE,
             palette = "jco", ggtheme = theme_classic())

## ----k-means-random-data, fig.height=3, fig.width=3----------------------
# K-means on the random dataset
km.res2 <- kmeans(random_df, 3)
fviz_cluster(list(data = random_df, cluster = km.res2$cluster),
             ellipse.type = "norm", geom = "point", stand = FALSE,
             palette = "jco", ggtheme = theme_classic())

# Hierarchical clustering on the random dataset
fviz_dend(hclust(dist(random_df)), k = 3, k_colors = "jco",  
          as.ggplot = TRUE, show_labels = FALSE)

## It can be seen that the k-means algorithm and the hierarchical clustering impose a classification on the random uniformly distributed data set even if there are no meaningful clusters present in it. This is why, clustering tendency assessment methods should be used to evaluate the validity of clustering analysis. That is, whether a given data set contains meaningful clusters.

## We can conduct the Hopkins Statistic test iteratively, using 0.5 as the threshold to reject the alternative hypothesis. That is, if H < 0.5, then it is unlikely that D has statistically significant clusters.

## ---- fig.show = "asis"--------------------------------------------------
library(factoextra)
# Compute Hopkins statistic for iris dataset
res <- get_clust_tendency(df, n = nrow(df)-1, graph = FALSE)
res$hopkins_stat

# Compute Hopkins statistic for a random dataset
res <- get_clust_tendency(random_df, n = nrow(random_df)-1,
                          graph = FALSE)
res$hopkins_stat

## It can be seen that the iris data set is highly clusterable (the **H** value = 0.82 which is far above the threshold 0.5). However the random_df data set is not clusterable ($H = 0.46$)

## 1. Compute the dissimilarity (DM) matrix between the objects in the data set using the Euclidean distance measure

## ----dissimilarity-matrix,  fig.width=3, fig.height=2.7------------------
fviz_dist(dist(df), show_labels = FALSE)+
  labs(title = "Iris data")

fviz_dist(dist(random_df), show_labels = FALSE)+
  labs(title = "Random data")

## The dissimilarity matrix image confirms that there is a cluster structure in the iris data set but not in the random one.

