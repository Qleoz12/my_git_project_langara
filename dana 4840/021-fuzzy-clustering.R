## In this article, we'll describe how to compute fuzzy clustering using the R software.

## ---- eval = FALSE-------------------------------------------------------
## fanny(x, k, metric = "euclidean", stand = FALSE)

## - **x**: A data matrix or data frame or dissimilarity matrix

## ----fuzzy-clustering-fanny, fig.show = "asis"---------------------------
library(cluster)
df <- scale(USArrests)     # Standardize the data
res.fanny <- fanny(df, 2)  # Compute fuzzy clustering with k = 2

## ----membership-coefficient, fig.show = "asis"---------------------------
head(res.fanny$membership, 3) # Membership coefficients
res.fanny$coeff # Dunn's partition coefficient
head(res.fanny$clustering) # Observation groups

## ----visualize, fig.height=4.5-------------------------------------------
library(factoextra)
fviz_cluster(res.fanny, ellipse.type = "norm", repel = TRUE,
             palette = "jco", ggtheme = theme_minimal(),
             legend = "right")

## ----silhouette, fig.height=4--------------------------------------------
fviz_silhouette(res.fanny, palette = "jco",
                ggtheme = theme_minimal())

