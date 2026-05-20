## In this chapter, we described an hybrid method, named **hierarchical k-means clustering** (hkmeans), for improving k-means results.

## Note that, k-means algorithm will improve the initial partitioning generated at the step 2 of the algorithm. Hence, the initial partitioning can be slightly different from the final partitioning obtained in the step 4.

## ------------------------------------------------------------------------
df <- scale(USArrests)

## ---- fig.show="asis"----------------------------------------------------
# Compute hierarchical k-means clustering
library(factoextra)
res.hk <-hkmeans(df, 4)
# Elements returned by hkmeans()
names(res.hk)

## ---- eval = FALSE-------------------------------------------------------
## # Print the results
## res.hk

## ----hierarchical-k-means-clustering, fig.show="asis"--------------------
# Visualize the tree
fviz_dend(res.hk, cex = 0.6, palette = "jco", 
          rect = TRUE, rect_border = "jco", rect_fill = TRUE)
   
# Visualize the hkmeans final clusters
fviz_cluster(res.hk, palette = "jco", repel = TRUE,
             ggtheme = theme_classic())

