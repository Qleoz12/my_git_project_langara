## ----dendrogram-hclust, echo = FALSE-------------------------------------
factoextra::fviz_dend(hclust(dist(scale(USArrests)), "ward.D2"), 
          main = "Hierarchical Clustering", sub = "", cex = 0.6)

## In previous chapters, we defined several methods for measuring distances between objects in a data matrix. In this chapter, we'll show how to visualize the dissimilarity between objects using dendrograms.

