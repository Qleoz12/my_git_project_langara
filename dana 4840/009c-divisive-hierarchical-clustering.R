## This article introduces the divisive clustering algorithms and provides practical examples showing how to compute divise clustering using R.

## ----compute-diana, fig.height=4-----------------------------------------
# Compute diana()
library(cluster)
res.diana <- diana(USArrests, stand = TRUE)

# Plot the dendrogram
library(factoextra)
fviz_dend(res.diana, cex = 0.5,
          k = 4, # Cut in four groups
          palette = "jco" # Color palette
          )

