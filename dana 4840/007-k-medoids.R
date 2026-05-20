## In this article, We'll describe the PAM algorithm and provide practical examples using **R** software. In the next chapter, we'll also discuss a variant of PAM named **CLARA** (Clustering Large Applications) which is used for analyzing large data sets.

## **Build phase**:

## Note that, in practice, you should get similar results most of the time, using either euclidean or Manhattan distance. If your data contains outliers, Manhattan distance should give more robust results, whereas euclidean would be influenced by unusual values.

## ------------------------------------------------------------------------
data("USArrests")      # Load the data set
df <- scale(USArrests) # Scale the data
head(df, n = 3)        # View the firt 3 rows of the data

## ---- eval = FALSE-------------------------------------------------------
## pam(x, k, metric = "euclidean", stand = FALSE)

## - **x**: possible values includes:

## ---- eval = FALSE-------------------------------------------------------
## install.packages(c("cluster", "factoextra"))

## ------------------------------------------------------------------------
library(cluster)
library(factoextra)

## ----pam-optimal-clusters-wss, fig.height=3------------------------------
library(cluster)
library(factoextra)
fviz_nbclust(df, pam, method = "silhouette")+
  theme_classic()

## From the plot, the suggested number of clusters is 2. In the next section, we'll classify the observations into 2 clusters.

## ------------------------------------------------------------------------
pam.res <- pam(df, 2)
print(pam.res)

## The printed output shows:

## ------------------------------------------------------------------------
dd <- cbind(USArrests, cluster = pam.res$cluster)
head(dd, n = 3)

## ------------------------------------------------------------------------
# Cluster medoids: New Mexico, Nebraska
pam.res$medoids

# Cluster numbers
head(pam.res$clustering)

## ----pam-k-medoids-clustering-plot, fig.width=5, fig.height=4.7----------
fviz_cluster(pam.res, 
             palette = c("#00AFBB", "#FC4E07"), # color palette
             ellipse.type = "t", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic()
             )

## Note that, for large data sets,  \textit{pam}() may need too much memory or too much computation time. In this case, the function \textit{clara}() is preferable. This should not be a problem for modern computers.

