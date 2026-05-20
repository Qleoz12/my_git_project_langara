## 1. Split randomly the data sets in multiple subsets with fixed size (sampsize)

## ----clara, fig.show ="asis"---------------------------------------------
set.seed(1234)
# Generate 500 objects, divided into 2 clusters.
df <- rbind(cbind(rnorm(200,0,8), rnorm(200,0,8)),
           cbind(rnorm(300,50,8), rnorm(300,50,8)))

# Specify column and row names
colnames(df) <- c("x", "y")
rownames(df) <- paste0("S", 1:nrow(df))

# Previewing the data
head(df, nrow = 6)

## ---- eval = FALSE-------------------------------------------------------
## clara(x, k, metric = "euclidean", stand = FALSE,
##       samples = 5, pamLike = FALSE)

## - **x**: a numeric data matrix or data frame, each row corresponds to an observation, and each column corresponds to a variable. Missing values (NAs) are allowed.

## ---- eval = FALSE-------------------------------------------------------
## install.packages(c("cluster", "factoextra"))

## ------------------------------------------------------------------------
library(cluster)
library(factoextra)

## ----clara-optimal-clusters-wss, fig.height=3----------------------------
library(cluster)
library(factoextra)
fviz_nbclust(df, clara, method = "silhouette")+
  theme_classic()

## From the plot, the suggested number of clusters is 2. In the next section, we'll classify the observations into 2 clusters.

## ------------------------------------------------------------------------
# Compute CLARA
clara.res <- clara(df, 2, samples = 50, pamLike = TRUE)

# Print components of clara.res
print(clara.res)

## ------------------------------------------------------------------------
dd <- cbind(df, cluster = clara.res$cluster)
head(dd, n = 4)

## ------------------------------------------------------------------------
# Medoids
clara.res$medoids

# Clustering
head(clara.res$clustering, 10)

## ----clara-k-medoids-clustering-large-data-sets-plot, fig.width=4.5, fig.height=4----
fviz_cluster(clara.res, 
             palette = c("#00AFBB", "#FC4E07"), # color palette
             ellipse.type = "t", # Concentration ellipse
             geom = "point", pointsize = 1,
             ggtheme = theme_classic()
             )

