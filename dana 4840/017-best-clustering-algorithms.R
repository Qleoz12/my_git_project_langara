## We'll start by describing the different measures in the clValid package for comparing clustering algorithms. Next, we'll present the function *clValid*(). Finally, we'll provide R scripts for validating clustering results and comparing clustering algorithms.

## The values of APN, ADM and FOM ranges from 0 to 1, with smaller value corresponding with highly consistent clustering results. AD has a value between 0 and infinity, and smaller values are also preferred.

## Note that, the clValid package provides also biological validation measures, which evaluates the ability of a clustering algorithm to produce biologically meaningful clusters. An application is microarray or RNAseq data where observations corresponds to genes.

## ---- eval = FALSE-------------------------------------------------------
## clValid(obj, nClust, clMethods = "hierarchical",
##         validation = "stability", maxitems = 600,
##         metric = "euclidean", method = "average")

## - **obj**: A numeric matrix or data frame. Rows are the items to be clustered and columns are samples.

## ------------------------------------------------------------------------
library(clValid)
# Iris data set:
# - Remove Species column and scale
df <- scale(iris[, -5])

# Compute clValid
clmethods <- c("hierarchical","kmeans","pam")
intern <- clValid(df, nClust = 2:6, 
              clMethods = clmethods, validation = "internal")
# Summary
summary(intern)

## It can be seen that hierarchical clustering with two clusters performs the best in each case (i.e., for connectivity, Dunn and Silhouette measures). Regardless of the clustering algorithm, the optimal number of clusters seems to be two using the three measures.

## ------------------------------------------------------------------------
# Stability measures
clmethods <- c("hierarchical","kmeans","pam")
stab <- clValid(df, nClust = 2:6, clMethods = clmethods, 
                validation = "stability")
# Display only optimal Scores
optimalScores(stab)

## For the APN and ADM measures, hierarchical clustering with two clusters again gives the best score. For the other measures, PAM with six clusters has the best score.

