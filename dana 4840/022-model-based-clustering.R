## In this chapter, we illustrate model-based clustering using the R package *mclust*.

## ----scatter-plot, fig.width=3.5, fig.height=3.5-------------------------
# Load the data
library("MASS")
data("geyser")

# Scatter plot
library("ggpubr")
ggscatter(geyser, x = "duration", y = "waiting")+
  geom_density2d() # Add 2D density

## Note that, model-based clustering can be applied on univariate or multivariate data.

## ------------------------------------------------------------------------
library("mclust")
data("diabetes")
head(diabetes, 3)

## ---- fig.show="asis"----------------------------------------------------
library(mclust)
df <- scale(diabetes[, -1]) # Standardize the data
mc <- Mclust(df)            # Model-based-clustering
summary(mc)                 # Print a summary

## ---- eval = FALSE-------------------------------------------------------
## mc$modelName                # Optimal selected model ==> "VVV"
## mc$G                        # Optimal number of cluster => 3
## head(mc$z, 30)              # Probality to belong to a given cluster
## head(mc$classification, 30) # Cluster assignement of each observation

## ----model-base-clustering, fig.width=3.2, fig.height=3------------------
library(factoextra)
# BIC values used for choosing the number of clusters
fviz_mclust(mc, "BIC", palette = "jco")
# Classification: plot showing the clustering
fviz_mclust(mc, "classification", geom = "point", 
            pointsize = 1.5, palette = "jco")
# Classification uncertainty
fviz_mclust(mc, "uncertainty", palette = "jco")

## Note that, in the uncertainty plot, larger symbols indicate the more uncertain observations.

