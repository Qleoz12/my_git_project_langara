## There are many methods to calculate this distance information. In this article, we describe the common **distance measures** and provide R codes for computing and visualizing distances.

## Pearson correlation measures the degree of a linear relationship between two profiles.

## Note that,

## In the formula above, $x$ and $y$ are two vectors of length $n$ and,  means $\bar{x}$ and $\bar{y}$, respectively. The distance between x and y is denoted $d(x, y)$.

## Standardization makes the four distance measure methods - Euclidean, Manhattan, Correlation and Eisen - more similar than they would be with non-transformed data.

## ------------------------------------------------------------------------
# Subset of the data
set.seed(123)
ss <- sample(1:50, 15)   # Take 15 random rows
df <- USArrests[ss, ]    # Subset the 15 rows
df.scaled <- scale(df)   # Standardize the variables

## All these functions compute distances between rows of the data.

## ------------------------------------------------------------------------
dist.eucl <- dist(df.scaled, method = "euclidean")

## ------------------------------------------------------------------------
# Reformat as a matrix
# Subset the first 3 columns and rows and Round the values
round(as.matrix(dist.eucl)[1:3, 1:3], 1)

## In this data set, the columns are variables. Hence, if we want to compute pairwise distances between variables, we must start by transposing the data to have variables in the rows of the data set before using the *dist*() function. The function *t*() is used for transposing the data.

## ------------------------------------------------------------------------
# Compute
library("factoextra")
dist.cor <- get_dist(df.scaled, method = "pearson")

# Display a subset
round(as.matrix(dist.cor)[1:3, 1:3], 1)

## ------------------------------------------------------------------------
library(cluster)
# Load data
data(flower)
head(flower, 3)

# Data structure
str(flower)

# Distance matrix
dd <- daisy(flower)
round(as.matrix(dd)[1:3, 1:3], 2)

## ----visualize-distance-measures, fig.show = "asis", fig.width = 4.5, fig.height=3.9----
library(factoextra)
fviz_dist(dist.eucl)

