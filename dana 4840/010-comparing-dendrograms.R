## ------------------------------------------------------------------------
df <- scale(USArrests)

## ------------------------------------------------------------------------
# Subset containing 10 rows
set.seed(123)
ss <- sample(1:50, 10)
df <- df[ss,]

## ------------------------------------------------------------------------
library(dendextend)

# Compute distance matrix
res.dist <- dist(df, method = "euclidean")

# Compute 2 hierarchical clusterings
hc1 <- hclust(res.dist, method = "average")
hc2 <- hclust(res.dist, method = "ward.D2")

# Create two dendrograms
dend1 <- as.dendrogram (hc1)
dend2 <- as.dendrogram (hc2)

# Create a list to hold dendrograms
dend_list <- dendlist(dend1, dend2)

## ----compare-dendrogram-tanglegram, fig.height=3, fig.width=7,  fig.show="asis"----
# Align and plot two dendrograms side by side
dendlist(dend1, dend2) %>%
  untangle(method = "step1side") %>% # Find the best alignment layout
  tanglegram()                       # Draw the two dendrograms

# Compute alignment quality. Lower value = good alignment quality
dendlist(dend1, dend2) %>%
  untangle(method = "step1side") %>% # Find the best alignment layout
  entanglement()                     # Alignment quality

## ----compare-dendrogram-customize, fig.height=3, fig.width=7, eval = FALSE----
## dendlist(dend1, dend2) %>%
##   untangle(method = "step1side") %>%
##   tanglegram(
##     highlight_distinct_edges = FALSE, # Turn-off dashed lines
##     common_subtrees_color_lines = FALSE, # Turn-off line colors
##     common_subtrees_color_branches = TRUE # Color common branches
##     )

## Note that, "unique" nodes, with a combination of labels/items not present in the other tree, are highlighted with dashed lines.

## Note that, just because we can get two trees to have horizontal connecting lines, it doesn’t mean these trees are identical (or even very similar topologically).

## ---- fig.show = "asis"--------------------------------------------------
# Cophenetic correlation matrix
cor.dendlist(dend_list, method = "cophenetic")

# Baker correlation matrix
cor.dendlist(dend_list, method = "baker")

## ------------------------------------------------------------------------
# Cophenetic correlation coefficient
cor_cophenetic(dend1, dend2)

# Baker correlation coefficient
cor_bakers_gamma(dend1, dend2)

## ----compare-multiple-dendrograms, fig.show="asis", fig.height=3, fig.width=4----
# Create multiple dendrograms by chaining
dend1 <- df %>% dist %>% hclust("complete") %>% as.dendrogram
dend2 <- df %>% dist %>% hclust("single") %>% as.dendrogram
dend3 <- df %>% dist %>% hclust("average") %>% as.dendrogram
dend4 <- df %>% dist %>% hclust("centroid") %>% as.dendrogram
# Compute correlation matrix
dend_list <- dendlist("Complete" = dend1, "Single" = dend2,
                      "Average" = dend3, "Centroid" = dend4)
cors <- cor.dendlist(dend_list)
# Print correlation matrix
round(cors, 2)
# Visualize the correlation matrix using corrplot package
library(corrplot)
corrplot(cors, "pie", "lower")

