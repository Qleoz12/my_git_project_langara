## ------------------------------------------------------------------------
# Load data
data(USArrests)

# Compute distances and hierarchical clustering
dd <- dist(scale(USArrests), method = "euclidean")
hc <- hclust(dd, method = "ward.D2")

## ---- eval= FALSE--------------------------------------------------------
## install.packages(c("factoextra", "dendextend"))

## ---- echo = FALSE-------------------------------------------------------
library(factoextra)

## ----fviz_dend, fig.show = "asis", fig.height=4.5, eval = FALSE----------
## library(factoextra)
## fviz_dend(hc, cex = 0.5)

## ---- eval = FALSE-------------------------------------------------------
## fviz_dend(hc, cex = 0.5,
##           main = "Dendrogram - ward.D2",
##           xlab = "Objects", ylab = "Distance", sub = "")

## ---- eval = FALSE-------------------------------------------------------
## fviz_dend(hc, cex = 0.5, horiz = TRUE)

## ----cutree, fig.height=3.7----------------------------------------------
fviz_dend(hc, k = 4, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE, # Add rectangle around groups
          rect_border = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"), 
          rect_fill = TRUE)

## ----ggplot2-dendrogram, fig.height=3.7, eval = FALSE--------------------
## fviz_dend(hc, k = 4,                 # Cut in four groups
##           cex = 0.5,                 # label size
##           k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
##           color_labels_by_k = TRUE,  # color labels by groups
##           ggtheme = theme_gray()     # Change theme
##           )

## ----ggplot2-dendrogram-color-palette, fig.height=3----------------------
fviz_dend(hc, cex = 0.5, k = 4, # Cut in four groups
          k_colors = "jco")

## ----horizontal-dendrograms-rectangle, fig.height=4----------------------
fviz_dend(hc, k = 4, cex = 0.4, horiz = TRUE,  k_colors = "jco", 
          rect = TRUE, rect_border = "jco", rect_fill = TRUE)

## ----ggplot2-dendrogram-circular, fig.height=3.7, fig.width=3.7----------
fviz_dend(hc, cex = 0.5, k = 4, 
          k_colors = "jco", type = "circular")

## ----phylogenic-tree, fig.height=5, fig.width=6.5------------------------
require("igraph")
fviz_dend(hc, k = 4, k_colors = "jco",
          type = "phylogenic", repel = TRUE)

## ----phylogenic-tree-layout, fig.height=5, fig.width=6.5, eval = FALSE----
## require("igraph")
## fviz_dend(hc, k = 4, # Cut in four groups
##           k_colors = "jco",
##           type = "phylogenic", repel = TRUE,
##           phylo_layout = "layout.gem")

## ----dendrogram-zoom, fig.height=3.2-------------------------------------
fviz_dend(hc, xlim = c(1, 20), ylim = c(1, 8))

## ----cut-dendrogram, fig.height=3, fig.width=3, fig.show="as.is"---------
# Create a plot of the whole dendrogram,
# and extract the dendrogram data
dend_plot <- fviz_dend(hc, k = 4, # Cut in four groups
          cex = 0.5, # label size
          k_colors = "jco"
          )
dend_data <- attr(dend_plot, "dendrogram") # Extract dendrogram data


# Cut the dendrogram at height h = 10
dend_cuts <- cut(dend_data, h = 10)
# Visualize the truncated version containing
# two branches
fviz_dend(dend_cuts$upper)

## ----whole-dendrogam, fig.height=3.7-------------------------------------
# Plot the whole dendrogram
print(dend_plot)

## ----subtree, fig.height=3, fig.width = 3.2------------------------------
# Plot subtree 1
fviz_dend(dend_cuts$lower[[1]], main = "Subtree 1")

# Plot subtree 2
fviz_dend(dend_cuts$lower[[2]], main = "Subtree 2")

## ----sub-tree-circular, fig.height=3.2, fig.width=3.2--------------------
fviz_dend(dend_cuts$lower[[2]], type = "circular")

## ----save-pdf, eval = FALSE----------------------------------------------
## pdf("dendrogram.pdf", width=30, height=15)            # Open a PDF
## p <- fviz_dend(hc, k = 4, cex = 1, k_colors = "jco" ) # Do plotting
## print(p)
## dev.off()                                             # Close the PDF

## ---- eval = FALSE-------------------------------------------------------
## data <- scale(USArrests)
## dist.res <- dist(data)
## hc <- hclust(dist.res, method = "ward.D2")
## dend <- as.dendrogram(hc)
## plot(dend)

## ---- eval = FALSE-------------------------------------------------------
## library(dendextend)
## dend <- USArrests[1:5,] %>% # data
##         scale %>% # Scale the data
##         dist %>% # calculate a distance matrix,
##         hclust(method = "ward.D2") %>% # Hierarchical clustering
##         as.dendrogram # Turn the object into a dendrogram.
## plot(dend)

## ---- eval = FALSE-------------------------------------------------------
## set(object, what, value)

## 1. **object**: a dendrogram object

## ---- eval=FALSE---------------------------------------------------------
## library(dendextend)
## # 1. Create a customized dendrogram
## mycols <- c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07")
## dend <-  as.dendrogram(hc) %>%
##    set("branches_lwd", 1) %>% # Branches line width
##    set("branches_k_color", mycols, k = 4) %>% # Color branches by groups
##    set("labels_colors", mycols, k = 4) %>%  # Color labels by groups
##    set("labels_cex", 0.5) # Change label size
## 
## # 2. Create plot
## fviz_dend(dend)

