## Previously, we described how to visualize dendrograms. Here, we'll demonstrate how to draw and arrange a heatmap in R.

## ------------------------------------------------------------------------
df <- scale(mtcars)

## ---- eval = FALSE-------------------------------------------------------
## heatmap(x, scale = "row")

## ----r-base-heatmap, fig.show='asis'-------------------------------------
# Default plot
heatmap(df, scale = "none")

## In the plot above, high values are in red and low values are in yellow.

## ---- eval = FALSE-------------------------------------------------------
## col<- colorRampPalette(c("red", "white", "blue"))(256)

## ---- eval = FALSE-------------------------------------------------------
## library("RColorBrewer")
## col <- colorRampPalette(brewer.pal(10, "RdYlBu"))(256)

## ----r-base-heatmap-color------------------------------------------------
# Use RColorBrewer color palette names
library("RColorBrewer")
col <- colorRampPalette(brewer.pal(10, "RdYlBu"))(256)
heatmap(df, scale = "none", col =  col, 
        RowSideColors = rep(c("blue", "pink"), each = 16),
        ColSideColors = c(rep("purple", 5), rep("orange", 6)))

## ----gplots-heatmap-2----------------------------------------------------
# install.packages("gplots")
library("gplots")
heatmap.2(df, scale = "none", col = bluered(100), 
          trace = "none", density.info = "none")

## ----pheatmap, fig.height=4, eval = FALSE--------------------------------
## library("pheatmap")
## pheatmap(df, cutree_rows = 4)

## ----interactive-heatmap, eval = FALSE-----------------------------------
## library("d3heatmap")
## d3heatmap(scale(mtcars), colors = "RdYlBu",
##           k_row = 4, # Number of groups in rows
##           k_col = 2 # Number of groups in columns
##           )

## ----dendextend----------------------------------------------------------
library(dendextend)
# order for rows
Rowv  <- mtcars %>% scale %>% dist %>% hclust %>% as.dendrogram %>%
   set("branches_k_color", k = 3) %>% set("branches_lwd", 1.2) %>%
   ladderize
# Order for columns: We must transpose the data
Colv  <- mtcars %>% scale %>% t %>% dist %>% hclust %>% as.dendrogram %>%
   set("branches_k_color", k = 2, value = c("orange", "blue")) %>%
   set("branches_lwd", 1.2) %>%
   ladderize

## ---- eval = FALSE-------------------------------------------------------
## heatmap(scale(mtcars), Rowv = Rowv, Colv = Colv,
##         scale = "none")

## ----dendextend-heatmap, eval = FALSE------------------------------------
## library(gplots)
## heatmap.2(scale(mtcars), scale = "none", col = bluered(100),
##           Rowv = Rowv, Colv = Colv,
##           trace = "none", density.info = "none")

## ---- eval = FALSE-------------------------------------------------------
## library("d3heatmap")
## d3heatmap(scale(mtcars), colors = "RdBu",
##           Rowv = Rowv, Colv = Colv)

## ---- eval = FALSE-------------------------------------------------------
## source("https://bioconductor.org/biocLite.R")
## biocLite("ComplexHeatmap")

## ----simple-heatmap, fig.height=4----------------------------------------
library(ComplexHeatmap)
Heatmap(df, 
        name = "mtcars", #title of legend
        column_title = "Variables", row_title = "Samples",
        row_names_gp = gpar(fontsize = 7) # Text size for row names
        )

## ---- eval = FALSE-------------------------------------------------------
## library(circlize)
## mycols <- colorRamp2(breaks = c(-2, 0, 2),
##                     colors = c("green", "white", "red"))
## Heatmap(df, name = "mtcars", col = mycols)

## ----colors-rcolorbrewer, eval = FALSE-----------------------------------
## library("circlize")
## library("RColorBrewer")
## Heatmap(df, name = "mtcars",
##         col = colorRamp2(c(-2, 0, 2), brewer.pal(n=3, name="RdBu")))

## ----dendogram-appearance, fig.height=4----------------------------------
library(dendextend)
row_dend = hclust(dist(df)) # row clustering
col_dend = hclust(dist(t(df))) # column clustering
Heatmap(df, name = "mtcars", 
        row_names_gp = gpar(fontsize = 6.5),
        cluster_rows = color_branches(row_dend, k = 4),
        cluster_columns = color_branches(col_dend, k = 2))

## It's important to use the set.seed() function when performing k-means so that the results obtained can be reproduced precisely at a later time.

## ----k-means, eval = FALSE-----------------------------------------------
## # Divide into 2 groups
## set.seed(2)
## Heatmap(df, name = "mtcars", k = 2)

## ----split-heatmap, fig.height=4-----------------------------------------
# split by a vector specifying rowgroups
Heatmap(df, name = "mtcars", split = mtcars$cyl,
        row_names_gp = gpar(fontsize = 7))

## Note that, *split* can be also a data frame in which different combinations of levels split the rows of the heatmap.

## ----split-heatmap-multiple-variables, fig.height=4----------------------
# Split by combining multiple variables
Heatmap(df, name ="mtcars", 
        split = data.frame(cyl = mtcars$cyl, am = mtcars$am),
        row_names_gp = gpar(fontsize = 7))

## ---- eval = FALSE-------------------------------------------------------
## Heatmap(df, name ="mtcars", col = mycol,
##         km = 2, split =  mtcars$cyl)

## ---- eval = FALSE-------------------------------------------------------
## # install.packages("cluster")
## library("cluster")
## set.seed(2)
## pa = pam(df, k = 3)
## Heatmap(df, name = "mtcars", col = mycol,
##         split = paste0("pam", pa$clustering))

## ---- eval = FALSE-------------------------------------------------------
## HeatmapAnnotation(df, name, col, show_legend)

## ------------------------------------------------------------------------
df <- t(df)

## ----heatmap-annotation, fig.width = 5.4, fig.height = 5-----------------
# Annotation data frame
annot_df <- data.frame(cyl = mtcars$cyl, am = mtcars$am, 
                       mpg = mtcars$mpg)
# Define colors for each levels of qualitative variables
# Define gradient color for continuous variable (mpg)
col = list(cyl = c("4" = "green", "6" = "gray", "8" = "darkred"),
            am = c("0" = "yellow", "1" = "orange"),
            mpg = circlize::colorRamp2(c(17, 25), 
                                       c("lightblue", "purple")) )
# Create the heatmap annotation
ha <- HeatmapAnnotation(annot_df, col = col)

# Combine the heatmap and the annotation
Heatmap(df, name = "mtcars",
        top_annotation = ha)

## ---- eval = FALSE-------------------------------------------------------
## ha <- HeatmapAnnotation(annot_df, col = col, show_legend = FALSE)
## Heatmap(df, name = "mtcars", top_annotation = ha)

## ----complex-heatmap-annotation, fig.width = 6, fig.height = 5.5, eval = FALSE----
## # Define some graphics to display the distribution of columns
## .hist = anno_histogram(df, gp = gpar(fill = "lightblue"))
## .density = anno_density(df, type = "line", gp = gpar(col = "blue"))
## ha_mix_top = HeatmapAnnotation(hist = .hist, density = .density)
## # Define some graphics to display the distribution of rows
## .violin = anno_density(df, type = "violin",
##                        gp = gpar(fill = "lightblue"), which = "row")
## .boxplot = anno_boxplot(df, which = "row")
## ha_mix_right = HeatmapAnnotation(violin = .violin, bxplt = .boxplot,
##                               which = "row", width = unit(4, "cm"))
## # Combine annotation with heatmap
## Heatmap(df, name = "mtcars",
##         column_names_gp = gpar(fontsize = 8),
##         top_annotation = ha_mix_top,
##         top_annotation_height = unit(3.8, "cm")) + ha_mix_right

## ----combine-multiple-heatmaps, fig.width = 7, fig.height = 5, eval = FALSE----
## # Heatmap 1
## ht1 = Heatmap(df, name = "ht1", km = 2,
##               column_names_gp = gpar(fontsize = 9))
## # Heatmap 2
## ht2 = Heatmap(df, name = "ht2",
##         col = circlize::colorRamp2(c(-2, 0, 2), c("green", "white", "red")),
##         column_names_gp = gpar(fontsize = 9))
## # Combine the two heatmaps
## ht1 + ht2

## You can use the option width = unit(3, "cm")) to control the size of the heatmaps.

## Note that when combining multiple heatmaps, the first heatmap is considered as the main heatmap. Some settings of the remaining heatmaps are auto-adjusted according to the setting of the main heatmap. These include: removing row clusters and titles, and adding splitting.

## ---- eval = FALSE-------------------------------------------------------
## draw(ht1 + ht2,
##     row_title = "Two heatmaps, row title",
##     row_title_gp = gpar(col = "red"),
##     column_title = "Two heatmaps, column title",
##     column_title_side = "bottom",
##     # Gap between heatmaps
##     gap = unit(0.5, "cm"))

## Legends can be removed using the arguments *show_heatmap_legend = FALSE*, *show_annotation_legend = FALSE*.

## ----gene-expression-data, fig.height = 7, fig.width = 5.4, eval = FALSE----
## expr <- readRDS(paste0(system.file(package = "ComplexHeatmap"),
##                       "/extdata/gene_expression.rds"))
## mat <- as.matrix(expr[, grep("cell", colnames(expr))])
## type <- gsub("s\\d+_", "", colnames(mat))
## ha = HeatmapAnnotation(df = data.frame(type = type))
## 
## Heatmap(mat, name = "expression", km = 5, top_annotation = ha,
##     top_annotation_height = unit(4, "mm"),
##     show_row_names = FALSE, show_column_names = FALSE) +
## Heatmap(expr$length, name = "length", width = unit(5, "mm"),
##     col = circlize::colorRamp2(c(0, 100000), c("white", "orange"))) +
## Heatmap(expr$type, name = "type", width = unit(5, "mm")) +
## Heatmap(expr$chr, name = "chr", width = unit(5, "mm"),
##     col = circlize::rand_color(length(unique(expr$chr))))

## It's also possible to visualize genomic alterations and to integrate different molecular levels (gene expression, DNA methylation, ...). Read the vignette, on Bioconductor, for further examples.

## ----matrix-column-distribution, eval = FALSE----------------------------
## densityHeatmap(scale(mtcars))

