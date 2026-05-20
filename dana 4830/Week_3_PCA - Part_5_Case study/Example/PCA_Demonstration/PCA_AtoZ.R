rm(list=ls())

library("FactoMineR")
library("factoextra")
library("ggplot2")

#data includes 27 athletes and 13 vairables about performance
decathlon <- read.csv2("C:/Users/qnguyen/Documents/Langara/DANA4830/Topics/Section-3-PCA/PCA_Demonstration/decathlon.csv")


rownames(decathlon) <- decathlon[,1]
decathlon <- decathlon[,-1]
View(decathlon)# head(decathlon2)

#select active individuals and numerical variables
decathlon.active <- decathlon[1:23, 1:10]

#inspect the correlation pattern of the variables
cor(decathlon.active) #discus + shot.put = high correlation
                      #X100m + X110.hurdle = correlation

#reduce the dimensions
#prcomp(x, scale = FALSE)
#if variables has differences in rage, scale should be true
decathlon.pca <- prcomp(decathlon.active, scale = TRUE)

summary(decathlon.pca)
names(decathlon.pca)

#the standard deviations of the principal components
  decathlon.pca$sdev#equal to the square root of the eigenvalues

decathlon.pca$rotation #the matrix of variable loadings into each components

decathlon.pca$center#the variable means

decathlon.pca$scale#the variable standard deviations (the scalings applied to each variable )

decathlon.pca$x#The coordinates of the individuals

#understand the result of PCA
library(factoextra)
# Eigenvalues - rule of thumb is to select components with eigenvalues larger than 1
eig.val <- get_eigenvalue(decathlon.pca)
eig.val

# Results for Variables
res.var <- get_pca_var(decathlon.pca)
res.var$coord          # Coordinates



#visualise eigenvalues (scree plot)
fviz_eig(decathlon.pca)


#Graph of variable
#Positive correlated variables point to the same side of the plot
#Negative correlated variables point to opposite sides of the graph.
fviz_pca_var(decathlon.pca,
             col.var = "contrib",
             repel = TRUE     # Avoid text overlapping
)

#biplot for variables and observations
fviz_pca_biplot(decathlon.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)


#when the observations are not many
#graphs of observation to see similar profile are grouped together
fviz_pca_ind(decathlon.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#Visualize qualitative data
groups <- as.factor(decathlon$Competition[1:23])
fviz_pca_ind(decathlon.pca,
             col.ind = groups, # color by groups
             palette = c("#00AFBB",  "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Groups",
             repel = TRUE
)




#########################

#how to predict the coordinates of supplementary individuals and variables using 
#only the information provided by the previously performed PCA.

# Data for the supplementary individuals
ind.sup <- decathlon[24:27, 1:10]



#Predict the coordinates of new individuals data
ind.sup.coord <- predict(decathlon.pca, newdata = ind.sup)
ind.sup.coord[, 1:4]

#
