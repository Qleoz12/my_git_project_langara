rm(list=ls())

library(factoextra)#visualization


#load data
data<-read.csv("~/Desktop/wine.csv", header = TRUE)
summary(data)
names(data)


#correlation
cor(mydata[,-1])

#select 5 variables for the convenience
mydata <- data[,1:6]
View(data)

#another function to calculate PCA
wine.pca<-prcomp(mydata[,-1], scale = TRUE)
names(wine.pca)#properties of principal component results

print(wine.pca)
summary(wine.pca)

biplot(wine.pca)
#normal visualization
screeplot(wine.pca, main ="Scree Plot", xlab="Components")
screeplot(wine.pca, main="Scree Plot", type="line")

#if you face error Call.graphics
# run the function dev.off()

#visualize the eigenvalues 
fviz_eig(wine.pca)


#variable visualiztion
fviz_pca_var(wine.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#biplot
fviz_pca_biplot(wine.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

fviz_pca_ind(wine.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)


library(factoextra)
# Eigenvalues
eig.val <- get_eigenvalue(wine.pca)
eig.val#3 PCs or 4PCs


