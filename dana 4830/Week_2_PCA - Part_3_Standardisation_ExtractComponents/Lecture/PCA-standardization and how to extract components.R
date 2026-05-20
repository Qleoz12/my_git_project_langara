
df <- read.csv("PCA1standardization.csv")
View(PCA1standardization)

df <- PCA1standardization

#slide7
df.pca <- prcomp(df)
df.pca


names(df.pca)
df.pca$rotation#linear combination for all PCs
df.pca$center#the variable means
df.pca$scale

#slide 10 - score of PCA dataset
df.pca$x
df.pca$sdev#equal to the square root of the eigenvalues
          #(2.41)^2 = 1.5509

#slide 13
summary(df.pca)

#slide15
library(factoextra)
fviz_eig(df.pca)

#loadings - slide 7 of weights and loadings
res.var <- get_pca_var(df.pca)
res.var$coord # Coordinates
              # The loading of -0.82 tells us the correlation coefficient between Diastolic.BP and PC1 is -0.82


# Eigenvalues - rule of thumb is to select components with eigenvalues larger than 1
#slide 6
eig.val <- get_eigenvalue(df.pca)
eig.val

#graphs of observation to see similar profile are grouped together
fviz_pca_ind(df.pca,
             col.ind = "cos2", # Color by the quality of representation
             repel = TRUE     # Avoid text overlapping
)

#rotation
varimax.one <- varimax(df.pca$loadings)
varimax.one

##principal fun from psych
#loadings in psych are not unit eigenvectors because it uses them for factor rotation
library(psych)
pca_fit <- principal(df, nfactors = 2, rotate = "none")
pca_fit$loadings


pca_rot <- principal(r = df, rotate = "varimax", nfactors = 2)
pca_rot$loadings

