rm(list = ls())

##load library
library(ggplot2)
library("FactoMineR")
library("factoextra")

##load contigency table
crime <- read.delim("~/Desktop/CrimeA.txt", row.names=1) # read file 
View(crime)

#In our example, the row and the column variables are statistically significantly associated (p-value < 0.05).
chisq <- chisq.test(crime)
chisq

#library factoMineR
library("FactoMineR")
library("factoextra")
res.ca.crime <- CA(crime, graph = TRUE)#symetric plot shows a global pattern within the data
summary(res.ca.crime)
print(res.ca.crime)
res.ca.crime$row

library("factoextra")
#we examine the eigenvalues to determine the number of axis to be considered
#Eigenvalues correspond to the amount of information retained by each axis. Dimensions are ordered decreasingly and listed according to the amount of variance explained in the solution. Dimension 1 explains the most variance in the solution, followed by dimension 2
#The cumulative percentage explained is obtained by adding the successive proportions of variation explained to obtain the running total. For instance, 90.89% plus 39.91% equals 9.11%, and so forth. Therefore, about 99.99% of the variation is explained by the first two dimensions.
eig.val <- get_eigenvalue(res.ca.crime)
eig.val

#we can use scree plot to determine the number of dimensions
#eigenvalue
fviz_screeplot(res.ca.crime,addlabels=T) + 
  geom_hline(yintercept=50,linetype=2,color="red")

# Contributions to the principal components
head(row$contrib)

#contribution of axis 1 - column
fviz_contrib(res.ca.crime, choice="col",axes=1)

#contribution of axis 1 - row
fviz_contrib(res.ca.crime, choice="row",axes=1)


row <- get_ca_row(res.ca.crime)
row
#row$coord: coordinates of each row point in each dimension (1, 2 and 3). Used to create the scatter plot.
#row$cos2: quality of representation of rows.
#var$contrib: contribution of rows (in %) to the definition of the dimensions.
# Coordinates
head(row$coord)
# Cos2: quality on the factore map
head(row$cos2)#The values of the cos2 are comprised between 0 and 1. The sum of the cos2 for rows on all the CA dimensions is equal to one.
#quality of fit for columns
fviz_ca_col(res.ca.crime, col.col = "cos2",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))


#quality of fit for rows
fviz_ca_row(res.ca.crime, col.row = "cos2",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
fviz_ca_row(res.ca.crime, col.row = "cos2",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
            repel = TRUE)


#library ade4
library(ade4)
#scannf = a logical value indicating whether the eigenvalues bar plot should be displayed
res.ca.ade <- dudi.coa(crime, scannf = FALSE, nf = 4)
res.ca.ade
res.ca.ade$tab
res.ca.ade$li
res.ca.ade$co
  
##Asymmetric plot (row space)
fviz_ca_biplot(res.ca.crime,repel=T,map="rowprincipal")

##Visualize the results for rows
fviz_ca_row(res.ca.crime)

##Visualize the results for columns
fviz_ca_col(res.ca.crime)
