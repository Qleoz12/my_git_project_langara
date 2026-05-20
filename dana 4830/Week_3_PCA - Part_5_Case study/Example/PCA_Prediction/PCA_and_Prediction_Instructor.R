dev.off()#clear output
rm(list=ls())
#https://www.datavedas.com/dimensionality-reduction-in-r/

#PCA and linear prediction
BosData <- read.csv("~/Desktop/Testing/BostonHousing.csv") 
View(BosData)

#CHAS is categorical variable so we remove from the dataset
BosData <- BosData[,-4]
BosData <- BosData[,-12]
names(BosData)
str(BosData)

names(BosData)
#splitting data
library(caTools)
set.seed(123)
split <- sample.split(BosData$CAT..MEDV,SplitRatio = 0.70)
train_set<- subset(BosData,split==T)
test_set<- subset(BosData,split==F)
View(train_set)

#fitting PCA
pca_train <- train_set[1:11]
pca = prcomp(pca_train,scale. = T)
summary(pca)
#7PCs explain 94%


#Generate loading matrix
Matrix <- pca$rotation
Matrix

#Generate PCA Loadings
loadings <- as.data.frame(pca$x)
str(loadings)
Y_train <- train_set$CAT..MEDV
pca_train2 <- cbind(loadings,Y_train)

#create a new dataset for 7PCs
pca_train2 <- loadings[,1:7]
Y_train <- train_set$CAT..MEDV
pca_train2 <- cbind(loadings[,1:7],Y_train)


#using this new dataset to do prediction using linear regression
lin_model <- lm(Y_train~.,data=pca_train2)
summary(lin_model)


############
#looking at test set
pca_test <- test_set[1:11]
names(pca_test)

pca_test2 <- predict(pca, newdata = pca_test)

# convert the above output into a dataset and add the dependent variable to it 
#so that we can predict values using the above created Linear Regression Model
pca_test2 <- as.data.frame(pca_test2)
View(pca_test2)
pca_test3 <- pca_test2[1:7]
Y_test <- test_set$MEV
pca_test4 <- cbind(pca_test3,Y_test)
