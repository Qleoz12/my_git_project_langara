dev.off()#clear output
rm(list=ls())

#PCA and linear prediction
BosData <- read.csv("~/Desktop/Testing/BostonHousing.csv") 
View(BosData)

#CHAS and CAT..MEDV categorical variable so we remove from the dataset
BosData <- BosData[,-4]
BosData <- BosData[,-13]
names(BosData)
str(BosData)

names(BosData)
#splitting data
library(caTools)
set.seed(123)
split <- sample.split(BosData,SplitRatio = 0.70)
train_set<- subset(BosData,split==T)
test_set<- subset(BosData,split==F)
View(train_set)

#fitting PCA
pca_train <- train_set[1:11]
pca = prcomp(pca_train,scale. = T)
summary(pca)
#4PCs explain 82% of total variability
#PC1: NOX, INDUS, TAX, DIS, RAD - locations (TAX:INDUS: 0.7; TAX:NOX: 0.7, DIS:TAX: 0.5)
#PC2: AGE, PTRATIO, LSTAT - demographics 
#PC3: ZN, RM - size
#PC4: CRIM - crime

#linear regression on original data
ln.model <- lm(MEDV~ ., data = train_set)
summary(ln.model)

#Generate loading matrix
Matrix <- pca$rotation
Matrix

#Generate PCA Loadings
loadings <- as.data.frame(pca$x)
str(loadings)
Y_train <- train_set$MEDV
pca_train2 <- cbind(loadings,Y_train)

#create a new dataset for 4PCs
pca_train2 <- loadings[,1:4]
Y_train <- train_set$MEDV
pca_train2 <- cbind(loadings[,1:4],Y_train)


#using this new dataset to do prediction using linear regression
lin_model <- lm(Y_train~.,data=pca_train2)
summary(lin_model)


############
#looking at test set
pca_test <- test_set[1:11]
names(pca_test)
View(pca_test)

pca_test2 <- predict(pca, newdata = pca_test)

# convert the above output into a dataset and add the dependent variable to it 
#so that we can predict values using the above created Linear Regression Model
pca_test2 <- as.data.frame(pca_test2)
View(pca_test2)

pca_test3 <- pca_test2[1:7]
Y_test <- test_set$CAT..MEDV
pca_test4 <- cbind(pca_test3,Y_test)
predict1 <- predict(lin_model,pca_test3)

error <- Y_test - predict1 #observed values on test set - predicted values on test set
mse <- mean(error^2)
R2=1-sum(error^2)/sum((Y_test- mean(Y_test))^2)
R2
