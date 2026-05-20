#load the dataset
new_my_data <- read.csv("~/Desktop/TEST/new_my_data.csv")
#check the data set
str(new_my_data)
View(new_my_data)

#divide the new data
pca.train <- new_my_data[1:nrow(new_my_data),]
pca.test <- new_my_data[-(1:nrow(new_my_data)),]

View(pca.test)

#prcomp function centers the variable to have mean equals to zero
prin_comp <- prcomp(pca.train, scale. = T)
names(prin_comp)
summary(prin_comp)

#testing when setting scale as FALSE
prin_comp1 <- prcomp(pca.train, center = TRUE, scale. = FALSE)
summary(prin_comp1)

#step 1 - center and scale refers to 
#respective mean and standard deviation of the variables that are used for normalization prior to implementing PCA

#outputs the mean of variables
prin_comp$center

#outputs the standard deviation of variables
prin_comp$scale

#step 2 - rotation
#The rotation measure provides the principal component loading
#Each column of rotation matrix contains the principal component loading vector
prin_comp$rotation

#this returns 44 principal components loadings
# the maximum number of principal component loadings is a minimum of (n-1, p)
#Let’s look at first 4 principal components and first 5 rows

#step 3
#compute the principal component score vector
dim(prin_comp$x)
#the matrix x has the principal component score vectors in a 8523 × 44 dimension

#plot the results of PC
#The parameter scale = 0 ensures that arrows are scaled to represent the loadings
#looking at the plot at the top, bottow, left and right
#the first PC correspond to a measure of Outlet_TypeSupermarket - right, Outlet_Establishment_Year 2007 - top
#the second PC correspond to a a measure of Outlet_Location_TypeTier1, Outlet_Sizeothe
biplot(prin_comp, scale = 0)

#to be more precise, we will look into variance of PCs
#compute standard deviation of each principal component
std_dev <- prin_comp$sdev

#compute variance
pr_var <- std_dev^2

#check variance of first 10 components
pr_var[1:10]

#We aim to find the components which explain the maximum variance
#To compute the proportion of variance explained by each component
#we simply divide the variance by sum of total variance

#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)

prop_varex[1:20]
#This shows that first principal component explains 10.3% variance
#Second component explains 7.3% variance
#Third component explains 6.2% variance 
#how do we select the component?

#scree plot
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")
#The plot above shows that ~ 30 components explains around 98.4% variance in the data set
#this mean we reduced from 44 to 30 components

#Let’s do a confirmation check, by plotting a cumulative variance plot
#cumulative scree plot

plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")
#This plot shows that 30 components results in variance close to ~ 98%
#the results of PCA can be used to predict by using decision tree or others