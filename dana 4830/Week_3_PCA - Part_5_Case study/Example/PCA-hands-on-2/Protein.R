rm(list=ls())
food <- read.csv("~/Desktop/protein.csv")
food

## correlation matrix
cor(food[,-1])

library(corrplot)
cor_vals <- round(cor(food[,-1]),4)

corrplot(cor_vals, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

pcafood <- prcomp(food[,-1], scale=TRUE)
## we strip the first column (country labels) from the data set
## scale = TRUE: variables are first standardized. Default is FALSE

#Importance of components by answeringhow many principal components do we need?
summary(pcafood)


#Double check the number of PCs by using screeplot
plot(pcafood, type = "l", main="")
mtext(side=1, "European Protein Principal Components", line=1, font=2)

screeplot(pcafood)


names(pcafood)
pcafood$x
#plot on 2 dimensions
biplot(pcafood, col=c("black","red"), cex=c(0.7,0.8))


#Check the amount of influence that each predictor variable has on each principal component.
#For PC 1
loading_Scores_PC_1 <- pcafood$rotation[,1]
fac_scores_PC_1 <- abs(loading_Scores_PC_1)
fac_scores_PC_1_ranked <- names(sort(fac_scores_PC_1,decreasing = T))


#For PC 2
loading_Scores_PC_2 <- pcafood$rotation[,2]
fac_scores_PC_2 <- abs(loading_Scores_PC_2)
fac_scores_PC_2_ranked <- names(sort(fac_scores_PC_2,decreasing = T))

#For PC 3
loading_Scores_PC_3 <- pcafood$rotation[,3]
fac_scores_PC_3 <- abs(loading_Scores_PC_3)
fac_scores_PC_3_ranked <- names(sort(fac_scores_PC_3,decreasing = T))

print("for PC 1")
pcafood$rotation[fac_scores_PC_1_ranked,1]


print("for PC 2")
pcafood$rotation[fac_scores_PC_2_ranked,2]


print("for PC 3")
pcafood$rotation[fac_scores_PC_3_ranked,3]


library(ggplot2)

scores <- data.frame(food, pcafood$x[,1:3])
plot_2 <-ggplot(scores,aes(x=PC1,y=PC2,color=Country )) + geom_point(size =2) + labs(title="Plotting Scores against PC1 and PC2")
plot_2

## how do the PCs look?
par(mfrow=c(1,2))
plot(foodpc[,1:2], type="n", xlim=c(-4,5))
text(x=foodpc[,1], y=foodpc[,2], labels=food$Country)
plot(foodpc[,3:4], type="n", xlim=c(-3,3))
text(x=foodpc[,3], y=foodpc[,4], labels=food$Country)


pcafood$rotation[,2]
