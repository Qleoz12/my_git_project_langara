# M05 EDA for Two Variables
library(readxl)

# (1) Two Numerical Variables -----
CorrData <- read_excel("CorrData.xlsx")
View(CorrData)
varStudy <- CorrData$StudyHours
varSocial <- CorrData$SMHours
varGPA <- CorrData$GPA

# Plot can make 2-D scatterplot for all Num. Vars.
plot(CorrData)

# (1A) GPA vs. Study Time
# Just to know the limits for the plots below
summary(varGPA)
summary(varStudy)

# Scatterplot or Scatter Diagram
# Note: For 2 variables, X-var goes first in plot()
plot(varStudy, # X-variable
     varGPA, # Y-variable
     main = "Scatterplot of GPA vs. Study",
     ylab = "GPA",
     xlab = "Study Time (in hrs)",
     cex = 0.8, # size of the dot
     pch = 20, # style of the dot, default is 1
     col = "blue")

# Data Frame is sometimes useful to handle bigger data sets
mydataframe <- data.frame(varStudy,varGPA)
plot(mydataframe,
     main = "Scatterplot of GPA vs. Study",
     ylab = "GPA",
     xlab = "Study Time (in hrs)",
     cex = 0.5, # size of the dot
     pch = 3, # style of the dot
     col = "violet")

# Covariance
cov(varStudy, varGPA)
varMin <- varStudy*60
cov(varMin, varGPA)

cor(varStudy, varGPA)
cor(varMin, varGPA)

# Correlation Coefficient
cor(varStudy, varGPA)
cor(varStudy, varSocial, varGPA) # This will give you an error
cor(mydataframe) 
cor(CorrData) # This shows you the versatility of data frames

# (1B) GPA vs. Social Media Time
plot(varSocial, # X-variable
     varGPA, # Y-variable
     main = "Scatterplot of GPA vs. Social Media",
     ylab = "GPA",
     xlab = "Social Media Time (in hrs)",
     cex = 0.75, # size of the dot
     pch = 4, # style of the dot
     col = "red")

cor(varSocial, varGPA)

# (1C) Time Series Plot
TSData <- read_excel("TSData.xlsx")
View(TSData)
plot(TSData,
     main = "Time Series of Unemployment Rate by Year",
     ylab = "Unemployment Rate (in %)",
     type = "l", # Please look up options for TYPE and LTY
     col = "blue")


# (2) Two Categorical Variables -----
Fulldata <- read_excel("Fulldata.xlsx")
View(Fulldata)
varGender <- Fulldata$GENDER
varMajor <- Fulldata$MAJOR

# You could either use data frame
mydata <- data.frame(varGender, varMajor)
mytab <- table(mydata)
# or insert the two variables in the table() function
mytab <- table(varGender, varMajor)

mytab

# Bar Graph - Basic or Stacked Version
barplot(mytab,
        main = "Bar Graph of Gender & Major",
        ylab = "Frequency",
        xlab = "Major",
        col = c("Steelblue","Orange"))
# Bar Graph - Adding Legend
barplot(mytab,
        legend.text = TRUE,
        main = "Bar Graph of Gender & Major",
        ylab = "Frequency",
        xlab = "Major",
        col = c("Blue","Pink"))
# Bar Graph - Side-by-side Bar Graph
barplot(mytab,
        legend.text = TRUE,
        beside = TRUE,
        main = "Bar Graph of Gender & Major",
        ylab = "Frequency",
        xlab = "Major",
        col = c("Grey","Orange"))

# Find marginal totals - NULL=Table, 1=Row, 2=Column
margin.table(mytab) # Overall Total
margin.table(mytab,1) # Row Totals
margin.table(mytab,2) # Column Totals

# Find marginal % - NULL=Table, 1=Row, 2=Column
round(proportions(mytab),2)
round(proportions(mytab,margin = 1),2)
round(proportions(mytab,margin = 2),2)

# Bar Graph - using column percentages
mycoltab <- proportions(mytab, margin = 2)
mycoltab
barplot(mycoltab,
        legend.text = TRUE,
        beside = TRUE,
        main = "Bar Graph of Gender & Major",
        ylab = "Percentage",
        xlab = "Major",
        col = c("Grey","Orange"))

barplot(mycoltab,
        legend.text = TRUE,
        main = "Bar Graph of Gender & Major",
        ylab = "Percentage",
        xlab = "Major",
        col = c("Blue","Pink"))


# (3) Num. Dep. Var vs. Cat. Indep. Var. -----
varStudy <- Fulldata$STUDY
varGPA <- Fulldata$GPA
varAge <- Fulldata$AGE
varMajor <- Fulldata$MAJOR

boxplot(varStudy ~ varMajor,
        main = "Boxplot of Study Time by Major",
        ylab = "Study Time (in hours)",
        xlab = "Major",
        col = "orange")
boxplot(varGPA ~ varGender,
        horizontal = TRUE,
        main = "Boxplot of GPA by Gender",
        ylab = "Gender",
        xlab = "GPA",
        col = "pink")
boxplot(varAge ~ varGender,
        horizontal = TRUE,
        main = "Boxplot of Age by Gender",
        ylab = "Gender",
        xlab = "Age",
        col = "red")
