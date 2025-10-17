# M04 EDA for One Numerical Variable

# Import data from Excel - Make sure the data set is in the same directory
library(readxl)
Fulldata <- read_excel("Fulldata.xlsx")
View(Fulldata)

# (1) Summarize the numerical variable Age -----
varAge <- Fulldata$AGE

# Summary statistics
summary(varAge) # five-number summary + mean
mean(varAge)
median(varAge)
min(varAge)
max(varAge)
quantile(varAge,probs=c(0.25,0.75)) # first quartile and third quartile
IQR(varAge)
sd(varAge) # standard deviation
var(varAge) # variance
# Create a frequency (distribution) table
table(varAge)
# Create a probability (distribution) table
round(proportions(table(varAge)),2)

# Histogram - basic in frequency
hist(varAge,
     main="Histogram of Age",
     xlab="Age")
# Histogram - in proportions
hist(varAge,
     freq = FALSE,
     main = "Histogram of Age",
     xlab = "Age",
     ylab = "Proportion",
     ylim = c(0,0.15))
# Histogram - with varying breaks/bins
hist(varAge,
     breaks = 10,
     main = "Histogram of Age",
     xlab = "Age",
     xlim = c(15,40),
     col = "#0000FF")

# Boxplot - default in vertical
boxplot(varAge,
        main = "Boxplot of Age",
        ylab = "Age",
        col = "pink",
        ylim = c(15,40))
# Boxplot - horizontal
boxplot(varAge,
        horizontal = TRUE,
        main="Boxplot of Age",
        xlab="Age",
        col="orange",
        ylim=c(15,40))


# (2) Density Plot -----
# Useful when you only want to show the shape
dens <- density(varAge)
dens
plot(dens)
# Density Plot - with colour
polygon(dens, col = "steelblue")


# (3) MISTAKE -----
# Wrong use of Bar Graph for a Numerical Variable
barplot(varAge,
        main="Wrong Graph",)
barplot(table(varAge),
        main="Wrong Graph",)


