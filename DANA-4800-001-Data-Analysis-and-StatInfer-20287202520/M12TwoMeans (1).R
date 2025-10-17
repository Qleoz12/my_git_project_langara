
# Import data from Excel - Make sure the data set is in the same directory
library(readxl)
Data <- read_excel("SalesComparison.xlsx")
View(Data)

summary(Data)

# One-Sample t-Tests - To see if mu is less than 50 ----
t.test(Data$V1,
       alternative="less",
       mu = 50)

# Two-Sample t-Tests - Same Sample Size ----
# Checking the Equal Variance Assumption
var(Data)

# Two-Sample t-Test - To see if mu1 is less than mu2 (with equal variance assumption)
t.test(Data$V1,
       Data$V2, 
       alternative="less",
       mu = 0,
       var.equal = TRUE)

# Two-Sample t-Test - To see if mu1 is less than mu2 (without equal variance assumption)
t.test(Data$V1,
       Data$V2, 
       alternative="less",
       mu = 0,
       var.equal = FALSE)
# Note the two degrees of freedom are almost the same with identical n's

# Two-Sample t-Tests - Different Sample Size ----
VerA <- c(54.7, 48.7, 56.2, 64.5, 47.8, 47.8, 65, 57.3, 45.5, 55.2, 45.6, 45.6, 52.3,
          31.8, 33.6, 44.6, 40.4, 53, 41.4, 36.6, 64, 47.9, 50.6, 36.4, 44.8)
VerB <- c(57.3, 50.8, 48.1, 59.9, 63.8, 62.8, 46.6, 51.5, 57.3, 63.2)
length(VerA)
length(VerB)

# Two-Sample t-Test - To see if mu1 is less than mu2 (with equal variance assumption) ----
t.test(VerA,
       VerB, 
       alternative="less",
       mu = 0,
       var.equal = TRUE)

# Two-Sample t-Test - To see if mu1 is less than mu2 (without equal variance assumption) ----
t.test(VerA,
       VerB,
       alternative="less",
       mu = 0,
       var.equal = FALSE)
# Note the two degrees of freedom are very different with different n's
