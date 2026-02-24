#Activity 3_1
TAMPALMS <- read.delim("TAMPALMS.txt")
View(TAMPALMS)
head(TAMPALMS)
attach(TAMPALMS)
plot(x=Market_Val, y=Sale_Price, ylab = "Sales price in $thousands", xlab = "Appraised price in $ thousands", 
     main = "Scatterplot of Sales Revenue vs. Advertising Expenditure" )

abline(lm(Sale_Price~Market_Val), col="Red", lty=1, lwd=2)

#Regression
model=lm(Sale_Price~Market_Val)
summary(model)

summary(Market_Val)

##ANOVA Tabel
anova(model)
dim(TAMPALMS)

sqrt(349833/74)

68.86^2
#Activity 3_2

FHWABRIDGE <- read.delim("FHWABRIDGE.txt")
View(FHWABRIDGE)
attach(FHWABRIDGE)

model_1 <- lm(SDArea ~ NumberSD)
summary(model_1)


plot(NumberSD, SDArea)
abline(model_1)

anova(model_1)

s <- sqrt(20173064/(52-2))
s

#2.
model=lm(Sale_Price~Market_Val)
summary(model)

confint(model,level=0.95)







