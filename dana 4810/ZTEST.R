BONES <- read.csv("H:/DANA/Ch1/BONES.csv")
View(BONES)
attach(BONES)
head(BONES)
summary(BONES)

t.test(BONES,mu=8.5, alternative='two.sided')

p_value

###Rejection Region

z = qnorm((0.05)/2, lower.tail = FALSE)
z
####T-Test

BENZENE <- read.csv("H:/DANA/Ch1/BENZENE.csv")
View(BENZENE)
attach(BENZENE)
head(BENZENE)
summary(BENZENE)

t.test(ï..Benzene,mu=1, alternative='greater')