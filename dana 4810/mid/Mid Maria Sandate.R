#Data
low.btw <- load("low_bwt.Rdata")
attach(low.bwt)

### Model completed

#a)

model1 <- lm(birthwt~toxemia,)
summary(model1)

# i) Equation  birth weight = 1097.215 +7.785*Toxemia Yes

# ii) CI(-124.)

confint(model1,level=0.95)
