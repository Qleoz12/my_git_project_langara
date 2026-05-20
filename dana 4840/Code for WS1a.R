Age<-c(22,33,52,46)
Race<-factor(c(1,3,1,6))
Height<-factor(c("Tall", "Short", "Moderate", "Tall"), order = TRUE, levels = c("Short", "Moderate", "Tall"))
Income<-c(0.39, 0.34, 0.51, 0.63)
Ismale<-factor(c(TRUE, TRUE, FALSE, FALSE))
Politics<-factor(c("moderate", "liberal", "moderate", "conservative"))

df<-data.frame(Age, Race, Height, Income, Ismale, Politics)

library(cluster)
gowerdistance<-daisy(df)
gowerdistance
gowermatrix<-as.matrix(gowerdistance)
gowermatrix
gowermatrix2<-round(gowermatrix, 4)
gowermatrix2
