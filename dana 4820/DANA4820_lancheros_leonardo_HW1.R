# 19-01-2026
# 
# DANA4820_lancheros_leonardo_HW1.docx
# r studio

# Load the necessary package
# install.packages("readxl")
library(readxl)

path<-"P:/langara/term 3/dana 4820"
file <- file.path(path, "car.csv")

# 3) Read the csv
car <- read.csv(file, stringsAsFactors = TRUE)


# 4) Clean column names
names(car) <- trimws(names(car))

# 5) Check structure
str(car)


# Frequency table
table(car$buying)

# Proportions
prop.table(table(car$buying))

ylimit=500
barplot(
  table(car$buying),
  col = "steelblue",
  main = "Distribution of Buying Price",
  xlab = "Buying Price",
  ylab = "Count",
  ylim = c(0, ylimit),
  yaxt = "n"   # desactiva eje Y por defecto
)

# Agregar eje Y con escala cada 50
axis(2, at = seq(0, ylimit, by = 10))


# Frequency table
table(car$evaluation)

# Proportions
prop.table(table(car$evaluation))


barplot(
  table(car$evaluation),
  col = "darkgreen",
  main = "Distribution of Car Evaluation",
  xlab = "Evaluation",
  ylab = "Count",
  ylim = c(0, 1300),
  yaxt = "n"
)
axis(2, at = seq(0, 1300, by = 100))


table(car$buying, car$evaluation)

# Row-wise proportions
prop.table(table(car$buying, car$evaluation), margin = 1)


barplot(
  prop.table(table(car$buying, car$evaluation), 1),
  beside = FALSE,
  col = c("red", "orange", "lightgreen", "darkgreen"),
  legend = TRUE,
  main = "Buying Price vs Car Evaluation",
  xlab = "Buying Price",
  ylab = "Proportion"
)

# Create a binary variable for acceptable or better
car$acceptable <- car$evaluation %in% c("acc", "good", "vgood")

# Proportion by buying price
aggregate(acceptable ~ buying, data = car, mean)

# Crear la tabla de proporciones (row-wise)
# Crear la tabla de proporciones (row-wise)
prop_tab <- prop.table(table(car$buying, car$evaluation), 1)

# Dibujar el barplot y guardar las posiciones
bp <- barplot(
  prop_tab,
  beside = FALSE,
  col = c("red", "orange", "lightgreen", "darkgreen"),
  legend = TRUE,
  main = "Buying Price vs Car Evaluation",
  xlab = "Buying Price",
  ylab = "Proportion"
)

# Agregar etiquetas de texto
for (i in 1:ncol(prop_tab)) {
  cumulative_height <- 0
  for (j in 1:nrow(prop_tab)) {
    value <- prop_tab[j, i]
    if (value > 0) {
      text(
        x = bp[i],
        y = cumulative_height + value / 2,
        labels = round(value, 2),
        cex = 0.8
      )
    }
    cumulative_height <- cumulative_height + value
  }
}


# Contingency table
table(car$maint, car$buying)

# Row-wise proportions
prop.table(table(car$maint, car$buying), margin = 1)


tab <- prop.table(table(car$maint, car$buying), 1)

barplot(
  tab,
  beside = TRUE,
  legend = TRUE,
  main = "Maintenance Cost vs Buying Price (Row Proportions)",
  xlab = "Maintenance Cost",
  ylab = "Proportion",
  ylim = c(0, 0.35),
  yaxt = "n"
)
axis(2, at = seq(0, 0.35, by = 0.05))

mosaicplot(
  table(car$maint, car$buying),
  main = "Maintenance Cost vs Buying Price",
  xlab = "Maintenance Cost",
  ylab = "Buying Price",
  color = TRUE
)

