# 18-08-2025
# 
# DANA4800_lancheros_leonardo_HW1.docx
# r studio

# Load the necessary package
# install.packages("readxl")
library(readxl)

path<-"P:/langara/term 1/DANA-4800-001 - Data Analysis and Stat Infer  20287.202520"
file <- file.path(path, "DANA4800_HW1_Q07_Data.xlsx")
group <- read_excel(file)

# Create a frequency table of the variable 'Distracted'
freq_table <-table(group$Distracted)
print(freq_table)
# Display proportions
prop_table <- prop.table(freq_table)
print(prop_table)




# Sample proportion of "TRUE" (distracted individuals)
p_hat <- prop.table(freq_table)["Yes"] #exactly values
p_hat

# Create a labeled frequency table with "Yes" and "No" instead of TRUE/FALSE
# and reorder so "Yes" comes before "No"
freq_table_named <- c(freq_table["Yes"], freq_table["No"])
percentages <- round(100 * freq_table_named / sum(freq_table_named), 1)
labels <- paste0(names(freq_table_named), ": ", percentages, "%")

title<-tools::toTitleCase('distracted drivers')
plotpie<-pie(freq_table_named,
             clockwise = TRUE,
             labels = labels,
             main = paste("piechart of  ", title,sep = " "))

# Produce the bar graph
bar_midpoints <-barplot(freq_table_named, 
        main = "Bar Graph of Distracted Responses", 
        ylab = "Frequency", 
        xlab = "Distracted", 
        col = c("skyblue", "orange"),
        ylim = c(0, 50))

# Add labels above each bar
text(x = bar_midpoints, 
     y = freq_table_named + 1,  # Position slightly above bar
     labels = freq_table_named, 
     cex = 1.2)  # Text size (optional)    








# 8 point
# Define the room rates
rates <- c(286, 378, 245, 292, 244, 314, 298, 319, 282, 237, 
           289, 275, 285, 227, 270, 322, 281, 274, 317, 293)

# Calculate the average
average_rate <- mean(rates)

# Print the result
average_rate


# Calculate the median
median_rate <- median(rates)

# Print the result
median_rate




# Calculate los cuartiles
quartiles <- quantile(rates)

# shows Q1 (first cuartil)
Q1 <- quartiles[2]
Q1

# shows Q3 (tercer cuartil)
Q3 <- quartiles[4]
Q3

# Calculate  range intercuartilico
IQR_value <- IQR(rates)
IQR_value

# Calculate fences
lower_fence <- Q1 - 1.5 * IQR_value
upper_fence <- Q3 + 1.5 * IQR_value

# Find outliers
outliers <- rates[rates < lower_fence | rates > upper_fence]

# Print results
list(
  Q1 = Q1,
  Q3 = Q3,
  IQR = IQR_value,
  Lower_Fence = lower_fence,
  Upper_Fence = upper_fence,
  Outliers = outliers
)




# 9. A first-year Langara student wanted to know the weekly 
# expenses (in CAD$) of typical Langara students.
# To investigate this, she got a random sample of 100 students 
# this term. The data set is in “Expense” worksheet of the file 
# “DANA4800_HW1_Q09_Data.xlsx” on BrightSpace. 

file_HW1_Q09 <- file.path(path, "DANA4800_HW1_Q09_Data.xlsx")
group2 <- read_excel(file_HW1_Q09)
summary(group2)

# Assuming the data is in the first column, extract it:
data <- group2[[1]]  # Adjust if your column name is known, e.g., group2$Score
data
# Create histogram with 6 specific breaks and proportion on y-axis
titleHistogramStudent <- tools::toTitleCase("Histogram of Expenses Weekly by Student")

# Crear histograma y guardar el objeto (proporciones)
hist_data <- hist(
  data,
  breaks = seq(0, 60, by = 10),
  freq = FALSE,                     # Proporciones
  main = titleHistogramStudent,
  xlab = "Ranges Expenses",
  ylab = "Proportion",
  col = "skyblue",
  border = "black",
  ylim = c(0, 0.05)
)

# Agregar los valores de proporción encima de las barras
text(
  x = hist_data$mids,
  y = hist_data$density,             # Usar proporciones (no counts)
  labels = round(hist_data$density, 3),  # Etiquetas redondeadas
  pos = 3,
  cex = 0.8,
  col = "black"
)


#Density Plot - 
# searched online not present in pdf looks like a link but no open any
dens <- density(data) #data is numeric representation of label expeenses
dens
# Plot the density curve with custom labels
# Plot the density curve
plot(
  dens,
  main = "Density Curve of Weekly Student Expenses",
  xlab = "Expenses",
  ylab = "Density",
  col = "darkgreen",
  lwd = 2,
  ylim = c(0, 0.05)
)

# Fill the curve with green
polygon(dens, col = "lightgreen", border = "darkgreen")

# Identify 3 points:
peak_index <- which.max(dens$y)           # Peak of the curve
left_index <- which.min(abs(dens$x - 15)) # Around 15 (start)
right_index <- which.min(abs(dens$x - 45))# Around 45 (end)

# Points to label
points_to_label <- c(left_index, peak_index, right_index)

# Add points and text labels
points(dens$x[points_to_label], dens$y[points_to_label], col = "red", pch = 19)

# Label each point with x (expense) and y (density)
text(
  x = dens$x[points_to_label],
  y = dens$y[points_to_label],
  labels = paste0("(", round(dens$x[points_to_label], 1), ", ", round(dens$y[points_to_label], 3), ")"),
  pos = 3,
  cex = 0.8,
  col = "black"
)

# Boxplot - horizontal
boxplot(data,
        horizontal = TRUE,
        main="Boxplot of Weekly Student Expense",
        xlab="Expense (CAD$)",
        col="orange",
        ylim=c(0,60))




# 10.	 A dietician wanted to find out how the total fat content (Fat; measured 
# in grams per serving) is dependent on the amount of calories (Calories; measured 
# in calories) among chicken burgers made from different fast food chains in Canada.
# A random sample of 20 chicken burgers was collected from different fast food chains 
# (one burger per fast food chain) and the information was recorded. 
# The data set is from “DANA4800_HW1_Q10_Data.xlsx” on BrightSpace.

file_HW1_Q10 <- file.path(path, "DANA4800_HW1_Q10_Data.xlsx")
group3 <- read_excel(file_HW1_Q10)
summary(group3)
VaraibleCaloriesI <- group3$Calories
VariablefFatD <- group3$Fat
VaraibleCaloriesI

# Scatterplot or Scatter Diagram
plot(VaraibleCaloriesI, # X-variable
     VariablefFatD, # Y-variable
     main = "Scatterplot of Fat vs. Calories",
     ylab = "Fat (in gr)",
     xlab = "Calories (in calories)",
     cex = 1, # size of the dot
     pch = 20, # style of the dot, default is 1
     col = "darkgreen")



# Covariance
cov_matrix=cov(group3)
cov_round <- round(cov_matrix,digits=1)
cov_round

#correlation

cor_coefficiente <- cor(group3)
cor_coefficiente_round <-round(cor_coefficiente, digits=4)
cor_coefficiente_round




# 
# 11.	Trying to accurately allocate labour hours in a moving job, the manager of a
# moving company would like to develop a method of predicting the 
# labour hours (Labour; measured in hours) based on the size of the high-rise apartment 
# (Size; measured in cubic feet). A random sample of 25 high-rise
# apartment moves was randomly selected 
# in downtown Vancouver in the previous calendar year. 
# The data set is in “DANA4800_HW1_Q11_Data.xlsx” on BrightSpace.


file_HW1_Q11 <- file.path(path, "DANA4800_HW1_Q11_Data.xlsx")
group4 <- read_excel(file_HW1_Q11)
summary(group4)
VaraibleI <- group4$Size
VariableD <- group4$Labour

# Scatterplot or Scatter Diagram
plot(VaraibleI, # X-variable
     VariableD, # Y-variable
     main = "Scatterplot of Labour vs. Size",
     ylab = "Labour (in hrs)",
     xlab = "size (in Cubic Feet)",
     cex = 1, # size of the dot
     pch = 20, # style of the dot, default is 1
     col = "darkgreen")


# Covariance
cov_matrix=cov(group4)
cov_round <- round(cov_matrix,digits=1)
cov_round

#correlation

cor_coefficiente <- cor(group4)
cor_coefficiente_round <-round(cor_coefficiente, digits=4)
cor_coefficiente_round


# 12.	The PopularKids data set was about opinions of a group of primary school 
# students, who were stratified by their origin (rural, suburban and urban). 
# More information about the data set can be in the following 
# link: https://www.openml.org/search?type=data&sort=runs&id=1100&status=active
# The data set is in “DANA4800_HW1_Q12_Data.xlsx” on BrightSpace.
# Note: In this question, let us only use Gender (boy and girl) as the row 
# variable and Goal (Grades, Popular, and Sports) as the column variable. 
# Every subsequent mentioning of “row” and “column” refer to this definition


file_HW1_Q12 <- file.path(path, "DANA4800_HW1_Q12_Data.xlsx")
group5 <- read_excel(file_HW1_Q12)
summary(group5)


VaraibleGender <- group5$Gender
VariableGoal <- group5$Goals

df <- data.frame(VaraibleGender, VariableGoal)
df
# Step 3: Create a two-way table (Gender as rows, Goal as columns)
twowaytable <- table(df)
print(twowaytable)

# Step 4: Compute column-wise proportions (margin = 2 means "among columns")
prop_table <- prop.table(twowaytable, margin = 2)
print(round(prop_table * 100, 1))  # convert to percentages and round

# Step 5: Extract percentage of boys among those whose goal is "Popular"
boy_popular_percentage <- prop_table["boy", "Popular"] * 100
print(paste("Percentage of boys whose goal is Popular:", round(boy_popular_percentage, 1), "%"))

# Compute table percentages
# Round to two decimal places
table_percentages <- prop.table(twowaytable) * 100
rounded_table_percentages <- round(table_percentages, 2)
rounded_table_percentages

# Compute row-wise proportions Round to two decimal places
row_percentages <- proportions(twowaytable, margin = 1) * 100
rounded_row_percentages <- round(row_percentages, 2)
rounded_row_percentages


# Compute column-wise proportions (margin = 2 means "among columns")
prop_table <- prop.table(twowaytable, margin = 2)
print(round(prop_table * 100, 1))  # convert to percentages and round



# Step 1: Get column percentages (proportions within each Goal)
col_percents <- prop.table(twowaytable, margin = 2) * 100

# Step 2: Create a side-by-side barplot
bar_positions <-barplot(
  col_percents,
  beside = TRUE,
  col = c("lightgreen", "pink"),
  ylim = c(0, 100),
  main = "Side-by-side Bar Graph of Goals by Gender",
  ylab = "Column Percentage (%)",
  xlab = "Goals",
  legend.text = rownames(col_percents),
  args.legend = list(title = "Gender", x = "topright")
)

# Step 3: Add text labels on top of each bar
text(
  x = bar_positions,
  y = col_percents,
  labels = round(col_percents, 1),
  pos = 3,            # position text above the bar
  cex = 1,          # size of text
  col = "black"
)
