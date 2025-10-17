# 18-08-2025
# 
# DANA4800_lancheros_leonardo_HW1.docx
# r studio

# Load the necessary package
# install.packages("readxl") # Uncomment to install if needed
library(readxl) # Load the readxl package for reading Excel files

path <- "P:/langara/term 1/DANA-4800-001 - Data Analysis and Stat Infer  20287.202520" # Set the file path
file <- file.path(path, "DANA4800_HW1_Q07_Data.xlsx") # Combine path and filename
group <- read_excel(file) # Read the Excel file into a data frame

# Create a frequency table of the variable 'Distracted'
freq_table <- table(group$Distracted) # Count occurrences of each value in 'Distracted'
print(freq_table) # Print the frequency table

# Display proportions
prop_table <- prop.table(freq_table) # Calculate proportions for each category
print(prop_table) # Print the proportions

# Sample proportion of "TRUE" (distracted individuals)
p_hat <- prop.table(freq_table)["Yes"] # Proportion of "Yes" responses
p_hat # Print the sample proportion

# Create a labeled frequency table with "Yes" and "No" instead of TRUE/FALSE
# and reorder so "Yes" comes before "No"
freq_table_named <- c(freq_table["Yes"], freq_table["No"]) # Reorder and select "Yes" and "No"
percentages <- round(100 * freq_table_named / sum(freq_table_named), 1) # Calculate percentages
labels <- paste0(names(freq_table_named), ": ", percentages, "%") # Create labels for the pie chart

title <- tools::toTitleCase('distracted drivers') # Capitalize the title
plotpie <- pie(freq_table_named, # Draw a pie chart
             clockwise = TRUE, # Draw clockwise
             labels = labels, # Use custom labels
             main = paste("piechart of  ", title, sep = " ")) # Set the main title

# Produce the bar graph
bar_midpoints <- barplot(freq_table_named, # Draw a bar plot
        main = "Bar Graph of Distracted Responses", # Set the main title
        ylab = "Frequency", # Y-axis label
        xlab = "Distracted", # X-axis label
        col = c("skyblue", "orange"), # Bar colors
        ylim = c(0, 50)) # Y-axis limits

# Add labels above each bar
text(x = bar_midpoints, # X positions (bar centers)
     y = freq_table_named + 1, # Y positions (just above bars)
     labels = freq_table_named, # Labels (frequencies)
     cex = 1.2) # Text size

# 8 point
# Define the room rates
rates <- c(286, 378, 245, 292, 244, 314, 298, 319, 282, 237, 
           289, 275, 285, 227, 270, 322, 281, 274, 317, 293) # Room rates data

# Calculate the average
average_rate <- mean(rates) # Mean of room rates

# Print the result
average_rate # Print average

# Calculate the median
median_rate <- median(rates) # Median of room rates

# Print the result
median_rate # Print median

# Calculate los cuartiles
quartiles <- quantile(rates) # Calculate quartiles

# shows Q1 (first cuartil)
Q1 <- quartiles[2] # First quartile (25th percentile)
Q1 # Print Q1

# shows Q3 (tercer cuartil)
Q3 <- quartiles[4] # Third quartile (75th percentile)
Q3 # Print Q3

# Calculate  range intercuartilico
IQR_value <- IQR(rates) # Interquartile range (Q3 - Q1)
IQR_value # Print IQR

# Calculate fences
lower_fence <- Q1 - 1.5 * IQR_value # Lower fence for outliers
upper_fence <- Q3 + 1.5 * IQR_value # Upper fence for outliers

# Find outliers
outliers <- rates[rates < lower_fence | rates > upper_fence] # Identify outliers

# Print results
list(
  Q1 = Q1,
  Q3 = Q3,
  IQR = IQR_value,
  Lower_Fence = lower_fence,
  Upper_Fence = upper_fence,
  Outliers = outliers
) # Print summary of quartiles, fences, and outliers

# 9. A first-year Langara student wanted to know the weekly 
# expenses (in CAD$) of typical Langara students.
# To investigate this, she got a random sample of 100 students 
# this term. The data set is in “Expense” worksheet of the file 
# “DANA4800_HW1_Q09_Data.xlsx” on BrightSpace. 

file_HW1_Q09 <- file.path(path, "DANA4800_HW1_Q09_Data.xlsx") # Path to Q09 data
group2 <- read_excel(file) # Read the Excel file (should be file_HW1_Q09)
summary(group2) # Print summary statistics

# Assuming the data is in the first column, extract it:
data <- group2[[1]]  # Extract first column (expenses data)
data # Print data

# Create histogram with 6 specific breaks and proportion on y-axis
titleHistogramStudent <- tools::toTitleCase("Histogram of Expenses Weekly by Student") # Title

# Crear histograma y guardar el objeto (proporciones)
hist_data <- hist(
  data, # Data to plot
  breaks = seq(0, 60, by = 10), # Breaks for bins
  freq = FALSE, # Show proportions (density)
  main = titleHistogramStudent, # Main title
  xlab = "Ranges Expenses", # X-axis label
  ylab = "Proportion", # Y-axis label
  col = "skyblue", # Bar color
  border = "black", # Bar border color
  ylim = c(0, 0.5) # Y-axis limits
)

# Agregar los valores de proporción encima de las barras
text(
  x = hist_data$mids, # X positions (bar centers)
  y = hist_data$density, # Y positions (bar heights)
  labels = round(hist_data$density, 3), # Rounded density values
  pos = 3, # Position above bars
  cex = 0.8, # Text size
  col = "black" # Text color
)

# Density Plot - 
# searched online not present in pdf looks like a link but no open any
dens <- density(data) # Calculate density estimate for data
dens # Print density object

# Plot the density curve with custom labels
plot(
  dens, # Density object
  main = "Density Curve of Weekly Student Expenses", # Main title
  xlab = "Expenses", # X-axis label
  ylab = "Density", # Y-axis label
  col = "darkgreen", # Line color
  lwd = 2, # Line width
  ylim = c(0, 0.05) # Y-axis limits
)

# Fill the curve with green
polygon(dens, col = "lightgreen", border = "darkgreen") # Fill under curve

# Identify 3 points:
peak_index <- which.max(dens$y) # Index of peak (max density)
left_index <- which.min(abs(dens$x - 15)) # Index near x=15
right_index <- which.min(abs(dens$x - 45)) # Index near x=45

# Points to label
points_to_label <- c(left_index, peak_index, right_index) # Indices to label

# Add points and text labels
points(dens$x[points_to_label], dens$y[points_to_label], col = "red", pch = 19) # Mark points

# Label each point with x (expense) and y (density)
text(
  x = dens$x[points_to_label], # X positions
  y = dens$y[points_to_label], # Y positions
  labels = paste0("(", round(dens$x[points_to_label], 1), ", ", round(dens$y[points_to_label], 3), ")"), # Labels
  pos = 3, # Position above points
  cex = 0.8, # Text size
  col = "black" # Text color
)

# Boxplot - horizontal
boxplot(data,
        horizontal = TRUE, # Horizontal boxplot
        main = "Boxplot of Weekly Student Expense", # Main title
        xlab = "Expense (CAD$)", # X-axis label
        col = "orange", # Box color
        ylim = c(0, 60)) # X-axis limits

# 10.	 A dietician wanted to find out how the total fat content (Fat; measured 
# in grams per serving) is dependent on the amount of calories (Calories; measured 
# in calories) among chicken burgers made from different fast food chains in Canada.
# A random sample of 20 chicken burgers was collected from different fast food chains 
# (one burger per fast food chain) and the information was recorded. 
# The data set is from “DANA4800_HW1_Q10_Data.xlsx” on BrightSpace.

file_HW1_Q10 <- file.path(path, "DANA4800_HW1_Q10_Data.xlsx") # Path to Q10 data
group3 <- read_excel(file_HW1_Q10) # Read the Excel file
summary(group3) # Print summary statistics

VaraibleCaloriesI <- group3$Calories # Extract Calories column
VariablefFatD <- group3$Fat # Extract Fat column
VaraibleCaloriesI # Print Calories

# Scatterplot or Scatter Diagram
plot(VaraibleCaloriesI, # X-variable (Calories)
     VariablefFatD, # Y-variable (Fat)
     main = "Scatterplot of Fat vs. Calories", # Main title
     ylab = "Fat (in gr)", # Y-axis label
     xlab = "Calories (in calories)", # X-axis label
     cex = 1, # Size of the dot
     pch = 20, # Dot style
     col = "darkgreen") # Dot color

# Covariance
cov_matrix = cov(group3) # Calculate covariance matrix
cov_round <- round(cov_matrix, digits = 1) # Round covariance values
cov_round # Print covariance matrix

# Correlation Coefficient
cor_coefficiente <- cor(group3) # Calculate correlation matrix
cor_coefficiente_round <- round(cor_coefficiente, digits = 4) # Round correlation values
cor_coefficiente_round # Print correlation matrix

> cor_coefficiente_round
         Calories    Fat
Calories   1.0000 0.6818
Fat        0.6818 1.0000


# 11.	Trying to accurately allocate labour hours in a moving job, the manager of a moving company would like to develop a method of predicting the labour hours (Labour; measured in hours) based on the size of the high-rise apartment (Size; measured in cubic feet). A random sample of 25 high-rise