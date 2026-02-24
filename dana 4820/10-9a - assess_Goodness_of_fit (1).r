# Assume the data file (Hours-and-Grades.csv) is saved on your computer.
# Use the file.choose() function to locate and select the file
fileLocation = file.choose()

# Read the data from the CSV file using read.csv()
mydata = read.csv( fileLocation )

# Use the names() function to view the column names in the dataset
names( mydata )
# output: [1] "Hours" "Grade"


#-----------------------------------------------------
# Assess the Goodness of Fit for Logistic Regression Model
# - Plot the Proportion of success (Exam Grade) 
#   against the midtpoint of the interval of predictor variable (e.g Hours) 
# - Hosmer-Lemeshow test
#-----------------------------------------------------

# I implement the goodness_of_fit(...) function to perform the tasks above.
# This function takes the following four inputs:
#
# 1. ydata                - A categorical vector with exactly two levels (e.g., "Pass" / "Fail").
#                            This represents the outcome variable for which we are assessing the goodness of fit.
#
# 2. success.label        - The specific value within ydata that represents a success (e.g., "Pass")
#                          
#
# 3. xdata                - A numeric vector representing the predictor variable (e.g., hours studied).
#                         
# 4. xinterval.endpoints  - A numeric vector defining the interval boundaries
#                           (e.g.  0,3,6,9,12)



goodness.of.fit = function( ydata, success.label, xdata, xinterval.endpoints )
{
	#set up the intervals and get the midpoint of the interval
	my.xinterval = cut( xdata, breaks = xinterval.endpoints, right=FALSE)
	my.len = length(xinterval.endpoints)  #number of intervals
	my.midpt = ( xinterval.endpoints[1: (my.len-1) ] + xinterval.endpoints[2:my.len] )/2


	#Calculate the proportion of succss within each interval
	my.prop = prop.table( table(ydata, my.xinterval), margin=2)
	my.success.prop = my.prop[success.label, ]
	
	#fit the logistic model to the data and extract the estimated coefficients 
	my.fitted.model = glm( factor(ydata) ~ xdata, family = binomial )
	my.intercept <- my.fitted.model$coef[1]
	my.slope <- my.fitted.model$coef[2]
	
	#define the logistic function
	logistic_func <- function(xx) {
		lin = my.intercept + my.slope * xx
		exp(lin) / (1 + exp(lin))
	}
	
	#Calculate the estimated probability of success and average probability within each interval
	my.est.p = logistic_func( xdata )
	my.avg.est.p = tapply( my.est.p, my.xinterval, mean)
	
	
	#--- Perform "Hosmer" Goodness-of-Fit test ----
	
	#Get the sample size within each interval and Chi-Square statistic / p-value
	my.n = tapply( my.est.p, my.xinterval, length) 
	chisq.stat = sum( (my.success.prop - my.avg.est.p)^2 * my.n / ( my.avg.est.p * (1 - my.avg.est.p) ) )
	pvalue = 1 - pchisq( chisq.stat, df = my.len   - 2)
	
	#Display the results
	print( '-----  Hosmer-Lemeshow test -----')
	print( paste('Chisquare Statistic', round(chisq.stat, 5), sep=" = ") ) 
	print( paste('Degree of freedom', (my.len - 2), sep=" = ") )
	print( paste('p-value', round(pvalue,5), sep=" = ") )
	
	#plot the proportion of success against mid-point of the interval
	plot(
		my.midpt,        		  # mid-point of the interval
		my.success.prop ,         # success proportion within interval
		xlab = 'Predictor(x)',  		#label x-axis 
		ylab = 'Proportion of Success', #label y-axis
		lwd = 3,                  # Set the line width to 3
		pch = 20,                 # Use solid circles to represent the points
		cex = 1.2                 # Set the size of the points (1.2x the original size)
	)
	
	# Add the logistic regression curve to the plot
	curve(
		logistic_func(x),         # Use the logistic function to generate values
		lwd = 3,                  # Set the line width of the curve
		col = "red",              # Set the color of the curve to red
		add = TRUE                # Add the curve to the existing plot
	)
}



#Run the Goodness-of-Fit procedure
goodness.of.fit( 
	ydata = mydata$Grade, 
	success.label = "P", 
	xdata = mydata$Hours, 
	xinterval.endpoints = c(0,3,6,9,12) 
)


#-----------------------------------------------------
# Assess the Goodness of Fit for Logistic Regression Model
# - using ROC and AUC
#-----------------------------------------------------
# Install and load pROC package (if not already installed)
#install.packages("pROC") # Only run if pROC is not installed
library(pROC)


# I implemented a function, myROC.AUC, to create the ROC curve and calculate the AUC.
# This function takes the following four inputs:
# 1. ydata                - A categorical vector with exactly two levels (e.g., "Pass" / "Fail").
#                            This represents the outcome variable for which we are assessing the goodness of fit.
#
# 2. xdata                - A numeric vector representing the predictor variable (e.g., hours studied).
#  
myROC = function( ydata, xdata)
{
	#Fit the logistic regression model to the data
	fitted.model <- glm(factor(ydata) ~ xdata, family = binomial)
	
	#Estimate the probability of success
	pred_probs <- predict(fitted.model, type = "response")
	
	#Get the ROC curve using the binary response outcome (y) and its estimated probability
	roc_curve <- roc(ydata, pred_probs)
	names(roc_curve)
	
	#Plot the ROC curve and display AUC on the plot
	FPR = 1 - roc_curve$specificities
	TPR = roc_curve$sensitivities
	plot(FPR, TPR, main = "ROC Curve", col = "blue", type = "l", lwd = 2, xlab="False Positive Rate", ylab="True Positive Rate")
	text(0.8, 0.2, paste("AUC = ", round(roc_curve$auc,4)),cex=1.3)
	curve( 0 + 1 * x, add=TRUE, lwd=0.5, col="grey")
	
	return( roc_curve)
}

#Run the ROC.AUC procedure
myROC(mydata$Grade, mydata$Hours)

