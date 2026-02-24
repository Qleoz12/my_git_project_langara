fileAddress = "http://mylinux.langara.bc.ca/~sli/4820/datascience_pass_dataset.csv"
mydata = read.csv( fileAddress )

names( mydata )
#"study_hours_per_week" "coding_experience"    "pass"  

#--------------------------------------
#get the dummy variables
#--------------------------------------
table(mydata$coding_experience)
# Intermediate         None   Proficient 

coding.none = ifelse( mydata$coding_experience == "None", 1, 0)
coding.Proficient = ifelse( mydata$coding_experience == "Proficient", 1, 0)

hour = mydata$study_hours_per_week


#--------------------------------
#get the response variable, pass
#--------------------------------
pass = factor( mydata$pass )


#----------------------------------
# Fit the logistic regression model
#----------------------------------
fitted.model = glm(  pass ~ hour + coding.none + coding.Proficient, family=binomial)

summary( fitted.model)


#--------------------------------
#Fit the logistic regression model with intercept and hour
#-------------------------------------
fitted.model.only.hour = glm( pass ~ hour, family=binomial)

#-------------------------
#Run the likelihood Ratio test
#---------------------------
anova( 
	fitted.model.only.hour, 
	fitted.model, 
	test="Chisq"
	)
	
#----------------------
#Make prediction
#----------------------

#-------------------------------------
#students with no experience (none)
x0 = c(
	1,  #intercept 
	10, #hour = 10
	1,  #coding.none = 1
	0,  #coding.Proficient = 0
)
#get the regression coefficient
coef = coef(fitted.model)

#calculate the log-odds 
#      = sum( (1, 10, 1, 0) * (A, B.hour,  B.coding.none, B.coding.proficient) ) 
log.odds = sum(x0 * coef)

#odds = e^( log.odds )
odds = exp(log.odds)

#probability = odds / (1 + odds ) 
prob = odds / (1 + odds )

#-------------------------------------
#students with Intermediate experience 
#hours = 10
x0 = c(1, 10, 0, 0)
coef = coef(fitted.model)
log.odds = sum(x0 * coef)
odds = exp(log.odds)
prob = odds / (1 + odds )

#-------------------------------------
#students with proficient experience 
#hours = 10
x0 = c(1, 10, 0, 1)
coef = coef(fitted.model)
log.odds = sum(x0 * coef)
odds = exp(log.odds)
prob = odds / (1 + odds )




