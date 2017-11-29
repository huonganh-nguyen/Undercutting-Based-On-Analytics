#######################################################
### Undercutting ALLSTATE online Case
###
###
### Load additional file to install packages
source("installpackages.R")
###
###
### Load ALLSTATE DATA FILE
ALLcost <- read.csv("ALLSTATEcost.csv")

###
###
### Data Preparation ###########################
################################################
###
### Lets see a summary of it
###
summary(ALLcost)
### there are plenty of things to decide here.
### Which variables have NA's:
### risk_factor also has NA (that should be also a level)
### duration_previous it has 0 and NA's we probably need to treat them differently.
### C_previous
### location
### 
### Lets print the first 6 data points
ALLcost[1:6,]
### we see that the first customer requested 2 quotes
### if we are predicting the behavior of the customer, we should take that in consideration
### but first we will predict the cost quoted by ALLSTATE
### so we start by assuming it does not discriminate across used id and shopping_pt (an assumption)
drop <- c("customer_ID","shopping_pt","record_type","time","location")
### This creates a dataframe (DATA) from d without the columns in drops
DATA <- ALLcost[,!(names(ALLcost) %in% drop)]
head(DATA)
###
DATA$car_value <-  factor(DATA$car_value)
DATA$day <-  factor(DATA$day)
DATA$state <-  factor(DATA$state)
DATA$homeowner <- factor(DATA$homeowner)
DATA$married_couple <- factor(DATA$married_couple)
duration_NA <-  ifelse( is.na(DATA$duration_previous) , 1, 0 )        ### creating a dummy variable for NA
### number of NA in duration
sum(duration_NA)
### corresponds to 5% of the sample 783/15483
sum(duration_NA)/length(duration_NA)
### It is not that big and we could just drop them in a first analysis
### however we wil create a dummy variable
DATA$duration_previous[duration_NA>0] <-0 ### making NA to zero
### lets look at C_previous
C_NA <-  ifelse( is.na(DATA$C_previous), 1, 0 )        ### creating a dummy variable for NA
### how many?
sum(C_NA)
### very suspecious...
cor(C_NA,duration_NA)
### HAHA... the same observations that do not report previous duration...
### Lets treat C_previous as factor
DATA$C_previous[C_NA>0] <-0 ### making NA to zero
DATA$C_previous <-  factor(DATA$C_previous)                           
### Lets look at risk_factor as well...
risk_NA <- ifelse( is.na(DATA$risk_factor), 1, 0 )
sum(risk_NA)
### The NA for those are different observations...
DATA$risk_factor[risk_NA>0] <-0                     
### treat thatas a level "0" (a new category of risk...)
DATA$risk_factor <-  factor(DATA$risk_factor)                           
###
summary(DATA)
### there should be no NA's in the data at this point....

#################################
#### Question 1: Visualization
#################################
#### A suggestion is to go back to Class 1 and Class 2 Scripts
#### No additional hints for visualization at this point
#Are homeowners less risky? 
# Stacked Bar Plot with Colors and Legend
counts <- table(DATA$risk_factor, DATA$homeowner)
#Proportional Stacked Bar Plot 
prop = prop.table(counts, margin = 2)
barplot(prop, main="Homeowner and Risk",
        names.arg=c("Doesn't Own Home", "Owns Home"), col=c("blue","red", "yellow", "green", "orange"),
        legend = c("No Risk", "Low Risk", "Moderate Risk", "High Risk", "Most Risk"))

#Are married couples less risky?
counts <- table(DATA$risk_factor, DATA$married_couple)
#Proportional Stacked Bar Plot 
prop = prop.table(counts, margin = 2)
barplot(prop, main="Marriage  and Risk",
        names.arg=c("Not Married", "Married"), col=c("blue","red", "yellow", "green", "orange"),
        legend = c("No Risk", "Low Risk", "Moderate Risk", "High Risk", "Most Risk"))

#Are married couples homeowners?
counts <- table(DATA$homeowner, DATA$married_couple)
#Proportional Stacked Bar Plot 
prop = prop.table(counts, margin = 2)
barplot(prop, main="Marriage  and Homes",
        names.arg=c("Not Married", "Married"), col=c("blue","red"),
        legend = c("Doesn't Own Home", "Owns Home"))

#################################
#### Question 2: A first linear Regression Model. 
####             Feel free to use this or improve upon it.
#### this is a linear regression with all the variables in DATA
result <- glm(cost ~ ., data = DATA) 
### and to see the results (coefficients, p-values, etc...)
summary(result)
### the R-squared in this case is 
1 - (result$dev/result$null)

### As a side note, note that when running regressions, 
### sometimes R creates new columns automatically to run the regression
### for example, it creates dummies for you if you have columns that are
### factors. To get matrix with all these columns explicitly created 
### for the following regression
result <- glm(cost ~ ., data = DATA)
### simply use the command "model.matrix" as follows
M <- model.matrix(cost~., data = DATA)
summary(M)
### thus the same regression can be run as
resultM <- glm(DATA$cost ~ M)
### Just to make sure, lets see that R2 match...
1 - (resultM$dev/resultM$null)
###
### By having the design matrix you can easily drop or add variables
### based on any rule you like to use.
### For example, if you want to use only the first 5 columns of M
### you simple call with M[,1:5]
resultM5 <- glm(DATA$cost ~ M[,1:5])
summary(resultM5)
1 - (resultM5$dev/resultM5$null)
#### Another model one can consider is the one that 
#### would include interactions based 
#### on the the coverage options A through G
#### we can add those interactions in addition to the previous variables
result_interactions <- glm(cost ~ .+(A+B+C+D+E+F+G)^2, data = DATA)
summary(result_interactions)
1 - (result_interactions$dev/result_interactions$null)
#### this has all the variables plus all the interations 
####
####
#### Try model 1, adjusted R-squared is 0.6571
model1 <-lm(cost~.^2, data=DATA)
summary(model1)
#### Try model 2, adjusted R-squared is 0.4502
model2 <-lm(cost~., data=DATA)
summary(model2)
#### Try model 3, adjusted R-squared is 0.0005487
model3 <- lm(cost~ day+state+group_size+homeowner+car_age, data=DATA)
summary(model3)
#### Try model 4, adjusted R-squared is 0.1345
model4 <- lm(cost~risk_factor+car_age+car_value+group_size+age_youngest, data=DATA)
summary(model4)
#### Try model 5, adjusted R-squared is 0.4749
model5 <- lm(cost ~ .+(A+B+C+D+E+F+G)^2, data=DATA)
summary(model5)
#### Try model 6, adjusted R-squared is 0.1567
model6 <- lm(cost ~ A+B+C+D+E+F+G, data=DATA)
summary(model6)
#### We chose model 1, which has the highest adjusted R-squared value.

#################################
#### Questions 3 is conceptual questions about modeling framework.
#### No data analysis expected.
#################################

#################################
### Question 4 Provide quotes for new.customers
#################################

## The following command loads the "new.customers" to memory
## it is already formatted in our conventions 
## (where NA's in some variables were turned to level "0")
new.customers <- readRDS("NewCustomers.Rda")
head(new.customers)
summary(new.customers)
## Provide new quotes for customers in new.customers
predict.lm(model1, newdata = new.customers, interval='predict', level=0.95)

#################################
#### Question 5: No start script

