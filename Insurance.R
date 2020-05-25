#The data gives the details of third party motor insurance claims in Sweden for the
#year 1977. In Sweden, all motor insurance companies apply identical risk
#arguments to classify customers, and thus their portfolios and their claims
#statistics can be combined. The data were compiled by a Swedish Committee on
#the Analysis of Risk Premium in Motor Insurance. The Committee was asked to
#look into the problem of analyzing the real influence on the claims of the risk
#arguments and to compare this structure with the actual tariff.

#**************************************************************************************
#                             Variable description                                    *
#**************************************************************************************

# The insurance dataset holds 7 variables and the description of these variables are
# given below:
# Variable Description
# Kilometers Kilometers travelled per year
# 1: < 1000
# 2: 1000-15000
# 3: 15000-20000
# 4: 20000-25000
# 5: > 25000
# Zone Geographical zone
# 1: Stockholm, Göteborg, and Malmö with surroundings
# 2: Other large cities with surroundings
# 3: Smaller cities with surroundings in southern Sweden
# 4: Rural areas in southern Sweden
# 5: Smaller cities with surroundings in northern Sweden
# 6: Rural areas in northern Sweden
# 7: Gotland
# Bonus No claims bonus; equal to the number of years, plus one, since the last claim
# Make 1-8 represents eight different common car models. All other models are combined in class 9.
# Insured :Number of insured in policy-years
# Claims  : Number of claims
# Payment : Total value of payments in Skr (Swedish Krona)





#*************************************************************************************************
# The committee is interested to know each field of the data collected through descriptive       *
# analysis to gain basic insights into the data set and to prepare for further analysis.         *
#                                                                                                *
#*************************************************************************************************


setwd(choose.dir())
motor_insurance <- read.csv('SwedishMotorInsurance.csv')

head(motor_insurance,5) # View a the first 5 rows of the data

str(motor_insurance)
#The structure shows that Kilometres,Zone,Bonus and Make are numerical whereas they should 
# be factors

colSums(is.na(motor_insurance))# Chekcing for missing values in the data
# We see that there are no missign values

#To get a detailed summary of the data we call the summary function 
summary(motor_insurance)

######################################################################################################



#***********************************************************************************
# The total value of payment by an insurance company is an important factor        *
#  to be monitored. So the committee has decided to find whether this payment is   * 
#  related to number of claims and the number of insured policy years. They also   *
#  want to visualize the results for better understanding.                         *
#***********************************************************************************

# We can use the correlation matric and the correlation plot to investigate 
# the relationship between payment and number of claims, and the relationship
# between payment and number of insured policies 

res <- cor(motor_insurance)
round(res,2)

# The correlation matric shows that the is high positive correlation between payment 
# and number of claims also there is high positive correlation betwen payment and 
# number of insured policies

# There is 93% positive correlation between payment and number of insured policies
# There is 100% positive correlation between payment and number of claims                  

library(corrplot)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#Positive correlations are displayed in blue and negative correlatins are 
#displayed in red

# the correlation plot also confirms that there is high postitive correlation 
# between payments and insured policies
# also between payments and claims 
################################################################################################   


#**************************************************************************************************************
#  The committee wants to figure out the reasons for insurance payment increase                               *
#   and decrease. So they have decided to find whether distance, location, bonus,                             *
#    make, and insured amount or claims are affecting the payment or all or some of                           *
#     these are affecting it.                                                                                 *
#**************************************************************************************************************

# We can apply a linear regression model with payment being the dependent variable and 
#  the rest of the variables being independent/ predictor variables

str(motor_insurance)
# We see that the viriables Kilometres, Zone,Make and Bonus are numerical instead of
# categorical variables so we first have to convert them into factors

var <- c('Kilometres','Zone','Bonus','Make')
for (i in var ){
  motor_insurance[,i]= as.factor(motor_insurance[,i])}

str(motor_insurance)
#The output shows that the variables has been converted to factors
# We see that the viriable kilometres has 5 levels 
#  ZOne and Bonus have 7 levels and Make has 9 levels

linear_model <- lm(formula = Payment~.,data = motor_insurance)
linear_model
summary(linear_model)

#Result : The summary of the model shows that the variable Killometres is very significant to the model
#         at 0.1% since Kilometres2,Kilometres3,Kilometres4 and Kilometres5  are all significant at 0.1%
#
#       : Zone2,Zone 3,Zone 5 and Zone 7 are not significant to the model whereas Zone4  and Zone6
#       : are significant to the model at 0.1% significance level 
#       
#       : The variable Bonus1,Bonus2,Bonus3,Bonus4,Bonus5 and bonus 6 is are not significant to the model
#       : whereas Bonus7 is significant to the model at 1% significance level

#       : Make2 and Make3  are significant to the model at 5% significance level, Make4 is significant to the model
#       : at 0.1% significance level, Make5,Make6 and Make7 are significant to the model at 1% significance level
#       : Make8 and Make9 are not significant to the model.

#       : Insured and Claims are also significant to the model at 0.1% significance level.
#       
#       : The adjusted R-squared = 0.9953 which tells us that our model is about 99.5% accurate
#       : Which is suspiciously high in my opinion.
############################################################################################################



#***********************************************************************************************************
#  The insurance company is planning to establish a new branch office,                                     *
#    so they are interested to find at what location, kilometer, and bonus                                 *
#     level their insured amount, claims, and payment get increased.                                       *
#                                                                                                          * 
#***********************************************************************************************************

# We can use simple linear regression.


#By Location

group_zone<-apply(motor_insurance[,c(5,6,7)], 2, function(x) tapply(x, motor_insurance$Zone, mean))
group_zone
#Zone 4 has the highest number of Insured years,Claims and payment
#Zone 1-4 have more Insured years,High Claims and Payment compared to Zone 5, Zone 6 and Zone 7 

#By Kilometre

group_kilometres<-apply(motor_insurance[,c(5,6,7)], 2, function(x) tapply(x, motor_insurance$Kilometres, mean))
group_kilometres
# group2 has the highest Payments and highest number of Payments Claims although group1 has the 
#  highest number of Insured years

#By Bonus

group_bonus<-apply(motor_insurance[,c(5,6,7)], 2, function(x) tapply(x, motor_insurance$Bonus, mean))
group_bonus
#Bonus7 has high Number of Insured years,high number of claims and also high Payment.
# The rest of the bonus levels don't have much variations between them.

#############################################################################################################


#**********************************************************************************************************
#  The committee wants to understand what affects their claim rates so as                                 *
#   to decide the right premiums for a certain set of situations. Hence, they                             *
#   need to find whether the insured amount, zone, kilometer, bonus, or make                              *
#   affects the claim rates and to what extent.                                                           *
#**********************************************************************************************************


# We can use the linear regression model again except this time we make claims the dependent variable
# and making Insured,Zone,Kilometre,Bonus and Make the independent/predictor variables


linear_model_claims <- lm(formula = Claims~ Make+Insured+Kilometres+Zone+Bonus,data = motor_insurance)
linear_model_claims
summary(linear_model_claims)

#The sumamry of the model
#Zone3,Zone4,Zone5,Zone6,Zone7,Bonus2,Bonus3,Bonus4,Bonus5,Bonus6,Bonus7,Make9 and Insured are significant
# to the model at 0.1%  

#Make3,Make4,Make8,kilometre4 and Kilometre5 are significant tot the model at 1% significant model

#Zone2,Make5, Make6 and Make7 are significant to the model at 5% singnificance model.

# Thus varibale that is not significant to the model and does not affect number of claims is Kilometres3
# The Adjustred R-squared = 0.8421, which means out model is 84.21% accurate.

