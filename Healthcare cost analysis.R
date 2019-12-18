
getwd()
setwd ("C:\\Users\\admin\\Desktop\\DS with R\\Projects\\Healthcare cost analysis")
# Loading the data
library(readxl)
healthdata <- read_excel("1555054100_hospitalcosts.xlsx")
View(healthdata)
head(healthdata)

#EDA
str(healthdata)
summary(healthdata)
 # Total 500 obs and 6 variables, all numerical data
 # Missing values in the RACE column--To be addressed

# Missing value imputation
sum(is.na(healthdata$RACE)==TRUE) # only one missing value
table(healthdata$RACE) # Maximunm people belongs to the RACE ,1
healthdata$RACE[is.na(healthdata$RACE)==TRUE] <- 1
summary(healthdata)

#Univariate analysis (boxplot,density plot)
 boxplot(healthdata$AGE)  # No outliers
 
 bx <-boxplot(healthdata$LOS)   #Outliers are present
 quantile(healthdata$LOS, seq(0,1,0.02))  # 4%  is 1 and 96% is 7
 bx$stats
healthdata$LOS <- ifelse(healthdata$LOS >7, 7,healthdata$LOS)
healthdata$LOS <- ifelse(healthdata$LOS <1, 1,healthdata$LOS)

bx1<- boxplot(healthdata$TOTCHG)
bx1$stats
quantile(healthdata$TOTCHG, seq(0,1,0.02))  #  96% is 10588
healthdata$TOTCHG <- ifelse(healthdata$TOTCHG >=10587,10587,healthdata$TOTCHG)

bx2<- boxplot(healthdata$APRDRG)
bx2$stats
quantile(healthdata$APRDRG, seq(0,1,0.02))  #  4% is 96 and 96% is 758
healthdata$APRDRG <- ifelse(healthdata$APRDRG >=758,758,healthdata$APRDRG)
healthdata$APRDRG <- ifelse(healthdata$APRDRG <=96,96,healthdata$APRDRG)

plot(density(healthdata$AGE)) # right skewed, mean greater than median
plot(density(healthdata$LOS))
plot(density(healthdata$TOTCHG)) #right skewed
plot(density(healthdata$APRDRG)) #left skewed


#Bivariate analysis
scatter.smooth(healthdata$AGE, healthdata$TOTCHG) #No correlation

# Converting to factors
healthdata_copy <- healthdata
healthdata$AGE <-as.factor(healthdata$AGE)

summary(healthdata$AGE)
summary(healthdata)



# Question-1 :-age category of people who frequent the hospital and has the maximum expenditure
table(healthdata$AGE)
   # 307 patients, from age group 0-1, which is the maximum
hist(healthdata_copy$AGE)

library("dplyr")
healthdata %>% group_by(AGE) %>% summarise(total_expenditure=sum(TOTCHG)) %>% arrange(desc(total_expenditure))
   # Maximum expenditure is for 0-1 age group



# Question-2 diagnosis-related group that has maximum hospitalization and expenditure
healthdata %>% group_by(APRDRG) %>% summarise(total_stay=sum(LOS),total_exp=sum(TOTCHG)) %>%
  arrange(desc(total_stay,total_exp))
   # The diagnosis group, 640 has the maximum hospitalization and expenditure



# Question-3 needs to analyze if the race of the patient is related to the hospitalization costs
healthdata %>% group_by(RACE) %>% summarise(race_cost=sum(TOTCHG))
hist(healthdata$RACE)
race_cost <-cor(healthdata$RACE,healthdata$TOTCHG)
race_cost
   # Race-1 has the maximum patients and therefore maximum cost



# Question-4 analyze the severity of the hosp costs by age and gender for the proper allocation of resources.

hosp_female_age=healthdata %>% group_by(FEMALE,AGE) %>% summarise(tot_cost=sum(TOTCHG)) %>% arrange(desc(tot_cost))
    # age group, 0-1 & Male has the most hospital cost



#Question-5  if the length of stay can be predicted from age, gender, and race.
    # Ho - LOS is independent of age, gender and race
    # H1 - LOS is dependent on age, gender and race
dev.off()
par(mfrow=c(1,3))
scatter.smooth(healthdata$AGE, healthdata$LOS)
scatter.smooth(healthdata$FEMALE, healthdata$LOS)
scatter.smooth(healthdata$RACE,healthdata$LOS)


mod1=lm(healthdata$LOS ~ AGE +FEMALE +RACE, data=healthdata)
mod1                                                                                    
summary(mod1)

# Model can only explain 0.01 variance in regression coefficient for predicting LOS.
#Also p-Value of model is higher than 0.05

step(mod1)
cor(healthdata_copy) # Very minimal correlation between LOS and AGE , FEMALE & RACE




#Question-6 agency wants to find the variable that mainly affects hospital costs.

healthmod1 <- lm(TOTCHG ~ ., healthdata)
healthmod1
summary(healthmod1)
 step(healthmod1)

 healthmod2 <- lm(TOTCHG ~ AGE+FEMALE+LOS+APRDRG,healthdata) 
healthmod2 
summary(healthmod2)

# AGE, FEMALE, LOS, APRDRG are the variables which mainly affects hospitalcosts.

library(car)
vif(healthmod2)  # No mulitcollinearity , variance of regrssion coeff is not inflated due the correlation between
   # predictor variables

# analysing residual plot

par(mfrow=c(2,2))
plot(healthmod2)

#1 - Autocorrelation

durbinWatsonTest(healthmod2)

#2 - Normality of errors

hist(residuals(healthmod2))

#3 - Homoscedasticity

plot(healthdata$TOTCHG, residuals(healthmod2))

dev.off()

library(predictmeans)

cooksd=CookD(healthmod2)
# Non constant variance test
ncvTest(healthmod2) # pvalue is less, so Ho is rejected and variance is not constant

