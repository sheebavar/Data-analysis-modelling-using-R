
# Data acqusition
admission_data <- read.csv("C:/Users/admin/Desktop/DS with R/Projects/college admission/College_admission.csv")
View(admission_data)

# structure of data
str(admission_data)
summary(admission_data)

# missing values
anyNA((admission_data)) # No missing values in the data set

# outlier analysis
par(mfrow=c(3,3))

bx_gre <- boxplot(admission_data$gre)
quantile(admission_data$gre,seq(0,1,0.02)) # 4% value is 380
summary(admission_data$gre)
bx_gre$stats
admission_data$gre<- ifelse(admission_data$gre<=340, median(admission_data$gre),admission_data$gre)

bx_gpa <- boxplot(admission_data$gpa,main="GPA OUTLIER PLOT")
quantile(admission_data$gpa, seq(0,1,0.02)) # 4% is 2.7000
summary(admission_data$gpa)
bx_gpa$stats
admission_data$gpa <- ifelse(admission_data$gpa <=2.420, median(admission_data$gpa),admission_data$gpa)

bx_ses <- boxplot(admission_data$ses,main="SES OUTLIER PLOT") # No outliers in ses
bx_gendermale <- boxplot(admission_data$Gender_Male, main="GENDER OUTLIER PLOT") # No outliersin gendermale
bx_race <- boxplot(admission_data$Race, main="RACE OUTLIER PLOT") # No outliers in RAce
bx_rank <- boxplot(admission_data$rank, main="Rank outlier plot") # No outliers in rank
bx_admit <- boxplot(admission_data$admit) # no outliers in admit

# transforming variables
# gre and gpa are numerical continous variables, while the rest are categorical variables
#converting categorical ariables into dummy variables
str(admission_data)
View(admission_data)
admission_data$admit<- as.factor(admission_data$admit) # response variable, must be factorised for classification

sort(table(admission_data$ses),decreasing=TRUE)
admission_data_new<- admission_data

admission_data_new$ses <- as.factor(admission_data_new$ses)
#admission_data_new$Gender_Male <- as.factor(admission_data_new$Gender_Male)
admission_data_new$Race <- as.factor(admission_data_new$Race)
admission_data_new$rank <- as.factor(admission_data_new$rank)

str(admission_data_new)
summary(admission_data_new)

#Data is normally distributed or not
par(mfrow=c(2,2))
plot(density(admission_data$gre), sub=paste("skewness :",skewness(admission_data$gre),"kurtosis", kurtosis(admission_data$gre)))
? plot()
plot(density(admission_data$gpa), sub=paste("skewness :",skewness(admission_data$gpa),"kurtosis", kurtosis(admission_data$gpa)))
library(moments)
skewness(admission_data$gpa)

plot(density(admission_data$ses))

# Dataset is almost normally distributed 

# identify significant variables
 # using correlation matrix, which shows the degree of association between numeric variables
install.packages("dummies")
library(dummies)
admission_data_dummy <- dummy.data.frame(admission_data_new)
str(admission_data_dummy)

dev.off()
library(corrplot)
cr <-cor(admission_data_dummy)
corrplot(cr, type='lower', order='AOE') # gpa, gre and rank1 are the significant variables

 #Using information gain for feature selection
install.packages("FSelector")
library(FSelector)
Information_gain(admission_data$admit~., data=admission_data)


#model building
library(caTools)
split <- sample.split(admission_data,SplitRatio = 0.7)
traindata <- subset(admission_data, split==TRUE)
testdata <- subset(admission_data, split==FALSE)

logmod1 <- glm(admission_data$admit~.,data=admission_data, family="binomial")
?glm
summary(logmod1)

step(logmod1)

logmod2 <- glm(admission_data$admit~ gre+gpa+rank, data=admission_data,family="binomial")
summary(logmod2)

library(caret)
logpred1 <- predict(logmod2, traindata, response="class")
head(logpred1)
prediction1 <- ifelse(logpred1>0.05,1,0)
table(traindata$admit,prediction1)
(146+14)/(146+14+12+56)  # 70.17% accuracy

confusionMatrix(traindata$admit,as.factor(prediction1))
confusionMatrix(traindata$admit,pred_factor, positive="1")

#ROC curve
install.packages("InformationValue")
library(InformationValue)
plotROC(traindata$admit,prediction1)
plotROC(traindata$admit, as.numeric(fitted(logmod2)))

logpred2 <- predict(logmod2,testdata)
head(logpred2)
prediction2 <- ifelse(logpred2>0.05,1,0)
head(prediction2)
table(testdata$admit,prediction2)
(109+14)/(109+14+6+43)  #71.51% accuracy for testdata in logistic regression

confusionMatrix(as.factor(testdata$admit),as.factor(prediction2))
confusionMatrix(as.factor(testset$vs),as.factor(prediction), positive="1")

#Decisiontree
library(rpart)
decisionmod1 <- rpart(admission_data$admit~.,data=admission_data, method="class")
summary(decisionmod1)
library(rpart.plot)
prp(decisionmod1)

decisionpredict<- predict(decisionmod1, testdata)
head(decisionpredict)
class(decisionpredict)
decisionpredict$score<- decisionpredict[,1] >decisionpredict[,2]
decisionpredict$score <- ifelse(decisionpredict[0] > decisionpredict[1],0,1)
table(testdata$admit,decisionpredict[,1])

# svm
library(e1071)
svm_mod <- svm(admission_data$admit~.,data=admission_data)
summary(svm_mod)        

svmpredict=predict(svm_mod,testdata)
head(svmpredict)
confusionMatrix(as.factor(testdata$admit),as.factor(svmpredict))
table(Actualvalues=testdata$admit, predictedvalues=svmpredict)
(113+12)/(113+13+45+2) # Accuaracy is 72.25% in SVM classifier

#randomforest
randomforest_mod=randomForest::randomForest(admission_data$admit~.,data=admission_data)
summary(randomforest_mod)

rnd_pred <- predict(randomforest_mod, testdata)
head(rnd_pred)
table(testdata$admit,rnd_pred)
(115+57)/(115+57+0+0) # 100% accuracy in random forest

# Random Forest classifier gives the best accuracy across other models
