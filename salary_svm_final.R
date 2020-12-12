## 1) Prepare a classification model using SVM for salary data 

#Data Description:
#age -- age of a person
#workclass	-- A work class is a grouping of work 
#education	-- Education of an individuals	
#maritalstatus -- Marital status of an individulas	
#occupation	 -- occupation of an individuals
#relationship -- 	
#race --  Race of an Individual
#sex --  Gender of an Individual
#capitalgain --  profit received from the sale of an investment	
#capitalloss	-- A decrease in the value of a capital asset
#hoursperweek -- number of hours work per week	
#native -- Native of an individual
#Salary -- salary of an individual

# Loading both training and testing data
salary_train<-read.csv("C:\\Users\\IN102385\\OneDrive - Super-Max Personal Care Pvt. Ltd\\Assignment - Data Science -UL\\SVM-R\\SalaryData_Train.csv")
salary_test<-read.csv("C:\\Users\\IN102385\\OneDrive - Super-Max Personal Care Pvt. Ltd\\Assignment - Data Science -UL\\SVM-R\\SalaryData_Test.csv")
str(salary_train)
# begin by training a simple linear SVM

# Loading the required packages
install.packages("kernlab")
library(kernlab)
install.packages("ggplot2")
library(ggplot2)
install.packages("caret")
library(caret)
library(e1071)
library(psych)
library(plyr)
# Converting the character variables into factors for analysis
salary_train$Salary <-as.factor(salary_train$Salary)
salary_test$Salary <-as.factor(salary_test$Salary)
salary_test$workclass<-as.factor(salary_test$workclass)
salary_test$education<-as.factor(salary_test$education)
salary_test$educationno<-as.factor(salary_test$educationno)
salary_test$maritalstatus<- as.factor(salary_test$maritalstatus)
salary_test$occupation<-as.factor(salary_test$occupation)
salary_test$relationship<-as.factor(salary_test$relationship)
salary_test$race<-as.factor(salary_test$race)
salary_test$sex<-as.factor(salary_test$sex)
salary_test$native <-as.factor(salary_test$native)

# Plot and ggplot 
ggplot(data=salary_train,aes(x=salary_train$Salary, y = salary_train$age, fill = salary_train$Salary)) +
  geom_boxplot() + ggtitle("Box Plot")
ggplot(data=salary_train,aes(x = salary_train$age, fill = salary_train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
plot(salary_train$Salary, salary_train$age)
# The salary of higher age people is higher than that of lower age
salary_train$workclass<-as.factor(salary_train$workclass)
plot(salary_train$workclass,salary_train$Salary)
ggplot(data=salary_train,aes(x = salary_train$workclass, fill = salary_train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
# Self employed and govt.people earn more than any other workclass people
# Many people are working in private industries although the ratio between
# low and high is high
salary_train$education<-as.factor(salary_train$education)
plot(salary_train$education,salary_train$Salary)
ggplot(data=salary_train,aes(x = salary_train$education, fill =salary_train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
# Bachelors,Professors, Masters and Doctorate people although less in number, but their salary is high
# HS-Gradepeople are high in numbers, but their salary range is low
salary_train$educationno<-as.factor(salary_train$educationno)
plot(salary_train$educationno,salary_train$Salary)
ggplot(data=salary_train,aes(x = salary_train$educationno, fill = salary_train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
# The higher degree in education earns more salary
salary_train$maritalstatus<-as.factor(salary_train$maritalstatus)
plot(salary_train$maritalstatus,salary_train$Salary)
ggplot(data=salary_train,aes(x = salary_train$maritalstatus, fill = salary_train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
# Married-civ-spouse salary is comparatively high in proportion
salary_train$occupation<-as.factor(salary_train$occupation)
plot(salary_train$occupation,salary_train$Salary)
ggplot(data=salary_train,aes(x = salary_train$occupation, fill = salary_train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
# Executive-Managerial category come under high salary category
salary_train$relationship<-as.factor(salary_train$relationship)
plot(salary_train$relationship,salary_train$Salary)
ggplot(data= salary_train,aes(x = salary_train$relationship, fill = salary_train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
# Husband by elation earns high salary
salary_train$race<-as.factor(salary_train$race)
plot(salary_train$race,salary_train$Salary)
ggplot(data=salary_train,aes(x = salary_train$race, fill = salary_train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
# White people in Us come under high category in earning high salary
salary_train$sex<-as.factor(salary_train$sex)
plot(salary_train$sex,salary_train$Salary)
ggplot(data=salary_train,aes(x = salary_train$sex, fill = salary_train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
# Male people comparatively get high salary
salary_train$native<-as.factor(salary_train$native)
plot(salary_train$native,salary_train$Salary)
ggplot(data= salary_train,aes(x = salary_train$native, fill = salary_train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
# United States people earn comparatively high salary
ggplot(data=salary_train,aes(x=salary_train$Salary, y = salary_train$capitalgain, fill = salary_train$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot") 
ggplot(data=salary_train,aes(x = salary_train$capitalgain, fill = salary_train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
# There is a high variation in capital gain among low salary category people
ggplot(data=salary_train,aes(x=salary_train$Salary, y = salary_train$capitalloss, fill = salary_train$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")
ggplot(data=salary_train,aes(x = salary_train$capitalloss, fill = salary_train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
# There is also high variation in capital loss as well more in low salary category
ggplot(data=salary_train,aes(x=salary_train$Salary, y = salary_train$hoursperweek, fill = salary_train$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")
ggplot(data=salary_train,aes(x = salary_train$hoursperweek, fill = salary_train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
##Training a model on the data ----
# begin by training a simple linear SVM
Salary_classifier <- ksvm(Salary ~ ., data = salary_train,
                          kernel = "vanilladot")
## Evaluating model performance ----
# predictions on testing dataset

Salary_predictions <-predict(Salary_classifier, salary_test)
head(Salary_predictions)
agreement <- Salary_predictions == salary_test$Salary
table(agreement)
prop.table(table(agreement))
# Accuracy of the model is 0.85

## Improving model performance ----
Salary_classifier_rbf <- ksvm(Salary ~ ., data = salary_train, kernel = "rbfdot")
Salary_predictions_rbf <- predict(Salary_classifier_rbf, salary_test)
head(Salary_predictions_rbf)
agreement_rbf <- Salary_predictions_rbf == salary_test$Salary
table(agreement_rbf)
prop.table(table(agreement_rbf))
# Accuracy of the model is 0.85

# CONCLUSION :
# Classification of Salary data has been made by using SVM model
# The model accuracy is 0.85 with bith linear and non-linear SVM models