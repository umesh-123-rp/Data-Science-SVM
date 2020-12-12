#classify the Size_Categorie using SVM

#month	month of the year: 'jan' to 'dec'
#day	day of the week: 'mon' to 'sun'
#FFMC	FFMC index from the FWI system: 18.7 to 96.20
#DMC	DMC index from the FWI system: 1.1 to 291.3
#DC	DC index from the FWI system: 7.9 to 860.6
#ISI	ISI index from the FWI system: 0.0 to 56.10
#temp	temperature in Celsius degrees: 2.2 to 33.30
#RH	relative humidity in %: 15.0 to 100
#wind	wind speed in km/h: 0.40 to 9.40
#rain	outside rain in mm/m2 : 0.0 to 6.4
#Size_Categorie 	the burned area of the forest ( Small , Large)

#Normalising data and divide into training and test data

# Loading the required packages
library(kernlab)
library(caret)
library(plyr)
library(ggplot2)
library(psych)
library(ggvis)
library(corrplot)
# Loading the Forest Fire Dataset
Forestfires<-read.csv("C:\\Users\\IN102385\\OneDrive - Super-Max Personal Care Pvt. Ltd\\Assignment - Data Science -UL\\SVM-R\\forestfires.csv")
View(Forestfires)
str(Forestfires)
FF<-Forestfires
FF %>% ggvis(~FFMC, ~DMC,fill = ~FF$size_category) %>% layer_points()
FF %>% ggvis(~FFMC, ~DC,fill = ~ FF$size_category) %>% layer_points()
FF %>% ggvis(~temp, ~RH,fill = ~FF$size_category) %>% layer_points()
FF %>% ggvis(~RH, ~rain,fill = ~FF$size_category) %>% layer_points()
# Understanding the pattern of area affected in forest fire
hist(FF$area)
# There are lot of zeros. Therefore, we need to check the data by log transformation
FF1<-mutate(Forestfires,y=log(area+1))
hist(FF1$y)

# Convert the character variables like month, day and size_category into factors
FF$month<-as.factor(FF$month)
FF$day<-as.factor(FF$day)
FF$size_category<-as.factor(FF$size_category)
hist(FF$FFMC)
hist(FF$DMC)
hist(FF$DC)
hist(FF$ISI)
hist(FF$temp)
hist(FF$RH)
# Understanding the data of individual variables
str(FF)
summary(FF)
describe(FF)
# Individual variables have wide difference in their data range.
# Therefore, we can normalise the data
normalise <- function(x) {
  return((x - min(x)) / (max(x) - min(x))) 
}
# Applying normalisation to all the important variables
FF$temp <- normalise(FF$temp)
FF$rain <- normalise(FF$rain)
FF$RH <- normalise(FF$RH)
FF$wind <- normalise(FF$wind)
summary(FF)
describe(FF)
corrplot(cor(FF))

# Splitting data set into Training and Testing
attach(FF)
set.seed(123)
ind <- sample(2, nrow(FF), replace = TRUE, prob = c(0.7,0.3))
FF_train <- FF[ind==1,]
FF_test  <- FF[ind==2,]

#Training a model on the data ----
# begin by training a simple linear SVM
model1<- ksvm(size_category ~ temp + rain + wind + RH, 
             data= FF_train, kernel = "vanilladot")
model1
# Training accuracy obtained from the model is 0.75

## Evaluating model performance ----
# predictions on testing dataset
Area_pred <- predict(model1, FF_test)
mean(Area_pred==FF_test$size_category)
head(Area_pred)
agreement <- Area_pred == FF_test$size_category
table(agreement)
prop.table(table(agreement))
# Testing accuracy is 0.68

## Further Improving model performance ----
model_rfdot<-ksvm(size_category~ temp + rain+ wind + RH, 
                  data= FF_train,kernel = "rbfdot")
model_rfdot
# Training accuracy has improved to 0.77 by using non-linear method-rbfdot
pred_rfdot<-predict(model_rfdot,newdata=FF_test)
mean(pred_rfdot==FF_test$size_category)
agreement_rbf <- pred_rfdot == FF_test$size_category
table(agreement_rbf)
prop.table(table(agreement_rbf))
# Testing accuracy is obseved to be 0.68

## Further Improving model performance ----
#   By using the non-linear model "besseldot"
model_besseldot<-ksvm(size_category~temp+rain+wind+RH, 
                      data= FF_train,kernel = "besseldot")
model_besseldot
# Training accuracy is observed as 0.75
pred_bessel<-predict(model_besseldot,newdata=FF_test)
mean(pred_bessel==FF_test$size_category)
agreement_bessel <- pred_bessel == FF_test$size_category
table(agreement_bessel)
prop.table(table(agreement_bessel))
# Testing accuracy remains at 0.68

## Further improving model performance by using plydot:
model_poly<-ksvm(size_category~temp+rain+wind+RH, 
                 data= FF_train,kernel = "polydot")
model_poly
# Training accuracy observed to be 0.75

pred_poly<-predict(model_poly,newdata = FF_test)
mean(pred_poly==FF_test$size_category)
agreement_poly <- pred_poly == FF_test$size_category
table(agreement_poly)
prop.table(table(agreement_poly))
# Testing accuracy is observed as 0.68

# CONCLUSION : 
# The model accuracy is observed to be 0.68 
  

