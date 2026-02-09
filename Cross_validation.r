###########
# Cross Validation
###########

# Load packages
library(caret)
library(ggplot2)
library(mgcv)
library(data.table)

## caret--> 
## healthy status--> making it with glm and binomial regression--> 

## looking at observations and predicted values--> measure accuracy of the model--> we want to train the model on a subset of the data
# Simulate Data #
# we have seen this data before when talking about GLM (binomial regression)
# we have 2 explanatory variables: 1) breed of cows (Angus and Hereford) and 2) the proportion of grass fed in their diet
# our response variable is whether the cows are healthy or not

# 80
set.seed(4)
grass <- runif(100, min=10, max=80)
cowdiet<-data.frame(grass)
cowdiet$breed <- sample(c("Angus","Hereford"), size = 100, replace = TRUE)
cowdiet$isAngus<-as.numeric(cowdiet$breed=="Angus")
xb <- -7 + 0.15*cowdiet$grass+3*cowdiet$isAngus
p <- 1/(1 + exp(-xb))
cowdiet$healthy <- rbinom(n = 100, size = 1, prob = p)
cowdiet<-subset(cowdiet, select = -isAngus) # remove isAngus column as this is purely for simulation purposes

# create the logistic regression model
cow.model <- glm(healthy ~ grass + breed, cowdiet, family = "binomial")
summary(cow.model)

# remind ourselves what the data look like
predicted<-cowdiet
predicted$healthy<-predict(cow.model, type="response") # get predicted values to draw lines on plot

ggplot(cowdiet, aes(x=grass, y=healthy, col=breed))+
  geom_jitter(height = 0.01, width=0)+
  geom_line(data=predicted)+
  scale_y_continuous(breaks=c(0,1))+
  theme_classic()

#### Part 1: Validation Set Approach ####

# Split the data into 2 halves
set.seed(11)
split <-createDataPartition(cowdiet$healthy, p=0.2, list=FALSE) # list = F ensures "split" is saved as a matrix
train.data  <- cowdiet[split, ] # use matrix indexing to create training data
test.data <- cowdiet[-split, ]  # keep everything in cowdiet that is not in split as test data
head(train.data)

# make a plot to show which points went into test and training data sets
ggplot(train.data, aes(x=grass, y=healthy))+
  geom_jitter(height=0.05, width=0.05)+ # add jitter to black training points to make things easier to see
  geom_jitter(data=test.data, color="orange", height=0.05, width=0.05)+# color test data points in orange
  scale_y_continuous(breaks=c(0,1))+
  theme_classic()+
  ggtitle("Orange=test, black=train")

## return the result of the pred value is a probability

## true value and predicted value--> confusion matrix== another functionn for accuracy measurements

## predictions are zero
## false negatives and false positives, 

## counts of consistency between predicted and observed values

## kappa values shows the 

# create model using the training data
cow.train <- glm(healthy ~ grass + breed, data=train.data, family = "binomial")

# make predictions using test data
test.data$healthy<-factor(test.data$healthy) # convert actual health status of test data into factor
pred.prob <- predict(cow.train, newdata=test.data, type="response") # predict probability of being healthy for test data points

# use probabilities to classify predicted healthy status as either 0 or 1. Convert this column to a factor
test.data$pred_healthy<-as.factor(ifelse(pred.prob>0.5, "1", "0")) # if prob is >0.5, classify as 1, otherwise 0

# Create confusion matrix to display results
confusionMatrix(test.data$healthy, test.data$pred_healthy)

# Confusion Matrix: A table used in classification to understand the performance of an algorithm, with one axis representing the predicted classes and the other the actual classes. In this case:
#
# True Negative (TN):  (Prediction: 0, Reference: 0)
# False Positive (FP):  (Prediction: 0, Reference: 1)
# False Negative (FN):  (Prediction: 1, Reference: 0)
# True Positive (TP):  (Prediction: 1, Reference: 1)
# Accuracy: The proportion of correctly predicted observations to the total observations.
#
# 95% CI: A 95% confidence interval for the accuracy. It gives a range where we are 95% confident that the true accuracy lies.
#
# No Information Rate (NIR): The accuracy that could be achieved by predicting the most frequent class. (0.68 or 68% in this case)
#
# P-Value [Acc > NIR]: Tests the null hypothesis that the classifier is no better than random. A small p-value (< 0.05) indicates that the classifier is significantly better than random.
#
# Kappa: A statistic that measures inter-rater reliability (and also accuracy for classification). It considers random agreement. A value of 1 indicates perfect agreement, while a value of 0 indicates no agreement beyond chance.
#
# Mcnemar's Test P-Value: A statistical test used on paired nominal data. It is applied to 2x2 contingency tables with a dichotomous trait, to determine if the row and column marginal frequencies are equal (i.e., whether there is “marginal homogeneity”). It’s often used to compare the performance of two classifiers.
#
# Sensitivity (or Recall): The proportion of actual positives that were correctly identified.
#
# Specificity: The proportion of actual negatives that were correctly identified.
#
# Pos Pred Value (or Precision): The proportion of positive identification that were actually correct.
#
# Neg Pred Value: The proportion of negative identification that were actually correct.
#
# Prevalence: The proportion of the actual positive cases in the dataset.
#
# Detection Rate: The proportion of the actual positive cases that were correctly identified
#
# Detection Prevalence: The proportion of positive identifications made by the classifier.
#
# Balanced Accuracy: The average of sensitivity and specificity. It is used when the classes are imbalanced.
#
# 'Positive' Class: The class that is treated as the 'positive' class in this binary classification.




# Re-run everything from line (where we split data into training and test set) to here again using set.seed(11)
# Repeat this process with set.seed(1).
# Notice how the accuracy fluctuates depending on how the data was parsed into training and test sets
# This single validation set approach is not great unless you have a v. large sample size
# Cross validation approaches are usually a better alternative

#### Part 2: Cross Validation with Caret Package ####

# Leave One Out Cross Validation

# set the CV method we want to use
loocv<- trainControl(method = "LOOCV")
# use train() to perform the CV process
set.seed(4)
# because we are using a logistic regression with a binomial outcome, we first need to set healthy as a factor to keep caret package happy
cowdiet$healthy<-factor(cowdiet$healthy)
cow.loocv <- train(healthy ~ grass+breed, data = cowdiet, method = "glm", family="binomial",
                      trControl = loocv)
# inside train() include the model you want cross validated
# caret will work with many differents kind of models
# the argument trControl references the CV method we set in the previous step

# look at the results
cow.loocv

# K-fold Cross Validation

# set the CV method we want to use
kfoldcv <- trainControl(method = "cv", number = 5) # this is for 5-fold CV

# use train() to perform the CV process
set.seed(19)
cow.5fold <- train(healthy ~ grass+breed, data = cowdiet, method = "glm", family="binomial",
                   trControl = kfoldcv)
# look at the results
cow.5fold
cow.5fold$results # gives SD for the accuracy and Kappa
cow.5fold$resample # see how accuracy varies between folds
cow.5fold
# Repeated K-fold Cross Validation

# same idea as previous two examples but set CV method as follows:
repkfoldcv <- trainControl(method = "repeatedcv", number = 5, repeats=3)
# this example uses 5-fold CV repated 3 times
cow.repkfold <- train(healthy ~ grass+breed, data = cowdiet, method = "glm", family="binomial",
                   trControl = repkfoldcv)


cow.repkfold$results
cow.repkfold$resample

## repeat the cross validation many different times to get a global observation--> 



#### Part 3: Cross Validation for Model Comparison ####


#read in the drought dataset we generated from week 3
drought<-read.csv("Week 6/drought.csv")
drought$species<-as.factor(drought$species)
str(drought)

# we made a gam model and an lm model
gam_model<-gam(biomass~species+s(soil, by=species), data=drought, method="REML")
summary(gam_model)

lm_poly_model<-lm(biomass~species*poly(soil,3), drought)
summary(lm_poly_model)

# get predicted values for each model
drought$lm_poly_pred<-predict(lm_poly_model, drought)
drought$gam_pred<-predict(gam_model, drought)

ggplot(drought, aes(x=soil, y=biomass, col=species))+
  geom_point(alpha=0.5)+
  geom_line(aes(y=lm_poly_pred))+
  geom_line(aes(y=gam_pred), linetype="dashed")+
  theme_classic()+
  theme(legend.position = c(0.2,0.7))


ggplot(drought, aes(x=gam_pred, y=biomass, col=species))+
  geom_point(alpha=0.5)+
  theme_classic()+
  theme(legend.position = c(0.2,0.7))
## 5 fold k validation--> respecify the commad for what type of training control we want to do, taking 20 % of the data as a training set

## we can do multiple/repeat the process multiple times--> 5 fold cross val 3 different times, shiffling 5th, shuffle fith , shuffle fith...
##  

# custom function for K-fold CV with different models
cv_test<-function(data, y, model, k){
  data_split<-split(data, sample(rep(1:k, each=nrow(data)/k), replace=F))
  results<-rbindlist(lapply(1:k, function(i){
    test<-rbindlist(data_split[i])
    train<-rbindlist(data_split[-i])
    mod<-eval(parse(text=model))
    predicted<-predict(mod, test)
    observed<-test[[y]]
    R2=cor(predicted, observed)^2
    RMSE=sqrt(mean((predicted-observed)^2))
    MAE=mean(abs(predicted-observed))
    return(data.frame(i, R2, RMSE, MAE))
  }))
}

model<-lm(biomass~soil, drought)
summary(model)

cor(drought$soil, drought$biomass)^2
# try different models: k=10 and we will do 5 reps
lmcv1<-rbindlist(lapply(1:5, function(x){
  cv_test(data=drought, y="biomass", model="lm(biomass~species*soil, data=train)", k=10)
}))
lmcv1$model<-"lm"

lmcv<-rbindlist(lapply(1:5, function(x){
  cv_test(data=drought, y="biomass",model="lm(biomass~species*poly(soil,3), data=train)", k=10)
}))
lmcv$model<-"lm_poly"

gamcv<-rbindlist(lapply(1:5, function(x){
  cv_test(data=drought, y="biomass",model="gam(biomass~species+s(soil, by=species), data=train, method='REML')", k=10)
}))
gamcv$model<-"gam"

# combine results into a single object
all<-rbind(gamcv, lmcv, lmcv1)

# make a plot
pdf("~/Desktop/CV_drought.pdf", width=1, height=2)

ggplot(all, aes(x=model, y=RMSE, col=model))+
  geom_boxplot(outlier.size = .5)+
  theme_classic(base_size = 6)+
  theme(legend.position = "none")

ggplot(all, aes(x=model, y=MAE, col=model))+
  geom_boxplot(outlier.size = .5)+
  theme_classic(base_size = 6)+
  theme(legend.position = "none")

ggplot(all, aes(x=model, y=R2, col=model))+
  geom_boxplot(outlier.size = .5)+
  theme_classic(base_size = 6)+
  theme(legend.position = "none")

dev.off()

# is the RMSE, R2, or MAE "significantly" better with the GAM?
# Note: it is somewhat un-useful to use statistical tests of signficance in this context because in practice we can inflate the sample sizes by just running more repetitions...but this is useful for estimating effect size (how much better is the gam vs polynomial for example in terms of a measure of error?)
model<-lm(RMSE~model, all)
summary(model)
TukeyHSD(aov(model))

model<-lm(MAE~model, all)
TukeyHSD(aov(model))

model<-lm(R2~model, all)
TukeyHSD(aov(model))

## build a gam model where we predict... things of a differnt species...


## ROC does what in statistics_


