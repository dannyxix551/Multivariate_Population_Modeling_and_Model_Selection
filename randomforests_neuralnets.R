

# Libraries ---------------------------------------------------------------
install.packages("neuralnet")
library(ggplot2)
library(caret)
# install.packages("neuralnet")
# install.packages("randomForest")
library(randomForest)
library(neuralnet)

## decisio tree --> desicion trees are binary in a radom forest model --> nodes_-> 

## esision trees try to fit data--> fitting data to find the best model--> it can pick up on non-linear patterns --> each desicion tree is a subset of the data--> each subset produces desicion tree with something called bootstrapping--> if we ran expireince thru d tree we wan tp see what is the most common desicion
## bagging -->, out of bag error--> when boottrap you randomly pick observations --> they become prosit for testing 


## gini score-- > when its missig it has bad accuracy--> non defining 
## bootstraping is commonly used in stats--. the idea that we can rando select observations from our data--> meaning we tando sample data here--> each pne becomes random--> 

## changing at mutiple thresholds--> interactions and non-linearities are mapped very well by random forests--> dpn't require any model selection process\#\

## increase number of desiion trees inmproves errorrates--> is model converging on number of trees that is producing stable error term


## using it to predict chlorophyll content--> with remote sensing --> even if its stochastic --> can they compare and YES the predictive accuracy is very good atually!--> 

## importance scores--> some sort of attempt at predicting wheate yeilds--> 

## is this data matched or scaled? is it even needed to be scaled? Well yes it needs to be scaled-- 


## you have to select for what you are looking for of course--> whcich of these helps predict this, this and this--> could i use a random forest--> for dieses prediction--? 
## primarily for prediction--> not for linear--> it deals for compexty quiet well---> some interpreterbility
## remember it is bias in multiclass problems--> build a subset that has equal subestet for both-

## neural network _--> general premise is that it is construced from series of nodes(that are tech functions)--> each node repr diff type of function-->
## listen to all input predictors--> buildthe most predictvive neural net--> can listen to each other with diff weights
# data --------------------------------------------------------------------

###### Data simulation: we are simulating a made-up example where there is some relationship between cattle health and diet
# dont worry about the code for simulating the data unless your interested
set.seed(1)
grass <- runif(100,min=18, max=100)
data<-data.frame(grass)
xb <- -9 + 0.2*data$grass
p <- 1/(1 + exp(-xb))
data$breed <- sample(c("Angus","Hereford"), size = 100, replace = TRUE)
data$isAngus<-as.numeric(data$breed=="Angus")
xb <- -10 + 0.2*data$grass+6*data$isAngus
p <- 1/(1 + exp(-xb))
data$healthy <- rbinom(n = 100, size = 1, prob = p)
data$healthy[which(data$grass>70)]<-rbinom(n=sum(data$grass>70), size=1, prob=c(0.5))
data$healthy[which(data$grass>80)]<-rbinom(n=sum(data$grass>80), size=1, prob=c(0.3))
data$healthy[which(data$grass>90)]<-rbinom(n=sum(data$grass>90), size=1, prob=c(0.1))


# now we have a dataset called data
# made of 3 variables, breed (2 breeds of cattle), grass (% of diet from grass) , and healthy (0,1 meaning unhealthy and healthy, respectively)
data


# Let's plot the data
# it is clearly very non-linear. There are also possibly some differences between breeds
ggplot(data, aes(x=grass, y=healthy))+
  geom_point()+
  facet_grid(~breed)


# Random forest -----------------------------------------------------------

# lets model the relationship between health and grass + breed with a random forest
# we are using randomForest package

#we are setting ntree to 100, this means our forest contains 100 random decision trees
set.seed(1)
model<-randomForest(factor(healthy)~grass+breed, data=data, ntree=1000, do.trace=T)

# we can look at the model outout which tells us some details about how well our model performed
# most notable is the Out of Bag Error rate which is essentially a % of erroneous predictions in the model
model
plot(model)

# we can do some R shenanigans to plot the predicted values:
predicted<-data.frame(data)
predicted$healthy<-as.numeric(predict(model))-1

ggplot(predicted, aes(x=grass, y=healthy))+
  geom_point(col="red")+
  facet_grid(~breed)

# we can summarize the accuracy with a confusion matrix
conf_rf<-confusionMatrix(table(data$healthy,predicted$healthy))
conf_rf
#we can look at the importance of variables in the model:
# values with larger MeanDecreaseGini ie. "Gini Score' are more important in the random forest
importance(model)
# and plot it
varImpPlot(model)


# binomial regression -----------------------------------------------------

# we can compare the performance to a bionomial regression
model <- glm(healthy ~ grass * breed, data, family = "binomial")
mean(data$healthy==as.numeric(fitted(model)>0.5))

predicted<-data.frame(data)
predicted$healthy<-fitted(model, newdata=data)

ggplot(data, aes(x=grass, y=healthy))+
  geom_point()+
  geom_line(data=predicted, size=1, col="red")+
  facet_grid(~breed)

# check the confusion matrix
conf_glm<-confusionMatrix(table(data$healthy,as.numeric(predicted$healthy>0.5)))
conf_glm


#  Random forest for REGRESSION (quantitative response)
# Example: crop yield (t/ha) driven by temp, rainfall, soil N, plus nonlinearities
# --------------------------------------------------------------------------

set.seed(10)
n <- 250
temp <- runif(n, 12, 38)          # growing-season mean temp (°C)
rain <- runif(n, 50, 450)         # seasonal rainfall (mm)
soilN <- runif(n, 0.05, 0.35)     # soil nitrogen (%)

# True system (unknown to the model):
# - yield increases with rain up to an optimum then plateaus
# - heat stress penalty above ~32°C
# - temp x soilN interaction (fertile soils benefit more from warmth)
yield <- 3 +
  0.015*rain - 0.00002*(rain-250)^2 +      # hump/plateau rainfall effect
  0.25*soilN*temp +                        # interaction
  ifelse(temp > 32, -0.3*(temp-32)^2, 0) + # heat stress threshold
  rnorm(n, 0, 0.8)

dat_reg <- data.frame(temp, rain, soilN, yield)

head(dat_reg)
set.seed(10)
rf_reg <- randomForest(yield ~ ., data=dat_reg,
                       ntree=400, importance=TRUE)
rf_reg
plot(rf_reg)  # OOB MSE vs number of trees

# OOB performance (built-in CV)
pred_oob <- rf_reg$predicted
rmse_oob <- sqrt(mean((pred_oob - dat_reg$yield)^2))
r2_oob <- 1 - sum((pred_oob - dat_reg$yield)^2) /
  sum((dat_reg$yield - mean(dat_reg$yield))^2)
rmse_oob; r2_oob

# Predicted vs observed
plot(dat_reg$yield, pred_oob,
     xlab="Observed yield (t/ha)", ylab="RF OOB predicted yield")
abline(0, 1, col="red")


# --------------------------------------------------------------------------
# 2) Importance values help interpretation
# Use same regression example: show variable importance + partial dependence
# --------------------------------------------------------------------------

importance(rf_reg)
varImpPlot(rf_reg, main="RF variable importance (yield regression)")

# Partial dependence plots show the *shape* of effects
partialPlot(rf_reg, pred.data=dat_reg, x.var="rain",
            main="Partial effect of rainfall on yield")
partialPlot(rf_reg, pred.data=dat_reg, x.var="temp",
            main="Partial effect of temperature on yield")
partialPlot(rf_reg, pred.data=dat_reg, x.var="soilN",
            main="Partial effect of soil N on yield")


# --------------------------------------------------------------------------
# Multicollinearity and RF importance scores
# Example: two vegetation indices tracking same underlying vigor
# Prediction stays good, importance gets 'shared' or unstable
# --------------------------------------------------------------------------

set.seed(11)
n <- 300
vigor <- rnorm(n)  # latent "true canopy vigor"
ndvi <- greenness + rnorm(n, sd=0.25)
evi  <- greenness + rnorm(n, sd=0.25)   # highly correlated with NDVI
lai  <- rnorm(n)     # independent canopy trait
pri <- rnorm(n)     # another canopy trait, unrelated

# Response depends on vigor + a bit of LAI
biomass <- 10 + 3*vigor + 1.2*lai + rnorm(n, 0, 1)

dat_col <- data.frame(ndvi, evi, lai, biomass, pri)

cor(dat_col$ndvi, dat_col$evi)  # show strong collinearity

set.seed(11)
rf_col <- randomForest(biomass ~ ., data=dat_col,
                       ntree=400, importance=TRUE)
importance(rf_col)
varImpPlot(rf_col, main="Importance under multicollinearity")

# NDVI and EVI "split" importance because they carry overlapping info.


# neural network ----------------------------------------------------------

# now lets try an artificial neural network
# we are going to use the package neuralnet

# we have to do some shenanigans, most importantly, have to convert our dataframe into a model matrix
# this only necessary becase one of our predictors (genotype) is a categorical variable
# normally linear regression does this step automatically but neuralnet in R package requires we do it manually
head(data)
mod_mat<-model.matrix(~., data)
head(mod_mat)

data
# We can now build our neural network
# Some things to note here.
# we need to specify "healthy" as a factor
# we need to use grass + breedHereford which is is the factor variable from the model matrix
# rep = the number of times the model is created from random starting points, we need many to pick the best one
# we are setting the threshold to 0.2, which simply decreases runtime at the cost of causing the algorithm to converge faster (slightly less accurate)
# we only have 2 predictors so hidden layer of 2 nodes is as high as we really should go
# we set the activation function to "logistic" even though it is the default
model<-neuralnet(factor(healthy)~grass+breedHereford, data = mod_mat, hidden = c(2, 3), rep=100,  stepmax = 10000, threshold=.2, act.fct="logistic")

model$startweights
# we created 100 replicates to try to find the best model, and now we want to know which did best
# we can do this by taking the model with the the lowest error
# this command returns the index of the best model
best_model<-which(model$result.matrix["error",]==min(model$result.matrix["error",]))

# we can plot the best model.
# note the internal nodes and
plot(model, rep=best_model)

# to plot the data we do some shenanigans to make a plot of prediced values from the best model
predicted<-data.frame(data)
predicted$healthy<-predict(model, newdata=mod_mat, rep=best_model)[,2]

ggplot(data, aes(x=grass, y=healthy))+
  geom_point()+
  geom_line(data=predicted, size=1, col="red")+
  facet_grid(~breed)

# check the confusion matrix
conf_nn<-confusionMatrix(table(data$healthy,as.numeric(predicted$healthy>0.5)))


# compare results ---------------------------------------------------------

conf_glm
conf_rf
conf_nn







