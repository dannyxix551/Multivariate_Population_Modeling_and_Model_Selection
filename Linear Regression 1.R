#PLS 206

# Linear regression part 1. -----------------------------------------------


library(datasets)

## Residuals, predicted values, slopes and intercepts in R:
#Trees dataset





###### notes for linear regressions 10/6/2025


## all models are wrong but some are useful--> george box

## regression to the population mean
## for every increase in x how much will y change
##what is a linear regression?
## sum of squares
## assumptions
## waht is hypothesis testing
## r^2 and variance

### difference between predicted and observed/fitted value == residual

## some are positive and negatice--> aove line and below
## to eliminate the positive and negative, sqaured then sumed==sum of sqaures

## regression finding the line thatproduces the least sum of squares
### we are trying to minimize the sum of sqaures--> to make it most accuracte to the regression

## lm(girth- height -- means as a function of heght, girth as a function of height, data set)

## ab line fucntion will draw line through plot

### normality of residuals is assumed

## model summary--> r ^2 value, p value, if p is low the null must go;

## what is the null hypothesis for linear model?

## there is no linear relationship--> reject the null if p is low


##  weak exogeneity--> related to experimental design, erronous measurements willl mess up relationships true
## linearity--> assumes relationship is really truly linear
## constant variance-->  variance in error does not depend on predictor

## multu colinnearity-->??


### violating assumptions of linearity--> 

## residuals vs fitted, non equal variace
## idea of constant variance--> coorelation between residuals and x 

## consider adding non-linear predictor
## you want them to sit along the diagnol line

## outlier plot 

## r^2 values and hypothesis testing--> defines the porportion of variance explained by predictor variables

##  var--> mean dif between y and predicted values of y 
## what is the fitted values 
## r^2 ==1 describes 100% of the variance, because the mean of variance of the predictor variables sits on the variance of the points to the mean 
## null--> predictors explain no change
##  what is the p value explaining? 

## model summary t value-- what is this? --> are they sig different than zero
## make sure you are focusing on what you are interested in.... carefull on what we are interpreting in relation to our question!
## does includngng any predocot explan more vaieance



## 290 seminars as well








##
















#look at "trees" dataset, we will compare tree height tree diameter (girth)
data(trees)
#perform some standard data checking
str(trees)
summary(trees)
hist(trees$Height)
hist(trees$Girth)

#plot a scatter plot between height and girth
plot(trees$Height, trees$Girth, col="red")

#create a linear (regression) model with lm()
model<-lm(Girth~Height, trees)
abline(model) #draw the regression line on the plot

#check out the model summaries
model
summary(model)
model_summary<-summary(model)

#examine individual components of the model summary object in R (its a list)
str(model_summary)

#check out the coefficients object, this has the model parameters and their statistics
model_summary$coefficients ## list extract what we want--> one of the items
model_coefficients<-model_summary$coefficients
class(model_coefficients)
str(model_coefficients)

data.frame(model_coefficients) # FYI if we were to convert to DF, notice change in column names. Spaces " " and parentheses "(" get converted to "." Reminder to use good variable naming in your data columns to avoid this kind of thing.


## matrix or an array
#extract the intercept and slope
intercept<-model_summary$coefficients[1,1]
# FYI: putting () around the line also prints the result
(intercept<-model_summary$coefficients[1,1])
(slope<-model_summary$coefficients[2,1])

#extract the residuals from the model & (add them as a column to the trees dataset)
model_summary$residuals ## diff between fitted value and obsevations
trees$residual<-model_summary$residuals ### shoes the - + direction of the difference
head(trees)


# Predicted values --------------------------------------------------------

#calculate the predicted values from the model for trees & (add as a column)
predict(model)
trees$predicted_Girth<-predict(model)
head(trees)

#Lets plot the predicted values alongside the observed values
plot(trees$Height, trees$Girth, col="red")
points(trees$Height, trees$predicted_Girth)
abline(model)
install.packages("ggplot")
library(ggplot)

ggplot(trees, aes(x=Height, y=Girth))+ geom_point(col = 'red')

#lets see if the predicted values really equal intercept + slope*Height
intercept+slope*trees$Height
trees$predicted_Girth
cor.test(intercept+slope*trees$Height, trees$predicted_Girth)

##How does it calculate predicted girth? 










#lets see if the Y=pred+resid is really true...
trees$predicted_Girth+trees$residual
trees$Girth
cor.test(trees$Girth, trees$predicted_Girth+trees$residual)

## 

#Making predictions of new values
plot(trees$Height, trees$Girth, col="red");abline(model)
predict(model, newdata = data.frame(Height=c(90))) #we pass new data.frame. ***must contain the predictor variable names
new_data<-data.frame(Height=c(50,70, 90))
new_data$Girth<-predict(model, new_data)
#how much do we expect the mean girth to change if all the trees grow 10 feet taller?
mean(predict(model, newdata = data.frame(Height=trees$Height+10)))-mean(trees$Girth)
## we can pass unseen observations to help predict whats there
## the new mean - old mean
# Checking model assumptions ----------------------------------------------
summary(model)

plot(model)


#simulating a dataset that violates assumptions of linearity
set.seed(1)
water<-rnorm(100)
#we are simulating a y variable to be a function of both x and x^2
#(with some residual variation introduced with rnorm())
growth<-100+4*water+rnorm(mean=0, sd=0.4, 100)+-7*(water+rnorm(mean=0, sd=0.4, 100))^2
water_data<-data.frame(growth, water)
plot(water_data$water,water_data$growth)
model<-lm(growth~water, data=water_data)
abline(model, col="red")
summary(model)
plot(model)
# adding a non-linear transformation to the model
model2<-lm(growth~water+I(water^2), data=water_data)
plot(water_data$water,water_data$growth)
points(data.frame(x=water, growth=predict(model2)), col="red", pch=20)
summary(model2)

plot(model2)

#simulating a dataset that violates constant variance (Heteroscedasticity)
set.seed(1)
biomass<-rnorm(10000, mean=10, sd=1)
# here the sd of the random values we are sampling from actually depend on the values of biomass
yield<-biomass*3+ rnorm(mean=1, sd=biomass*.5, 10000)
median(yield)
yield_data<-data.frame(yield, biomass)
plot(yield_data$biomass,yield_data$yield)
model_yield<-lm(yield~biomass, data=yield_data)
abline(model_yield, col="red")
summary(model_yield)
# when we check the model diagnostic plots we can see
#that the variance in residuals is larger at higher values of biomass
plot(model_yield, which = 3) #diagnostic plot 3

#we can see that the residuals are correlated with the fitted values with cor.test
cor.test(model_yield$fitted.values, sqrt(abs(scale(model_yield$residuals))))
#abs = absolute value
#scale = scaled (more on this later in the quarter)

# log transformation of yield to correct Heteroscedasticity-- what is this workd mean?

yield_data$log_yield<-log(yield_data$yield)
plot(yield_data$yield, yield_data$log_yield)

plot(yield_data$biomass, yield_data$log_yield)
model_log_yield<-lm(log_yield~biomass, data=yield_data)
# Plot only the third diagnostic plot (Scale-Location plot)
abline(model_log_yield, col="red")
plot(model_log_yield, which = 3) #diagnostic plot 3
plot(model_yield, which = 3)

# test if there is still Heteroscedasticity:
cor.test(model_log_yield$fitted.values, sqrt(abs(scale(model_log_yield$residuals))))

# bonus lesson: ggplot options for large data sets with overlapping points
plot(yield_data$biomass,yield_data$yield)

#install.packages("ggplot2")
library(ggplot2)
ggplot(yield_data, aes(x=biomass,y=log_yield))+
  geom_point(alpha=.1)+
  labs(x="Biomass (kg)", y="Yield (g)")
## ths plots the distribution of points
ggplot(yield_data, aes(biomass,log_yield))+
  geom_density_2d_filled()+
  labs(x="Biomass (kg)", y="Yield (g)")

# R2 ----------------------------------------------------------------------

#to examine R2 lets re-examine the relationship between girth and height
plot(trees$Height, trees$Girth, col="red")
model<-lm(Girth~Height, trees)

#look at the model summary, what is the R-squared?
summary(model)
# remember that R2 = (variance in y around mean - variance in y around fitted values)/variance in y around mean
# we can create these variables in R
var_mean<-var(trees$Girth)
var_fit<-var(model$residuals)

#lets check if they are correct
(var_mean-var_fit)/var_mean


#How does this compare to a standard correlation?
#Its the same!
cor.test(trees$Height, trees$Girth)
cor_results<-cor.test(trees$Height, trees$Girth)
str(cor_results)
cor_results$estimate^2
cor_results$p.value

# Hypothesis testing ------------------------------------------------------

# how do we know if our fit is "good" enough to be considered statistically significant?
# A look at our model summary
summary(model)
# there are several p-values
# the slope coefficient has a p-value
# the intercept has a p-value
# and the overall model has a p-value
# how does the overall model p-value compare to the slope p-value?
# Lets also look at the reuslts from our correlation test, how does that p-value compare?
cor_results

#  The F statistic --------------------------------------------------------

model<-lm(Girth~Height, trees)
model_summary<-summary(model)
model_summary

r2<-model_summary$r.squared #prop variance explained
k=1 #number of predictors
n=nrow(trees) # number of observations
f<-(r2/(k))/
  ((1-r2)/
     (n-k-1))
f

## F distribution has two different degrees of freedom (DF) values
df1=k # AKA numdf
df1
df2=n-k-1 #AKA dendf
df2

# formula for F statistic (you wont need to run this on your own data, the model output is sufficient, this is for educational purposes only)
f<-(r2/(df1))/
  ((1-r2)/
     (df2))
f
# compare to model
model_summary$fstatistic

pf(f, df1, df2, lower.tail = F) # "p" value = probability of observing this F value by chance, given df1, df2
# compare to model
model_summary

# ggplot has a built in function to plot an lm type model with error
ggplot(trees, aes(x=Height, y=Girth))+
  geom_point()+
  geom_smooth(method="lm")



# F statistic with multiple predictors ------------------------------------

model<-lm(Girth~Height+Volume, trees)
model_summary<-summary(model)
model_summary

r2<-model_summary$r.squared
k=2 #number of predictors is 2
n=nrow(trees)

f<-(r2/(k))/
  ((1-r2)/
     (n-k-1))
f

df1=k
df2=n-k-1

1-pf(f, df1, df2, lower.tail = T) # probability of observing F statistic given df1, df2 (too small for R, rounds to 0!)

#looking at our model again, you'll notice there are error bars around the intercept and slope estimates
summary(model)

# Multiple linear regression ----------------------------------------------
library(plotly)
install.packages("plotly")

plot3d <- plot_ly(trees,
                  x = ~Girth,
                  y = ~Height,
                  z = ~Volume,
                  type = "scatter3d",
                  color = ~Volume,
                  colors = c("red","blue","green"),
                  mode = "markers",
                  size=1)
plot3d

model<-lm(Volume~Girth+Height, trees)
summary(model)

# Interactions
model<-lm(Volume~Girth*Height, trees)
summary(model)

# we can calculate the predicted values by making up a dataframe with values of Height and Girth
predicted<-data.frame(Height=rep(60:90, each=13), Girth=rep(8:20, times=31))
predicted$Volume<-predict(model, predicted)

plot3d <- plot_ly(predicted,
                  x = ~Girth,
                  y = ~Height,
                  z = ~Volume,
                  type = "scatter3d",
                  color = ~Volume,
                  colors = c("red","blue","green"),
                  mode = "markers",
                  size=1)
plot3d

