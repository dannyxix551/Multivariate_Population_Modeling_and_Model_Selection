#PLS 206
# Linear regression part 2.

## install.packages("car")
library(car)

library(car) #provides the vif() function
# Visualize the data with ggplot2
library(ggplot2)



## multiple linear regression--> instead of fitted line we have fitted plane
## multiple linear regression dinds hthe slope and intercepts  that minimizes rcc
## + is going to be used to add variables to a model
## multicollinearity is when two variables are reduntant to each other...

## why we inerested--> we want to make causal inferences between relationships between variables

## we have variable x--> pos relationshop on and and z
## assumotions of uner -- weak exogeneity##linearity--> true linear
##multicollinearity--> predictors vars are coorelated to each other
## insiginig regress coe/ F stat iis 
## variance inflation factor--> VIF score meeds to be lower than 10






































#### Testing hypotheses: finding hidden relationships in agricultural systems

# Simulating data where temperature reduces beneficial insect activity,
# though both temperature and beneficial insects impact crop yield.
# We are interested in identifying the factors influencing crop yield.

# Simulate variables
temperature <- rnorm(100)  # Temperature data
beneficial_insects <- rnorm(100) - temperature  # Activity of beneficial insects decreases with higher temperature
crop_yield <- temperature + beneficial_insects + rnorm(100)  # Crop yield influenced by both temperature and beneficial insects

# Crop yield is not significantly correlated with temperature alone
plot(temperature, crop_yield, main = "Crop Yield vs Temperature", xlab = "Temperature", ylab = "Crop Yield")
summary(lm(crop_yield ~ temperature))
## why not causal anymore?

# Crop yield is significantly correlated with beneficial insects
plot(beneficial_insects, crop_yield, main = "Crop Yield vs Beneficial Insects", xlab = "Beneficial Insects", ylab = "Crop Yield")
summary(lm(crop_yield ~ beneficial_insects))
## there is multicollinearity?


# Relationship between temperature and beneficial insects
plot(temperature, beneficial_insects, main = "Temperature vs Beneficial Insects", xlab = "Temperature", ylab = "Beneficial Insects")

# Modeling the effects of both variables on crop yield reveals link between yield and temperature
model <- lm(crop_yield ~ beneficial_insects + temperature)
summary(model)
par(mfrow=c(2,2))
plot(model)
par(mfrow=c(1,1))

# Visualization of the combined effect
## don't use 3 dim in papers... make 2+3rd dimension
agri_data <- data.frame(temperature, beneficial_insects, crop_yield)
ggplot(agri_data, aes(x = temperature, y = crop_yield, color = beneficial_insects)) +
  geom_point(alpha = 0.8, size = 2) +
  scale_color_gradient2() +
  theme_minimal() +
  labs(
    title = "Crop Yield vs Temperature with Beneficial Insect Activity",
    x = "Temperature",
    y = "Crop Yield",
    color = "Beneficial Insects"
  )

#### Testing hypotheses: controlling for confounding variables

# Let's simulate some ecological data where rainfall influences both nutrient levels and plant yield.
# We are interested in understanding how rainfall and nutrients affect plant yield.

# Simulate data
rainfall <- rnorm(1000)  # Rainfall data
nutrients <- -rainfall + rnorm(1000)  # Nutrients negatively influenced by rainfall (e.g., leaching effect)
yield <- rainfall + rnorm(1000)  # Plant yield influenced by rainfall

# Yield is significantly correlated with rainfall
plot(rainfall, yield, main = "Yield vs Rainfall", xlab = "Rainfall", ylab = "Yield")
summary(lm(yield ~ rainfall))

# Yield is negatively correlated with nutrients
plot(nutrients, yield, main = "Yield vs Nutrients", xlab = "Nutrients", ylab = "Yield")
summary(lm(yield ~ nutrients))
### could come to false conclusion---> this would NOT BE true
### rainfall level--> this is where including co-variates is benifical
# When modeling both rainfall and nutrients together, rainfall is significant while nutrients are not
# This demonstrates that after accounting for rainfall, nutrients do not significantly predict yield
model <- lm(yield ~ rainfall + nutrients)
summary(model)
par(mfrow=c(2,2))
plot(model)
par(mfrow=c(1,1))
## create a co-variate to control for the predictor variable you are actually testing--> but how!

raindata <- data.frame(rainfall, nutrients, yield)
ggplot(raindata, aes(x = rainfall, y = yield, fill = nutrients)) +
  geom_point(alpha = 0.8, shape = 21, size = 2) +
  scale_fill_gradient2() +
  theme_minimal() +
  labs(title = "Yield vs Rainfall with Nutrient Levels", x = "Rainfall", y = "Yield", fill = "Nutrients")


#### Testing hypotheses: the problem of multicollinearity

# Let's simulate some ecological data where multicollinearity is a problem.
# Imagine a case where temperature and precipitation contribute to plant growth
# but temperature and precipitation also affect some measurement of the soil_microbiome.
# We are interested in understanding the factors that directly affect  plant growth.

temperature <- rnorm(1000)  # Temperature variable
precipitation <- rnorm(1000)  # Precipitation variable
soil_microbiome <- temperature + precipitation + rnorm(1000, sd=0.1)  # Soil microbiome influenced by temperature and precipitation
plant_growth <- temperature + precipitation + rnorm(1000)  # Plant growth influenced by temperature and precipitation
## just factors that effect plant growth
# Plant growth is significantly correlated by temperature
plot(temperature, plant_growth)
summary(lm(plant_growth~ temperature))

# Plant growth is significantly predicted by precipitation
plot(precipitation, plant_growth)
summary(lm(plant_growth~ precipitation))

# Plant growth is significantly predicted by soil microbiome
plot(soil_microbiome, plant_growth)
summary(lm(soil_microbiome~ precipitation))
## now we want to test it, be 100% accurate-- process of eliination?
###Evidence of collinearity:
# (1) Insignificant or weak regression coefficients in the multiple regression but highly significant F test for the whole model.
model <- lm(plant_growth ~ temperature + precipitation + soil_microbiome)
summary(model)
par(mfrow=c(2,2))
plot(model)
par(mfrow=c(1,1))
# (2) Evidence of high Variance Inflation Factor (VIF) > 10
vif(model)

## won't see multicollinearity with model plots

## multiple linear midel is making it more accurate! need to rmove multiple colluinearity variable
# We can try removing the variable that has the highest VIF score (soil_microbiome)
model <- lm(plant_growth ~ temperature + precipitation)
summary(model)
vif(model)
# Much better :)

### side quest: What if soil microbiome is perfectly correlated with other variables?

soil_microbiome <- temperature + precipitation
model <- lm(plant_growth ~ temperature + precipitation + soil_microbiome)
summary(model)
model_summary <- summary(model)
## this is true or false--> another name for something, what it means is what is perfectly colinear with other variables

model_summary$aliased
## variance inflation factor


# we should remove aliased variable (soil_microbiome) from model


# "birds" dataset: writing for loops and using apply functions ------------

#bd = bird density,
#grzinv = inverse of grazing impacts in area
#dist = distance to populated area
#height = average forest height
#peri = perimeter of forest patch
#mammal = density of large mammals
#area = patch area
#leg = cover by leguminous trees

#load the birds dataset
birds<-read.csv("C:/Users/dancr/Downloads/pls_206_in_class_code/birds.csv", row.names = NULL)
str(birds)
## remove column using null
birds$X<-NULL
#we are interested in bird density as a function of other variables.
#what do you notice from the model that suggests collinearity might be having and affect?
model<-lm(bd~grzinv+dist+height+peri+mammal+area+leg, birds)
summary(model)
vif(model)


## vif scores are really large

### start here on the canvas zoom









#alternative syntax "." means all other columns
## bd is a function of all others in the data.frame--> 
model_dot<-lm(bd~., birds)
summary(model_dot)
vif(model_dot)

# after removing variable with largest VIF (dist)--> remove var with largest vif value
model_reduced<-lm(bd~grzinv+height+peri+mammal+area+leg, birds)
vif(model_reduced)
summary(model_reduced)

#alternative syntax:
model_reduced2 <- lm(bd~.-dist, birds)
summary(model_reduced2)

#alternative syntax:
### full model had every pred, update it with/by removing a certain parameter
model_reduced3 <- update(model, . ~ . - dist)
summary(model_reduced3)

#### for future Friday session:
# what if we want to test individual predictors?
# lets write a for loop that calculates the correlation between bd and each variable individually
predictors<-colnames(birds)[2:8]
predictor<-predictors[1]
results<-data.frame()
for(predictor in predictors){
  model<-lm(birds$bd~birds[,predictor])
  model_summary<-summary(model)
  model_summary$coefficients
  message(predictor)
  message("p=", model_summary$coefficients[2,4])
  out<-(data.frame(predictor=predictor, p=model_summary$coefficients[2,4], r2=model_summary$r.squared))
  results<-rbind(out, results)
}


# we can also calculate this with an apply() function where we loop over the columns 2-8
# the ,2, in the function means we are looping over columns
# we create a little function where each column is going to be assigned to the variable "predictor"
#return() tells the function what to send back to the output
# this return is one of the major advantages over for() loops which dont return values without some shenanigans that I dont recommend
# recall that the p value for a cor.test() is the same as for a single linear regression
cors<-apply(birds[,2:8], 2, function(predictor){
  cor_results<-cor.test(predictor, birds$bd)
  return(cor_results$p.value)
})
#check the results
cors

# we can also use lapply() as well, for this we dont specify this will return a list
cors<-lapply(birds[,2:8], function(predictor){
  cor_results<-cor.test(predictor, birds$bd)
  return(cor_results$p.value)
})
cors



