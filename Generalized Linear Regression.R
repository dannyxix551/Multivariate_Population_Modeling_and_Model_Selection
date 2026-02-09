
install.packages("emmeans")
install.packages("nnet")
install.packages("GGally")
library(emmeans) # calculates estimated means of groups from model
library(nnet) # used for multinomial regression
library(ggplot2)




#### generalized linear models --> 

###  categorical predictor--> intercept == to mean level, beta coeficcient 
## build linear model with multiple cat variables--> is this depended on that?
##effect of water depends on n and effect of n depends on water

### interaction terms are complicated to interperet.




##  
## 
## 


























# multiple categorical variables in linear regression ---------------------

# lets refresh ourselves on a model where we are predicting a response with a single categorical variable
data<-data.frame(N=rep(c("low","high"), each=20))
data$is_high_N<-as.numeric(data$N=="high")
data$yield<-(10+data$is_high_N+0.5)+rnorm(40, sd=0.5)
ggplot(data, aes(x=N, y=yield, col=N))+
  geom_point()

#   model of yield in relation to N
model<-lm(yield~N, data)
summary(model)
# lets calculate the means yield for high and low N
aggregate(data$yield, by=list(N=data$N), mean)

# Now we will explore linear regression when there are two categorical variables
# First simulate some data where we have high and low treatments of Nitrogen and
data<-data.frame(N=rep(c("low","high"), each=20), Water=rep(c("control","drought"), times=20))
data$is_high_N<-as.numeric(data$N=="high")
data$is_drought<-as.numeric(data$Water=="drought")
data$yield<-(10+data$is_high_N*1+data$is_drought*-2)+rnorm(40, sd=0.5)

ggplot(data, aes(x=N, y=yield, col=Water))+
  geom_point()

# now lets see a model where yield is predicted by both N and Water
model<-lm(yield~N+Water, data)
summary(model)
model.matrix(model)
# we can calculate the mean yield for high and low N either in drought or control
aggregate(data$yield, by=list(N=data$N, Water=data$Water), mean)

##what if we want to model an interaction between N and drought?
#Here we are simulating a scenario where N has no effect under drought on yield; this is from the "2+data$is_drought*-2" in the slope coefficient
data$yield<-(15+data$is_high_N*(2+data$is_drought*-2)+data$is_drought*-2)+rnorm(40, sd=0.5)

ggplot(data, aes(x=N, y=yield, col=Water))+
  geom_point()+
  scale_color_manual(values=c(control="lightblue", drought="orange"))+
  theme_classic()

model<-lm(yield~N*Water, data)
summary(model)

model_means<- data.frame(emmeans(model, ~ N * Water))

model_means

## model selection is very important 

## slope between the pos ot negative if the means are different -->
ggplot(data)+
  geom_point(aes(x=N, y=yield, col=Water))+
  scale_color_manual(values=c(control="lightblue", drought="orange"))+
  geom_point(data=model_means, aes(x=N, y=emmean), shape=24, fill="black")+
  theme_classic()
#compare the mean values to the fitted values:
aggregate(data$yield, by=list(N=data$N, Water=data$Water), mean)

#how do means compare to fitted without interaction

model<-lm(yield~N+Water, data)
summary(model)

model_means2 <- data.frame(emmeans(model, ~ N * Water))
model_means2

ggplot(data)+
  geom_point(aes(x=N, y=yield, col=Water))+
  scale_color_manual(values=c(control="lightblue", drought="orange"))+
  geom_point(data=model_means, aes(x=N, y=emmean), shape=21, fill="black")+
  geom_point(data=model_means2, aes(x=N, y=emmean), shape=24, fill="black")+
  theme_classic()

#compare the mean values to the fitted values:
aggregate(data$yield, by=list(N=data$N, Water=data$Water), mean)



## also: this will be used in the homework, here is how we plot a boxplot with ggplot
ggplot(data, aes(x=N, y=yield, col=Water))+
  geom_boxplot()+
  scale_color_manual(values=c(control="lightblue", drought="orange"))+
  theme_classic()
# and here is a violin plot, notice that they give slightly different information.
# violin plot provides more about the distribution of the data
ggplot(data, aes(x=N, y=yield, fill=Water))+
  geom_violin()+
  scale_fill_manual(values=c(control="lightblue", drought="orange"))+
  theme_classic()


# "Logistic" regression -----------------------------------------------------



#we can simulate a dataset where we are sampling a stand of trees made up of pine and oak trees
# and looking for a particular bird species. Our response variable is a 1 and 0 (do we observe the bird or not)





### generalized linear models---> response variable is a category--> binary functions or variables

## categotical state--> binomial regression--> depending on the response variable you are trying to model--> healthy or unhealthy

##whats the probability of one of these variables being observed in nature--> ##

### do we see  bird or not ---> zero or one

# family is binomial, was there mysis there yes or no?

## can you force a temporal scale into a glm
## binomial regreassin are a curve, where cruve reoresents orobability of healthy being true--> fitting it to the probability of a certain outcome.
pine=rbinom(n = 100, size = 1, prob = 0.7)
oak=rbinom(n = 100, size = 1, prob = 0.4)

data<-data.frame(tree=rep(c("pine","oak"), each=100), bird=c(pine, oak))

#we use the glm(family = "binomial") function
#glm = "generalized linear model"
#binomial refers to having 2 levels of response, technically this is a bernoulli regression
# as we will see later, binomial regression can also be appled to two column counts of yes and no
model<-glm(bird~tree, data, family="binomial")
summary(model)

#lets look at the table of bird~tree (is s a bird observed or not 0,1)
table(data$bird, data$tree)
# you may also encounter these tests for such data analyses:
fisher.test(table(data$bird, data$tree))
chisq.test(table(data$bird, data$tree))

##poisson--> intgers that are enriched with low number

# to plot this kind of data we need to format the data into  a data.frame out of the table of counts
bird_counts<-data.frame(table(bird=data$bird, tree=data$tree))
ggplot(bird_counts, aes(x=tree, y=Freq, fill=bird))+
  geom_bar(stat="identity", position="stack", col="black")

#what are the odds of a bird being observed in an oak tree according to our model?
# we calculate the odds with this equation, where B equals the Intercept of our model
# we can extract this with the coef(model) function or we could just look at the model summary
B0=unname(coef(model)[1])
oak_prob=exp(B0)/(1+exp(B0)) # odds of bird in oak
log(oak_prob/(1-oak_prob)) # B0 = log odds of a bird in oaks
# note that B0 is negative: this is because oak trees are less likely to have birds (probability less than .50)

#what are the odds of a bird being observed in an pine tree according to our model?
# we need to use the B1 (treepine) coefficient for this
B1=unname(coef(model)[2])
pine_prob=exp(B0+B1)/(1+exp(B0+B1)) # remember B1 is the difference between groups, so we need to add B0 and B1
log(pine_prob/(1-pine_prob))
# note that log odds B0+B1 is positive: this is because pine trees are more likely to have birds (probability greater than .50)

#how accurate is our model? There are many ways to test this, we will explore one below.
# in other words, how often are we correctly predicting the bird (whether or not a bird is observed as a function of which kind of tree is sampled)
# Lets walk through some R code to get the answer...

# first we can look at the predicted values of our model
# these are the log odds ratio,
# so negative values mean the model is predicting that the observation is likely a 0
# positive values mean the model is th predicting that the observation is likely a 1
predict(model)

# we can test if the predicted values are greater than 0 with...which returns a bunch of T and F values
predict(model)>0

#which can be converted to numeric 0 and 1 with...
as.numeric(predict(model)>0)

# and we can compare these predictions to our original data with.. TRUE values are cases where the prediction matches the observed
as.numeric(predict(model)>0)==data$bird

# sum this (since in R TRUE is treated as 1 and FALSE is treated as 0) with... to get the total number of correct predictions
sum(as.numeric(predict(model)>0)==data$bird)

#and divide by the total number of observations (number of rows with nrow()) for a final percent correct
# the above calculations can just be combined into this single line to get the proportion of correct predictions by this quick method
prop_correct<-sum(as.numeric(predict(model)>0)==data$bird)/nrow(data)
prop_correct

# Difference between predict() and fitted() for glm:
head(predict(model)) # log odds
head(fitted(model)) # converted to actual probabilities
head(predict(model, type="response")) # converted to actual probabilities

# fitted(model) >0.5 is the same test as predict(model)>0
prop_correct<-sum(as.numeric(fitted(model)>0.5)==data$bird)/nrow(data)

# Logistic regression with numerical predictors ---------------------------

## Logistic regression allows us to predict a categorical variable (which we treat as 0 or 1) as function of numerical variables too
# Lets imagine a dataset where 100 cows had different proportions of their diet were grass
# We then scored them as healthy (1) or unhealthy (0)
grass <- runif(100,min=18, max=80)
data<-data.frame(grass)
# we want to predict the probaility of healthiness as a function of grass diet
# we can make up an intercept of -7 and and effect of grass of 0.2.
# because the intercept is negative, it means that when grass diet is 0, the probability of being healthy is very low
# and since the slope of grass is positive, it means the chances of being healthy increases with grass in the diet
xb <- -7 + 0.2*data$grass
# we convert these log odds ratios into probabilities with this function
# I wouldnt stress too much about these function other than to know that this is how we convert odds ratios into probabilities which are a bit more intuitive
p <- 1/(1 + exp(-xb))
#from the probabilies we can simulate data with the rbinom() function which draws from 0 or 1 by the probabilies provided in p
data$healthy <- rbinom(n = 100, size = 1, prob = p)

ggplot(data, aes(x=grass, y=healthy, col=healthy))+
  geom_point()

# we use the glm(family = "binomial") function
model <- glm(healthy ~ grass , data, family = "binomial")
summary(model)

### special note: binomial regression requires either that response is numeric (0 and 1) or a factor with 2 levels.
data$healthy<-ifelse(data$healthy==1,"Healthy","Unhealthy")
class(data$healthy)
# error because class data$healthy is "character"
model <- glm(healthy ~ grass , data, family = "binomial")

# convert to factor
data$healthy<-factor(data$healthy, levels=c("Unhealthy","Healthy"))

model <- glm(healthy ~ grass , data, family = "binomial")
summary(model)

#lets calculate what proportion "correct" our model is predicting health
# First though, since we just made this a factor, we have to convert back to numeric
data$healthy<-as.numeric(data$healthy) #note that it results in 1,2
data$healthy<-data$healthy-1 # so we have to subtract 1...

## now...
prop_correct<-sum(as.numeric(predict(model)>0)==data$healthy)/nrow(data)
prop_correct


#we can make this temporary dataframe which we add predicted values from the model to so we can plot the linear regression curve
predicted_data<-data
predicted_data$healthy<-predict(model, type="response")

ggplot(data, aes(x=grass, y=healthy, col=healthy))+
  geom_jitter(height = 0.01, width=0)+
  geom_line(data=predicted_data)+
  scale_y_continuous(breaks=c(0,1))+
  theme_classic()+
  geom_hline(yintercept = 0.5, linetype="dashed")



# like standard regression we can have multiple predictors of course too
# lets add breed to the model
data$breed <- sample(c("Angus","Hereford"), size = 100, replace = TRUE)
data$isAngus<-as.numeric(data$breed=="Angus")
xb <- -9 + 0.2*data$grass+4*data$isAngus
p <- 1/(1 + exp(-xb))

data$healthy <- rbinom(n = 100, size = 1, prob = p)

ggplot(data, aes(x=grass, y=healthy, col=breed))+
  geom_jitter(height = 0.01, width=0)

model <- glm(healthy ~ grass + breed, data, family = "binomial")
summary(model)

prop_correct<-sum(as.numeric(predict(model)>0)==data$healthy)/nrow(data)
prop_correct

predicted_data<-data
predicted_data$healthy<-predict(model, type="response")

ggplot(data, aes(x=grass, y=healthy, col=breed))+
  geom_jitter(height = 0.01, width=0)+
  geom_line(data=predicted_data)


## Binomial regression with counts of "Germinated", "Not_Germinated"
# Simulated dataset parameters
n_units <- 100
total_seeds_per_unit <- 10

# Simulating data
germination_data_large <- data.frame(
  Pot = 1:n_units,
  Light_Level = sample(c("Low", "High"), n_units, replace = TRUE)
)
# Add a quantitative predictor: Soil_Quality (randomly between 1 to 10)
germination_data_large$Soil_Quality <- sample(1:10, n_units, replace = TRUE)

# Adjust germination rate based on Soil_Quality
# Let's assume that for each unit increase in Soil_Quality, there's a 5% increase in germination probability
adjustment <- 0.05 * germination_data_large$Soil_Quality

# Adjust the germination probabilities based on Soil_Quality, ensuring they stay within [0, 1]
germination_data_large$Germinated <- ifelse(
  germination_data_large$Light_Level == "Low",
  rbinom(n_units, total_seeds_per_unit, prob = pmin(1, pmax(0, 0.4 + adjustment))),
  rbinom(n_units, total_seeds_per_unit, prob = pmin(1, pmax(0, 0.6 + adjustment)))
)

germination_data_large$Not_Germinated<- 10-germination_data_large$Germinated


# Display the first few rows of the updated dataset
head(germination_data_large)


ggplot(germination_data_large, aes(x = Soil_Quality, y = Germinated, color = Light_Level)) +
  geom_point(position = position_jitter(width = 0.3, height = 0.3), alpha = 0.6) + # Jittering for better visualization
  labs(title = "Germination Count by Soil Quality",
       x = "Soil Quality",
       y = "Number of Seeds Germinated") +
  theme_minimal()

# Fit binomial regression
model_binomial <- glm(cbind(Germinated, Not_Germinated) ~ Light_Level+Soil_Quality,
                      data = germination_data_large,
                      family = "binomial")

# Display summary
summary(model_binomial)


# comparison with linear model with germination fraction as response
model_linear <- lm(Germinated/10 ~ Light_Level+Soil_Quality,
                      data = germination_data_large,
                     )

# Display summary
summary(model_linear)

# First, generate the predicted probabilities
germination_data_large$Predicted_Prob <- predict(model_binomial, type = "response")

germination_data_large$Predicted_Prob_linear <- predict(model_linear)


# Convert these probabilities to expected counts of successes
# Given that each unit has total_seeds_per_unit seeds
germination_data_large$Predicted_Count <- germination_data_large$Predicted_Prob * total_seeds_per_unit

# Convert these probabilities to expected counts of successes
# Given that each unit has total_seeds_per_unit seeds
germination_data_large$Predicted_Count_linear <- germination_data_large$Predicted_Prob_linear * total_seeds_per_unit

# Now, add these predicted values to your ggplot
p <- ggplot(germination_data_large, aes(x = Soil_Quality, y = Germinated, color = Light_Level)) +
  geom_point(position = position_jitter(width = 0.3, height = 0.3), alpha = 0.6) +  # Observed values
  geom_line(aes(y = Predicted_Count, group = Light_Level)) + # Predicted values
  geom_line(aes(y = Predicted_Count_linear, group = Light_Level), linetype="dashed") +

  labs(title = "Germination Count by Soil Quality",
       x = "Soil Quality",
       y = "Number of Seeds Germinated") +
  theme_minimal()

print(p)


# Multinomial regression --------------------------------------------------
# multinom() from library(nnet)
library(GGally)
library(nnet)

iris
ggpairs(iris, aes(color = Species))+
  theme_bw(base_size = 8)
iris$Species

model <- nnet::multinom((Species) ~ Sepal.Length +
                    Sepal.Width +
                    Petal.Length +
                    Petal.Width, data = iris)

summary(model)

predicted_species <- predict(model, newdata = iris)

table(predicted_species, Species=iris$Species) #confusion matrix


# Poisson -----------------------------------------------------------------

set.seed(123)  # Setting seed for reproducibility

# Sample data parameters
n_samples <- 300

# Simulating amount of pesticide used (in liters per acre)
pesticide_used <- runif(n_samples, 0, 10)  # Random values between 0 and 10

# Simulating type of crop grown
crops <- sample(c("Wheat", "Corn", "Rice"), n_samples, replace = TRUE)

# Simulating number of pests based on the predictors
# Here we're assuming:
# - Baseline number of pests is higher for Wheat, followed by Corn, and then Rice.
# - Pesticide has a negative effect on the number of pests.
lambda <- exp(3 +
                -0.2*pesticide_used +
                ifelse(crops == "Corn", -0.5, 0) +
                ifelse(crops == "Rice", -1, 0))

number_of_pests <- rpois(n_samples, lambda)

# Combine data into a dataframe
data <- data.frame(Crop = crops, PesticideUsed = pesticide_used, NumberOfPests = number_of_pests)

head(data)

hist(data$NumberOfPests, breaks=100, col="red")

model <- glm(NumberOfPests ~ PesticideUsed +
               Crop, data = data, family = "poisson")

summary(model)

model_linear <- lm(NumberOfPests ~ PesticideUsed +
               Crop, data = data)

summary(model_linear)
plot(model_linear)

plot(predict(model, data, type = "response"), data$NumberOfPests);abline(a=0, b=1, col="red")
plot(model)
plot(predict(model_linear, data), data$NumberOfPests);abline(a=0, b=1, col="red")
plot(model_linear)

# Type I vs Type III ANOVA ------------------------------------------------

simulate_conservation_exp <- function(n_forest_protected, n_forest_unprotected, n_grassland_protected, n_grassland_unprotected) {

  set.seed(123)  # For reproducibility
  # Simulating data for forest protected
  forest_protected <- data.frame(
    HabitatType = rep("Forest", n_forest_protected),
    ProtectionStatus = rep("Protected", n_forest_protected),
    SpeciesRichness = rnorm(n_forest_protected, mean = 50, sd = 10)
  )

  # Simulating data for forest unprotected
  forest_unprotected <- data.frame(
    HabitatType = rep("Forest", n_forest_unprotected),
    ProtectionStatus = rep("Unprotected", n_forest_unprotected),
    SpeciesRichness = rnorm(n_forest_unprotected, mean = 45, sd = 10)
  )

  # Simulating data for grassland protected
  grassland_protected <- data.frame(
    HabitatType = rep("Grassland", n_grassland_protected),
    ProtectionStatus = rep("Protected", n_grassland_protected),
    SpeciesRichness = rnorm(n_grassland_protected, mean = 70, sd = 10)
  )

  # Simulating data for grassland unprotected
  grassland_unprotected <- data.frame(
    HabitatType = rep("Grassland", n_grassland_unprotected),
    ProtectionStatus = rep("Unprotected", n_grassland_unprotected),
    SpeciesRichness = rnorm(n_grassland_unprotected, mean = 48, sd = 10)  # Lower mean due to interaction effect
  )

  # Combining all the data frames
  data <- rbind(forest_protected, forest_unprotected, grassland_protected, grassland_unprotected)

  return(data)
}

# Unbalanced design (different sample sizes per treatment group)
data <- simulate_conservation_exp(n_forest_protected=50,
                                  n_forest_unprotected=75,
                                  n_grassland_protected=50,
                                  n_grassland_unprotected=150)
# Analysis
head(data)
table(data$HabitatType, data$ProtectionStatus)
# Type I (Sequential) ANOVA
model_I <- lm(SpeciesRichness ~ HabitatType*ProtectionStatus, data = data)
anova(model_I)

model_Ib <- lm(SpeciesRichness ~ ProtectionStatus*HabitatType , data = data)
anova(model_Ib)

# Type III ANOVA
library(car)
options(contrasts = c("contr.sum", "contr.poly"))
model_III <- lm(SpeciesRichness ~ HabitatType * ProtectionStatus, data = data)
Anova(model_III, type = "III")

model_IIIb <- lm(SpeciesRichness ~  ProtectionStatus * HabitatType, data = data)
Anova(model_IIIb, type = "III")


# reset to default
options(contrasts = c("contr.treatment", "contr.poly"))

# Compare when sample sizes are the same (balanced design)
data <- simulate_conservation_exp(n_forest_protected=100,
                                  n_forest_unprotected=100,
                                  n_grassland_protected=100,
                                  n_grassland_unprotected=100)


# Type I (Sequential) ANOVA
model_I <- lm(SpeciesRichness ~ HabitatType*ProtectionStatus, data = data)
anova(model_I)

model_Ib <- lm(SpeciesRichness ~ ProtectionStatus*HabitatType , data = data)
anova(model_Ib)

# Type III ANOVA
library(car)
options(contrasts = c("contr.sum", "contr.poly"))
model_III <- lm(SpeciesRichness ~ HabitatType * ProtectionStatus, data = data)
anova_results<-Anova(model_III, type = "III")
anova_results

# reset to default
options(contrasts = c("contr.treatment", "contr.poly"))





### polynomials and general additive models -->  

### not all things are linear--> 
## linear fit y=x
# quadratic model 
## cubic model
## polynimial terms that add curves to the fitted values
##gam--> generalized additive model-> non parametric smoothing functions
## will find a fit to the data--> for example--> measurements--> isotope of nitrogen deri
## we can fit different poly nomials t different functions
# overfit model to
# generalized additive model--> thought process--> each section of x split into indivd peieces, knots and weights, fitted values become sum of induvudual functinons
## gamm a == high value ==linear model
## gam==low, == wiggly model

## find a curve that fits data that has low sum of sqURES AND LOW 
## finding wigglyness is cooked into the gam model 
## in ecology its very important 
## method == reml, prefered method for gamma optimization fit , restricted max liklihoo
## if use reml
## add fitted values--> add interaction effects
## fit three differnt curves for three different species--> other people use it for is spatial modeling--> build preditive model f thwehre you can see 
### general adative--> can capture non-linearity better - fit than other non-linear model
## van use it for classification--> samples across a landscape- and fit species 
##  can do gam even with binomal too!
## generalized adative, series of knots in fitted values ==
## the number of knots -->is more knots more accurate? 
## varaibility in a field design... we randomize positions across the field... you can fit linear or non linear curvature to changing spatial distribution of your measurements---> 
## orthogonal polynomials--> source of confusion... when to use them/ 
## orthogonality--> in stat terms--> if it exist if its perfectly uncorrelted with somthing
## orthogonal--> perfectly non coorelated with one another--> no coorelation between two variables
## multipele orthogonals--> remove multicoliniearity!
## orthogonal polynomials- not coorelated with each other--> what could you use this for? predicted values will be the same
# concepts of splines--> gam--> imagine we fit different curves to diffeerent regions 20-50 cubic 50-100 squared
## popular non linear model that can be fit to nonnlinear data
## what is this concept most useful for? splots along x axis to fit cubics
## more accurate that a polynomial function alone--> how to determine how many knots you need?
## how accurate was our model--> intro to cross validatiaon, train on half and predict on other half
## local regression--> if you run in ggplot we can grab points that are nearby points--> 
## in GAM --. number oof splines that can be fit to the functions
## Non linear modeling --> incomme as a model of age and year
## scaling dat--> one unit of data does not make sense for all parts of data--> IT MAKES IT A UNITLESS FOR TO WHERE THINGS LINE UP AS 1 TO 1, KEEP THIS IN MIND WHEN DOING CROSS VLIDATAION
## SCALEd values should be perfectly coorelated-->, but there are no units...
## cyclic spines--> circading or diel over the course of a year??--> 
### if unsure of model predictos let it pick for selection = cures
##  


