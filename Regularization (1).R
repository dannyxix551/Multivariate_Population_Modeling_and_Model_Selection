###########
# Regularization (Ridge, Lasso, Elastic Net) b




##--> can shrink the beta coeficcient--> find the optimum predictably return based on the slopes of the different methods


# laas and elastic net--> shrinks them closer and closer to zero

## shrinking coeficients to find beridtter predictability--> for every increase in x we have dramatic outcome on y--> 

## lamda --> srtonger beta coeficcients are the stronger the penalty will beocme
## goal is to shrink the beta 
## coalessing toward a zero value---> 


## best value in lamda that icees best predicteve bvauel

## glm net slgihtly esoteric 


## lasso--> has different penalty term

## math shows that you can  create final output that converges toward zero and then is dropped

## become zero and are dropped from models 

## empirically test which model is best 

## ridge and lasso--> 
## amaller penatly term is better--> trying to optimize for the smallest beta coeficcient

## elastic net--> balance betwen ridge and lassw regression

## training your mdels on sme of your data, then predicting those pred values and if they are close to actual value then its a good model

## k fold- test and training data


## cross val--take the training sets, 5 fold-- split d into 5 == chunks

# repeated k fold cross validation--> second round of cross validation where it shiffles the chunks around , determining which is the most optimal

## things to consider --> would not be wise to

## find a model that fits a subset of the data--> thats what we use in the cross validataion


## accuracy? how much is it wrong? 


# how accurate would it be if we randomaly guessed

## rmse and r^2 adds more predictors

## root mean squres error--> square root of the mean error--> dif between observations and predicted things of the model

## mean absolute error-> low means its good--> ther eis not alot of error and there is not a good fit





## make this update  little later
