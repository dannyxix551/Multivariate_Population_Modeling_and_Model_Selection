library(ggplot2)

# check out the model (design) matrix for a normal linear regression
# notice that the matrix has an intercept plus one column for each predictor

##  week 4 lecture on linear model code --> 
## categorial predictors, design matrix, hypothesis testing, cat and linear predictors



## yes or no categorical variable--> can calculate the mean value--> mean of b - the mean of a
## yi =b mean of a +b1 mean of b - mean of a *xi+ei
##  4 groups multiple predictor variables--> turn categorical groups into a number...
## creating a gen linear model--> go from a group vector to observation having a value of 1, pseudovariable created, 1, 0, new pseudo for x-c , 1, binary, 00 to 1
## if you follow that logic--> all of the other variables are multipled by 0-->  how to fit cat variables into quantitative system
## design matrix -->
## going from 2 cat, 
## linear model --> what is model sum telling us_--> very significant p value is mean of group a diff than 0
## are a and b different from each other--> whats the differnce between b and a...
## mean of group a difference in mean from group b--> difference of means??
##  group--> a vs everything else--> 
### you want to use pairwise compairon, mean difference between each other--> the reg is the mean of a to the other means...
## tukey test--> parwise between each 
## trap loooking at multiple catagorical variable with a vat model
### combining groups --> interpolating the change in b for example depends on the what the variable is doing at the moment of interpolation--> which is why it can be diffult to understand...


x<-rnorm(20)
z<-rnorm(20)
y<-x+rnorm(20)
data<-data.frame(y, x, z)
model<-lm(y~x+z, data)
model.matrix(model)
summary(model)





# now lets examine a dataset comprised of two groups and a y response
# simulate a dataset with two groups that have different mean values
a<-rnorm(mean=10, n=10)
b<-rnorm(mean=12, n=10)
data<-data.frame(group=rep(c("a","b"), each=10), y=c(a,b))

# lets make a plot to visualize these groups, 
# you can change the size and base_size numbers if it looks crazy on your computer
ggplot(data, aes(x=group, y=y, col=group))+
  geom_point(size=2)+
  theme_classic(base_size = 10)

# lets make a linear model where we model y as a function of group
model<-lm(y~group, data)
summary(model)

# how does this compare to a t.test? We have to specify that the variance are equal since this is an assumption of linear models
t.test(y~group, data, var.equal = T)
#check out the design matrix. Two important things to note.
# there is not column for group "a" it is the "default" level and its mean is the intercept
# group "b" is 0 and 1. Our observed data in group "b" has a 1, while group "a" has a 0
model.matrix(model)

#for fun lets see how to change the "default" (i.e. intercept) group level
data$group<-factor(data$group, levels=c("b","a"))
levels(data$group)
model<-lm(y~group, data)
#note that the coefficient for groupa is the negative of the slope for groupb in the previous model (before we changed the levels of group)
summary(model)
model.matrix(model)






### difference betweek lm summary output and output of the anova, mean different things and see lecture!











# Lets do the same for a dataset with 4 groups and y response variable
a<-rnorm(mean=10, n=10)
b<-rnorm(mean=12, n=10)
c<-rnorm(mean=14, n=10)
d<-rnorm(mean=8, n=10)
data<-data.frame(group=rep(c("a","b","c","d"), each=10),  y=c(a,b,c,d))
ggplot(data, aes(x=group, y=y, col=group))+
  geom_point(size=2)+
  theme_classic(base_size = 10)

model<-lm(y~group, data)
summary(model)

#how does this compare to an Analysis of Variance (ANOVA)?
anova(model)
#notice the design matrix. all rows have a 1 in the intercept which is the mean of group "a"
# observations in group "b" have a 1 in that column but not in group "c" and so forth
model.matrix(model)

# if we want to compare all groups against each other
TukeyHSD(aov(model))


# lets examine a dataset with both categorical varibles and linear variables
a<-rnorm(mean=10, sd=0.5, n=20)
b<-rnorm(mean=9, sd=0.5, n=20)
data<-data.frame(group=rep(c("a","b"), each=20), x=c(a,b))
data
data$is_b<-as.numeric(data$group=="b") #needed for simulation
# y is equal to x (plus 2 if it is in group b) , then some residuals (rnorm)
data$y<-(data$x*1+data$is_b*2)+rnorm(40, sd=0.5)

ggplot(data, aes(x=x, y=y, col=group))+
  geom_point(size=2)+
  theme_classic(base_size = 10)

#how strong is the relationship between x and y?
model<-lm(y~x, data)
summary(model)

#how strong is the relationship between group and y?
model<-lm(y~group, data)
summary(model)

#how does the model change when we add group?
model<-lm(y~group+x, data)
summary(model)
#notice the design matrix.
model.matrix(model)

# as a refresher, lets check that the r2 value is still works, even when we have categorical variables
(var(data$y)-var(model$residuals))/var(data$y)


## Lets simulate a data set with interactions between a categorical variable and a continuous variable

a<-rnorm(mean=1, sd=0.5, n=20)
b<-rnorm(mean=1, sd=0.5, n=20)
data<-data.frame(group=rep(c("a","b"), each=20), x=c(a,b))
data
data$is_b<-as.numeric(data$group=="b")
data$y<-(data$x*(1+data$is_b*-2)+data$is_b*2)+rnorm(40, sd=0.3)

ggplot(data, aes(x=x, y=y, col=group))+
  geom_point(size=2)+
  theme_classic(base_size = 10)

#how strong is the relationship between x and y?
model<-lm(y~x, data)
summary(model)

#how strong is the relationship between group and y?
model<-lm(y~group, data)
summary(model)

#how does the model change when we add group?
model<-lm(y~group*x, data)
summary(model)
#but the anova says there's no significant effect of group
anova(model)
#notice the design matrix.
model.matrix(model)

#example why interpreting data is difficult when there is an interaction term
# here we make the main "effect" (that is, intercept difference) of b 0 
data$y<-(data$x*(1+data$is_b*-2)+data$is_b*0)+rnorm(40, sd=0.3)
ggplot(data, aes(x=x, y=y, col=group))+
  geom_point(size=2)+
  theme_classic(base_size = 10)

#lm: no significant difference of groupb and groupa interecepts
model<-lm(y~group*x, data)
summary(model)
#but the anova says there's as a highly significant effect of group
anova(model)

### one more example where the main effect actually matters
# here we make the main "effect" of b -1 but we also give it a 0.5 slope (vs a slope of 2 for the a group)
data$y<-(data$x*(2+data$is_b*-1.5)+data$is_b*-1)+rnorm(40, sd=0.1)
ggplot(data, aes(x=x, y=y, col=group))+
  geom_point(size=2)+
  theme_classic(base_size = 10)

#lm shows significant difference in intercept of groupb
model<-lm(y~group*x, data)
summary(model)
#and the anova also says there's as a highly significant effect of group
anova(model)

# in model matrix with interaction groupb:x is created...
# is 0 when group == "a" and is equal to x when group == "b"
model.matrix(model)


















































#Store initial parameters in R.
N0 = 4;
R = 0.5;

# Compute population sizes at the specified times.
N1 = (1+R)*N0;
N2 = (1+R)*N1;
N3 = (1+R)*N2;
N4 = (1+R)*N3;
N5 = (1+R)*N4;
N6 = (1+R)*N5;
N7 = (1+R)*N6;
N8 = (1+R)*N7;
N9 = (1+R)*N8;
N10 = (1+R)*N9;

# Output values of N1 to N10.
cat(" N1 = ", N1, "\n",
    "N2 = ", N2, "\n",
    "N3 = ", N3, "\n",
    "N4 = ", N4, "\n",
    "N5 = ", N5, "\n",
    "N6 = ", N6, "\n",
    "N7 = ", N7, "\n",
    "N8 = ", N8, "\n",
    "N9 = ", N9, "\n",
    "N10 = ", N10, "\n");


##  N1 =  6 
##  N2 =  9 
##  N3 =  13.5 
##  N4 =  20.25 
##  N5 =  30.375 
##  N6 =  45.5625 
##  N7 =  68.34375 
##  N8 =  102.5156 
##  N9 =  153.7734 
##  N10 =  230.6602
# Output scatterplot of N vs t.
plot(x = c( 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), xlab = "t (time)",
     y = c(N0,N1,N2,N3,N4,N5,N6,N7,N8,N9,N10), ylab = "N (population size)",
     ylim = c(0,250), las = 1)











N0 = 4;
R = 0.5; #for use in the discrete model
r = 0.5; #for use in the continuous model

# Compute population sizes using closed-form solution to discrete model
t = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
N1 = N0*(1+R)^t;

# Output scatterplot of N vs t.
plot (x = t, y = N, 
      xlab = "t (time)", ylab = "N (population size)", 
      ylim = c(0,500), las = 1);

# Overlay curve of continuous model
curve(N0*exp(r*x), from=0, to=10, add = TRUE);

view(N1)
view(n3)
