library(reshape2)
library(ggplot2)
library(data.table)





## intro to PCA--> what is Principal component analysis?

# helps vizualize data and features -->  useful in constructing linear models... 

## variance--> 

#intuition for the methods behavior...

##  in 3 dimensional space, we can define , line that passes through space of points... finding new vetors that pass through nth dimensional space--> first principal components--> second most amount of variace, is orthogonal to the first 
## third principal is the last dimension that you can pass through these series of points --> new axis, pc1,2 and 3, each point has a value ont heat axis--> all of our points exist on new vectors... 
## numbers describing how much variance is explained
##cant do it on one dim data
# a new vector that passes through two points at the same time...
## when you do PCA you need to scale the data
## high in y and lower in x
## amount of variance explained --> the eigen value is the portportion of variance explain by each data set-> fraction of variance explained

## run the summary of the PCA
## glimpse into depth of system
## watch video about PCA's later

# its really just to find things in the data that may not have been observed before --> can reveal certain things that may not be shown::

#3 could PCA help to show something in the mysis data?
## biplot is the variance explained
# relevent if have coorelated predictor variables:
## value is if there is coorelational structure within data
## if 0 ccor, there would be flat pca if zero coorealtion



















iris<-iris[,1:5]
# Iris example:
str(iris)

plot(iris)
# have to remove Species column because it is not numerical
iris_pca<-prcomp(iris[,-5], scale=T)
summary(iris_pca)
#common plots:
#will explain
plot(iris_pca, type="l")
plot(iris_pca)

biplot(iris_pca)

str(iris_pca)
iris_pca$rotation
iris_pca$x

# get PC coordinates for all observations
pcs<-data.frame(iris_pca$x)
str(pcs)
str(iris)

# add species column back so we can make a plot
pcs$Species<-iris$Species
ggplot(pcs, aes(x=PC1, y=PC2, col=Species))+
  geom_point()+
  theme_bw()+
  coord_fixed(ratio = 1)

ggplot(iris, aes(x=Sepal.Width, y=Sepal.Length, col=Species))+
  geom_point()+
  theme_bw()+
  coord_fixed(ratio = 1)


ggplot(iris, aes(x=Petal.Width, y=Petal.Length, col=Species))+
  geom_point()+
  theme_bw()+
  coord_fixed(ratio = 1)

# comparing Species in PC space? interesting?
model<-lm(PC1~Species, pcs)
summary(model)

# examining other parts of PCA results
# PC loadings (contribution of each variable to the PCs)
loadings<-iris_pca$rotation
loadings_df<-data.frame(loadings)

loadings_df$variable<-row.names(loadings_df)

ggplot(loadings_df, aes(x=PC1, y=PC2, label=variable))+
  geom_text()+
  geom_segment(aes(x=0, y=0, xend=PC1, yend=PC2))+
  theme_bw()+
  coord_fixed(ratio = 1)


biplot(iris_pca)


iris_pca<-prcomp(iris[,-5], scale=T)

# loadings: relationship between variables and PCs
iris_pca$rotation

# scores: relationship between observations and PCs
iris_pca$x

# eigenvalues: variance explained by PCs
iris_pca$sdev^2

plot(iris_pca)
#correlation between Sepal.Length and PC1
cor(iris$Sepal.Length, iris_pca$x[,1])
#... also equal to loading * sqrt(eigenvalue)
loadings[1,] * iris_pca$sdev

# for all correlations (not necessarily useful but serves as proof of concept)
t(apply(loadings, 1, function(x) x*iris_pca$sdev))



####### Now for using PCA to generate PC that are then used in linear models

# we will start by reading in this cassava dataset:
# consists of 1006 observations of cassava landraces
# with climate or origin measures (bioclim https://www.worldclim.org/data/bioclim.html)
# and a trait: Dry_matter (root dry mass, ie yield)
cassava<-read.csv("Week 6/cassava.csv")
head(cassava)

#lets consider a linear model with Dry_matter ~ all climate variables
model<-lm(Dry_matter~., cassava)
summary(model)
car::vif(model)

model<-lm(Dry_matter~.-bio7, cassava)
summary(model)
car::vif(model) #lots of multicollinearity


# lets look at the pairwise correlation among variables in the data
melted_cormat <- reshape2::melt(cor(cassava))
ggplot(melted_cormat, aes(x=Var1, y=Var2, fill=value))+
  geom_tile()+
  scale_fill_gradientn(colors=c("orange2","white","green3"))+
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5))

#extract matrix of climate data
climate_variables<-cassava[,1:19]

# lets do a PCA
# Scale your data!
pca_climate<-prcomp(climate_variables, scale=T)

#the summary
summary(pca_climate)

# and the individual parts
str(pca_climate)
pca_climate$sdev
plot(pca_climate)
pca_climate$rotation
pca_climate$x

#Scree Plot
plot(pca_climate)

#biolot
biplot(pca_climate)

##### WARNING: example if we didnt scale our data

pca_climate<-prcomp(cassava[,1:19])

plot(cassava$bio12, scale(cassava$bio12))

summary(pca_climate)
plot(pca_climate,type = "l")
biplot(pca_climate)

# why is bio12 the most important variable for PC1 when we dont scale?
####

# ok rerun this to make it correct (scaled):
pca_climate<-prcomp(climate_variables, scale=T, center=T)

# loadings
pca_climate$rotation

# scores
pca_climate$rotation

# we can extract our PC coordinates for our observations
cassava$pc1<-pca_climate$x[,1]
cassava$pc2<-pca_climate$x[,2]
cassava$pc3<-pca_climate$x[,3]
cassava$pc4<-pca_climate$x[,4]
cassava$pc5<-pca_climate$x[,5]

str(cassava)

# lets look at the pairwise correlation among variables in the data again
melted_cormat <- reshape2::melt(cor(cassava))
ggplot(melted_cormat, aes(x=Var1, y=Var2, fill=value))+
  geom_tile()+
  scale_fill_gradientn(colors=c("orange2","white","green3"), name="Correlation")+
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5))


# we can make a model with only PC1 as predictor
model<-lm(Dry_matter~pc1, cassava)
summary(model)

ggplot(cassava, aes(x=pc1, y=Dry_matter))+
  geom_point()+
  geom_smooth(method = "lm")

# we can also make a model with multiple PCs as predictors
model<-lm(Dry_matter~pc1+pc2+pc3+pc4+pc5, cassava)
summary(model)
car::vif(model) #no multicollinearity

model<-lm(Dry_matter~pc1+pc2+pc3, cassava)
summary(model)

# all PCs as predictors... now were back to the problem of model selection? :(
PCs<-data.table(pca_climate$x)
PCs$Dry_matter<-cassava$Dry_matter
model<-lm(Dry_matter~., PCs)
summary(model)
car::vif(model) #multicollinearity again because we have all PCs

model<-lm(Dry_matter~.-PC19, PCs) # remove least important PC
summary(model)
car::vif(model) #no multicollinearity again


ggplot(cassava, aes(x=pc1, y=Dry_matter, col=bio1))+
  geom_point()+
  theme_classic()+
  geom_smooth(method="lm")+
  scale_color_gradient(low="orange",high="blue")


#Making PCA plots with ggplot2
ggplot(cassava, aes(x=pc1, y=pc2, col=Dry_matter))+
  geom_point()+
  theme_classic()+
  coord_fixed(ratio = 1) +
  geom_hline(yintercept = 0, linetype="dashed")+
  geom_vline(xintercept = 0, linetype="dashed")+
  scale_color_gradient(low="orange",high="blue")+
  xlab(paste0("PC1 (", round(pca_climate$sdev[1]^2 / sum(pca_climate$sdev^2) * 100), "%)")) +
  ylab(paste0("PC2 (", round(pca_climate$sdev[2]^2 / sum(pca_climate$sdev^2) * 100), "%)"))

pca_climate$rotation
ggplot(cassava, aes(x=pc2, y=pc3, col=Dry_matter))+
  geom_point()+
  theme_classic()+
  coord_fixed(ratio = 1) +
  geom_hline(yintercept = 0, linetype="dashed")+
  geom_vline(xintercept = 0, linetype="dashed")+
  scale_color_gradient(low="orange",high="blue")+
  xlab(paste0("PC2 (", round(pca_climate$sdev[2]^2 / sum(pca_climate$sdev^2) * 100), "%)")) +
  ylab(paste0("PC3 (", round(pca_climate$sdev[3]^2 / sum(pca_climate$sdev^2) * 100), "%)"))

ggplot(cassava, aes(x=pc1, y=pc2, col=bio1))+
  geom_point()+
  scale_color_gradient(low="blue",high="orange")+
  theme_classic()+
  coord_fixed(ratio = 1) +
  geom_hline(yintercept = 0, linetype="dashed")+
  geom_vline(xintercept = 0, linetype="dashed")+
  xlab(paste0("PC1 (", round(pca_climate$sdev[1]^2 / sum(pca_climate$sdev^2) * 100), "%)")) +
  ylab(paste0("PC2 (", round(pca_climate$sdev[2]^2 / sum(pca_climate$sdev^2) * 100), "%)"))


# plot loadings for each predictor
loadings <- as.data.frame(pca_climate$rotation)

library(ggrepel)
ggplot(data = loadings, aes(x = PC1, y = PC2, label = rownames(loadings))) +theme_classic()+
  geom_segment(aes(x=0, y=0, xend=PC1, yend=PC2), col="gray") +
  geom_text_repel(size = 4, col="blue", max.overlaps=20) +
  coord_fixed(ratio = 1) +
  xlab(paste0("PC1 (", round(pca_climate$sdev[1]^2 / sum(pca_climate$sdev^2) * 100), "%)")) +
  ylab(paste0("PC2 (", round(pca_climate$sdev[2]^2 / sum(pca_climate$sdev^2) * 100), "%)"))

