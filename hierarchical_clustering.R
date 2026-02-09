




## hierarchical clustering --> you have a large matrix of information and you are trying to look for patterns--> re-order rows and columns looking for trends--> reordering tfor simmularities nd differences? 
## usefull for multivariate data--> k means clustering? 
## k is setting the number of items
## heirarchicl == each has there own cluster-- ducks to ducks--> define what is related to each other based on certian thing

# need to understand what is happening under the hood here--

## distanece matrix from clusters from important things to least important things--> what is the algoith that links thingfs togetejr into clusters

## 2 dimensions--> x and y, running --> 





# simulate data
multinorm<-function(means, sds, samp_size, name){
  if(length(means)!=length(sds)){
    stop("Length of means and sds arguments must be the same") }
  out<-data.frame(lapply(1:length(means), function(i){
    rnorm(n=samp_size, mean=means[i], sd=sds[i])
  }))
  colnames(out)<-letters[1:length(means)]
  out$group=name
  return(out)
}

# We are simulating 2 groups which differ by 2 variables.
# We are using the homemade function above so no need to get the details of what this code does:
data<-do.call("rbind", list(multinorm(means=c(10,10), sds=c(1,1), samp_size = 30, name="group1"),
                            multinorm(means=c(20,10), sds=c(1,1), samp_size = 30, name="group2")))

# lets look at the resulting data object:
# there are 2 variables (a and b, and group which is simulated)
str(data)

##########
# we will be making plots in ggplot2
library(ggplot2)

#lets have a look at the data in a scatterplot between our 2 variables
ggplot(data, aes(x=a, y=b))+
  geom_point(size=2)

# to perform hierarchical clustering we need to create a distance matrix.
# it is best to use scaled data.
# The default method for distance calculation is "euclidean" but we specify here in the code anyway
distance_matrix<-dist(scale(data[,1:2]), method="euclidean")

# we can then perform hierarchical clustering
# the default method is "complete" linkage though we specify in the code here anyway
hc<-hclust(distance_matrix, method = "single")
str(hc)
cutree(hc, k=2)
plot(hc)
# we can extract two clusters, k=2, from the tree with cutree(k=2).
# we can add the results to the data frame as a new column...
data$hclust_2<-factor(cutree(hc, k=2))

#... plot with ggplot
ggplot(data, aes(x=a, y=b, col=hclust_2))+
  geom_point(size=2)

# lets also plot the dendrogram
# for this we can use ggdendro package which allows us to convert the hclust() results in dendogram format ready for data.frame style plotting in ggplot
library(ggdendro)

# create a dendro_data object
dendr <- dendro_data(hc, type="rectangle")
str(dendr)

# add a cluster column to the dendr$label object
dendr$labels$hclust_2<-data$hclust_2[as.numeric(dendr$labels$label)]

# we can plot with regular ggplot() functions.
# The lines of the dendrogram are drawn with geom_segment
# the points are drawn with geom_point out of the dendr$labels data object
ggplot() +
  geom_segment(data=dendr$segments, aes(x=x, y=y, xend=xend, yend=yend)) +
  geom_point(data=dendr$labels, aes(x=x, y=y, col=hclust_2), size=1)

#### TRY RERUNNING ABOVE BUT CHANGE method="single" TO method="complete"

distance_matrix2<-dist(scale(data[,1:2]), method="euclidean")

hc<-hclust(distance_matrix2, method = "complete")
data$hclust_2<-factor(cutree(hc, k=2))

ggplot(data, aes(x=a, y=b, col=hclust_2))+
  geom_point(size=2)

dendr <- dendro_data(hc, type="rectangle")
dendr$labels$hclust_2<-data$hclust_2[as.numeric(dendr$labels$label)]

ggplot() +
  geom_segment(data=dendr$segments, aes(x=x, y=y, xend=xend, yend=yend)) +
  geom_point(data=dendr$labels, aes(x=x, y=y, col=hclust_2), size=1)

# lets explore some real data

# Genetic data in Arabidopsis thaliana

a_thaliana<-read.csv("Week 8/a_thaliana_file.csv") # meta data on 150 wild arabidopsis plants
genetics<-read.csv("Week 8/a_thaliana_genetics_file.csv") #genetic data (10933 mutations) for each accession
str(genetics)
str(a_thaliana)
distance_matrix<-dist(genetics[,-ncol(genetics)], method="binary") # distance matrix of genetic variation (removing the last column which is the Name of the plant accession)
hc<-hclust(distance_matrix, method="complete") # we will use complete linkage

dendr <- dendro_data(hc, type="rectangle")  # to plot we can convert into a dendrogram

# add Name and Region columns to the dendr$label object
# I will explain these lines in class as they are just getting R to work for what we need. In particular, I will explain the match() function which is extrememly valuable for merging datasets.
dendr$labels$Name<-as.character(genetics$Name[as.numeric(hc$order)])
dendr$labels$Country<-a_thaliana$Country[match(dendr$labels$Name, a_thaliana$Name)]


pdf("~/Desktop/A_thaliana_tree.pdf", width=60, height=15)
ggplot() +
  geom_segment(data=dendr$segments, aes(x=x, y=y, xend=xend, yend=yend), linewidth=2) +
  geom_point(data=dendr$labels, aes(x=x, y=y, col=Country), size=10)+
  geom_text(data=dendr$labels, aes(x=x, y=y, col=Country, label=Country), size=10, angle=90, hjust=1.5)+
  theme_void()+
  theme(axis.text.x=element_text(angle=90))+
  scale_y_continuous(expand=c(0.2,0))
dev.off()


# we are going to make heatmaps with gplots package heatmap.2() function
library(gplots)

# first some gene exression data in Arabidopsis thaliana from different tissues (1000 genes, 53 tissues)
expression<-read.csv("Week 8/gene_expression.csv")
str(expression)
expression_matrix<-t(as.matrix(scale(expression))) # need to do this to run heatmap.2() function below

# the plot is too big for the R window, so lets plot to a pdf file
# change the directory to something you want
pdf("~/Desktop/heatmap_expression.pdf", width=30, height=9)
# we need to convert the dataframe to a matrix. I am  also transposing the dataset (this doesn't change results but does change orientation of heatmap)
heatmap.2(expression_matrix, scale = "row", trace = "none", margins = c(5,20), main = "Manhattan", distfun = function(x){dist(x, method="manhattan")})

heatmap.2(expression_matrix, scale = "row", trace = "none", margins = c(5,20), main = "Euclidean", distfun = function(x){dist(x, method="euclidean")})

heatmap.2(expression_matrix, scale = "col", trace = "none", margins = c(5,20), main = "Manhattan", distfun = function(x){dist(x, method="manhattan")})

heatmap.2(expression_matrix, scale = "col", trace = "none", margins = c(5,20), main = "Euclidean", distfun = function(x){dist(x, method="euclidean")})
dev.off()

# next we can look at data of climate variables for landraces of cassava collected in Colombia
cassava_clim<-read.csv("Week 6/cassava.csv")
cassava_clim<-scale(cassava_clim[,1:19])

cassava_matrix<-t(as.matrix(cassava_clim)) # need to do this to run heatmap.2() function below

pdf("~/Desktop/cassava_climate.pdf", width=8, height=7)

heatmap.2(cassava_matrix, trace = "none", margins = c(5,10), main = "Single",hclustfun = function(x) hclust((x), method="single"),  distfun = function(x) dist((x), method="manhattan"))

heatmap.2(cassava_matrix, trace = "none", margins = c(5,10), main = "Complete",hclustfun = function(x) hclust((x), method="complete"),  distfun = function(x) dist((x), method="manhattan"))

heatmap.2(cassava_matrix, trace = "none", margins = c(5,10), main = "Ward",hclustfun = function(x) hclust((x), method="ward.D2"),  distfun = function(x) dist((x), method="manhattan"))

heatmap.2(cassava_matrix,scale="row", trace = "none", margins = c(5,10), main = "Ward row",hclustfun = function(x) hclust((x), method="ward.D2"),  distfun = function(x) dist((x), method="manhattan"))

heatmap.2(cassava_matrix,scale="col", trace = "none", margins = c(5,10), main = "Ward col",hclustfun = function(x) hclust((x), method="ward.D2"),  distfun = function(x) dist((x), method="manhattan"))

dev.off()

