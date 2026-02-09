# K-means clustering tutorial
# 1. Simulate low-dimensional data and run k-means (with and without scaling)
# 2. Explore how the choice of k affects results (elbow + silhouette)
# 3. Extend to high-dimensional data + PCA visualization
# 4. Apply k-means to real climate data from Colombia
#
# Learning goals:
# - Understand why scaling is important for k-means
# - See how to choose k using within-SS and silhouette scores
# - Connect clustering to real ecological data

library(ggplot2)
library(raster)
library(geodata)
library(maps)
library(rworldmap)
library(cowplot)
library(cluster)




## unsupervised learning--> convert large multivariate data sets into a few dimensions---> reducing them down into quantifiable observations


## one popular application-->  k means clustering --> measuring genetic clusters--> form phylogenetic relations--> snips across the genome
# clustering data into networks --> 
## k means clustering--> classify two different groups --> looking for significant things within the groups



## elbow method--> sum of squares- dif between mean and obsservaions--> wherever it turns--> use this model
## silhoutete score- score that has to do of variance intra vs inter variance--> how much smaller is inter vs intra variance
## determining the optimum k value for the data set--> 
# whichever has the smallest sum of squares--> determine on where process starts -->

## if it clusters wrong with the k-cluster--> it is likely due to scale--> where the different in scale makes it cluster incorrently based on euclidean distance--> 
#### now for clustering:
######### Simulating data:
# a homemade function to simulate multivariate normal data
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
set.seed(1)
data<-do.call("rbind", list(multinorm(means=c(1.0,10), sds=c(.1, 1), samp_size = 30, name="group1"),
                            multinorm(means=c(1.5,10), sds=c(.1, 1), samp_size = 30, name="group2")))

# lets look at the resulting data object:
# there are 2 variables (a and b, and group which is simulated)
str(data)


#lets have a look at the data in a scatterplot between our 2 variables
ggplot(data, aes(x=a, y=b))+
  geom_point(size=2)

# lets try identifying clusters in the data by k-means clustering.
# we use the kmeans() function and need to call this
# We will first try it with the raw data, with k = 2 clusters
# we are only interested in data from the first 2 columns... data[,1:2] ...since these are 2 variable dataset (a vs b)

k_clusts<-kmeans(data[,1:2], centers=2, nstart = 25)
#lets look at the k_clusts output:
k_clusts
## numbers don't mean anyth
## object of k
str(k_clusts)
# we can assign the resulting cluster output as a column in our data so we can plot it easily
data$k_clusts_2<-factor(k_clusts$cluster)
ggplot(data, aes(x=a, y=b))+
  geom_point(aes(col=k_clusts_2, shape=group), size=2)


## not working because we did't scale our data--> this is very important for PCA and for clusters--> 
# check how well the clustering performed relative to simulated groups with table():
# Not great (missing it about half the time as we can see in the plot above)
table(data$k_clusts_2, data$group)

# that looks odd - its because we did not scale the data
# the distance in variable "a" counts for almost nothing since it is much smaller in scale
# lets consider scaled data scale() converts values to z score (mean of 0 with values equal to number of standard deviations from mean)
scale(data[,1:2])
k_clusts<-kmeans(scale(data[,1:2]), centers=2, nstart = 25)
data$k_clusts_2<-factor(k_clusts$cluster)
ggplot(data, aes(x=a, y=b))+
  geom_point(aes(col=k_clusts_2, shape=group), size=2)

# check how well the clustering performed relative to simulated groups with table():
# nice!
table(data$k_clusts_2, data$group)

# we can see what happens with 3 centers
k_clusts<-kmeans(scale(data[,1:2]), centers=3, nstart = 100)
data$k_clusts_3<-factor(k_clusts$cluster)
ggplot(data, aes(x=a, y=b))+
  geom_point(aes(col=k_clusts_3, shape=group), size=2)

# lets try with 10 centers (set nstart = 1 to see what happens when we run several times - are they identical?)
# if we change to nstart = 100, we are likely that the clusters will be the same between runs
# the more nstarts, the better chances kmeans finds the optimal solution, but at the cost of computational time
k_clusts<-kmeans(scale(data[,1:2]), centers=10, nstart = 100)
data$k_clusts_10<-factor(k_clusts$cluster)
ggplot(data, aes(x=a, y=b))+
  geom_point(aes(col=k_clusts_10, shape=group), size=2)

#### Choosing clusters: lets try simulating 3 groups
set.seed(2)
data<-do.call("rbind", list(multinorm(means=c(12,10), sds=c(1, 1), samp_size = 30, name="group1"),
                            multinorm(means=c(11,15), sds=c(1, 1), samp_size = 30, name="group2"),
                            multinorm(means=c(7,8), sds=c(1, 1), samp_size = 30, name="group3")))

ggplot(data, aes(x=a, y=b))+
  geom_point(size=2)

# we will loop through different values of k and extract the total within group sum of squares (ss)
## in looping in r--sapply loops through value 1-10

ssc <- sapply(1:10, function(k)[
  message("testing", k)])

ss<-sapply(1:10, function(k){
  out<-kmeans(scale(data[,1:2]), centers=k, nstart=25)
  out$tot.withinss
})

# we can plot total within group sum of squares as a function of k
plot(1:10, ss, type="b")
# total within group sum of squares will  decrease with more K, but the change can "flatten" out after an optimum k

# we can calculate silhouette scores with help from the the cluster package

# we want to loop through different k and calculate silhouette score:

distance<-dist(scale(data[,1:2]))
silhouette_score <- sapply(2:10, function(k){
  km <- kmeans(scale(data[,1:2]), centers=k, nstart=25)
  sil <- silhouette(km$cluster, distance)
  mean(sil[, 3])
})

plot(2:10, silhouette_score, type='b' )

#k means can identify clusters in high-dimenstional data:
set.seed(4)
# we are simulating data with two groups with 10 different dimensions of data
data<-do.call("rbind", list(multinorm(means=runif(10, min = 0, max=3), sds=rep(0.5, 10), samp_size = 100, name="group1"),
                            multinorm(means=runif(10, min = 0, max=3), sds=rep(0.5, 10), samp_size = 100, name="group2"),
                            multinorm(means=runif(10, min = 0, max=3), sds=rep(0.5, 10), samp_size = 100, name="group3"),
                            multinorm(means=runif(10, min = 0, max=3), sds=rep(0.5, 10), samp_size = 100, name="group4"),
                            multinorm(means=runif(10, min = 0, max=3), sds=rep(0.5, 10), samp_size = 100, name="group5")))

str(data)

pdf("~/Desktop/pairwise_cor.pdf", height=15, width=15)
pairs(data[,1:10])
dev.off()

ggplot(data, aes(x=a, y=c))+
  geom_point(size=2)

silhouette_score <- sapply(2:10, function(k){
  km <- kmeans(scale(data[,1:10]), centers=k, nstart=25)
  sil <- silhouette(km$cluster, dist(scale(data[,1:10])))
  mean(sil[, 3])
})

plot(2:10, silhouette_score, type='b')

# to visualize the data we can calculate principle components for the dataset with prcomp()
pca<-prcomp(data[,1:10], scale. = T)
# we can add the PC values for each observation to our original data.frame with cbind()
data<-cbind(data, pca$x)
str(data)

#plot data
ggplot(data, aes(x=PC1, y=PC2))+
  geom_point( size=2)

#plot data (with true groups from simulation labeled)
ggplot(data, aes(x=PC1, y=PC2))+
  geom_point(aes(shape=group, col=group), size=2)

# calculate K-means cluster with k=5
k_clusts<-kmeans(scale(data[,1:10]), centers=5, nstart = 10000)
#add as column
data$k_clusts_5<-factor(k_clusts$cluster)
#plot results coloring points by cluster identified and shaped by the true group
ggplot(data, aes(x=PC1, y=PC2))+
  geom_point(aes(col=k_clusts_5, shape=group), size=2)

# check accuracy with table
# Nice!
table(data$k_clusts_5, data$group)



# Mapping, geospatial, clustering in R ------------------------------------
# last example is using clustering to group locations by climate

bio <- geodata::worldclim_global(
  var  = "bio",                          # request bioclimatic variables
  res  = 2.5,                            # spatial resolution in arc-minutes
  path = "~/Dropbox/Data/geospatial_datasets" # folder where data will be stored/cached
)

# FYI: toy coordinates to extract climate values for
coords <- data.frame(
  lon = c(-75, -73),   # example longitudes
  lat = c(5,   2)      # example latitudes
)

# extract BIO values at these lon/lat points from the global 'bio' raster
coords_bio_vals <- terra::extract(bio, coords[, c("lon", "lat")])

# bio <- getData(name="worldclim", var="bio", res=2.5, path="~/Dropbox/Data/geospatial_datasets")
# previous way of downloading WorldClim bioclim data using the raster package

map <- getMap(resolution = "low")        # load a low-resolution world map (country polygons)

SPDF <- subset(map, NAME == "Colombia")  # subset the world map to only the polygon for Colombia

SPDF_vect <- vect(SPDF)                  # convert the SpatialPolygonsDataFrame to a terra SpatVector

r2 <- crop(bio, extent(SPDF))            # crop global climate rasters to the bounding box of Colombia

r3 <- mask(r2, SPDF_vect)                # mask cropped rasters so only pixels inside Colombia remain
# here we are basically extracting the country of colombia from the climate data

# we convert to a dataframe for convenience, and remove NA values (oceans for example)
colombia_climate <- as.data.frame(r3, xy = TRUE)   # convert raster stack to a data frame, keeping x/y coords

str(colombia_climate)                              # inspect structure to see columns and data types

colnames(colombia_climate) <- gsub(
  "wc2.1_2.5m_", "",
  colnames(colombia_climate)
)                                                  # clean column names by removing WorldClim prefix

colombia_climate <- colombia_climate[!is.na(colombia_climate$bio_1), ]
# drop rows with NA in bio_1 (non-land / missing values)

# we can check with str() notice there are x and y columns: these are longitude and latitude
str(colombia_climate)                              # confirm final structure; x/y are lon/lat, bio_* are climate vars


# make plots of the individual climate variables

pdf("~/Desktop/Colombia_climate.pdf", width=7, height=5)
  plots<-lapply(1:19, function(i){
p<-ggplot(colombia_climate, aes(x=x, y=y, fill=colombia_climate[, 2+i]))+
  geom_tile()+
  scale_fill_gradientn(colors=c("orange","white","blue"), name=NULL)+
  theme_void(base_size = 8)+
  coord_fixed()+
  theme(legend.key.size = unit(0.25, "cm"))+
  ggtitle(paste("bio", i))
return(p)
  }
)
  cowplot::plot_grid(plotlist = plots, nrow=4)
  dev.off()

# k=2
k_clust<-kmeans(scale(colombia_climate[,3:21]), centers=2, nstart = 100)
colombia_climate$k_clust_2<-factor(k_clust$cluster)
ggplot(colombia_climate, aes(x=x, y=y, fill=k_clust_2))+
  geom_tile()+
  scale_fill_brewer(palette = "Spectral")+
  theme_minimal()+
  coord_fixed()

# k=3
k_clust<-kmeans(scale(colombia_climate[,3:21]), centers=3, nstart = 25)
colombia_climate$k_clust_3<-factor(k_clust$cluster)
ggplot(colombia_climate, aes(x=x, y=y, fill=k_clust_3))+
  geom_tile()+
  scale_fill_brewer(palette = "Spectral")+
  theme_minimal()+
  coord_fixed()

# k=4
k_clust<-kmeans(scale(colombia_climate[,3:21]), centers=4, nstart = 25)
colombia_climate$k_clust_4<-factor(k_clust$cluster)
ggplot(colombia_climate, aes(x=x, y=y, fill=k_clust_4))+
  geom_tile()+
  scale_fill_brewer(palette = "Spectral")+
  theme_minimal()+
  coord_fixed()

# k=5
k_clust<-kmeans(scale(colombia_climate[,3:21]), centers=5, nstart = 25)
colombia_climate$k_clust_5<-factor(k_clust$cluster)
ggplot(colombia_climate, aes(x=x, y=y, fill=k_clust_5))+
  geom_tile()+
  scale_fill_brewer(palette = "Spectral")+
  theme_minimal()+
  coord_fixed()

# k=6
k_clust<-kmeans(scale(colombia_climate[,3:21]), centers=6, nstart = 25)
colombia_climate$k_clust_6<-factor(k_clust$cluster)
ggplot(colombia_climate, aes(x=x, y=y, fill=k_clust_6))+
  geom_tile()+
  scale_fill_brewer(palette = "Spectral")+
  theme_minimal()+
  coord_fixed()

# k=10
k_clust<-kmeans(scale(colombia_climate[,3:21]), centers=10, nstart =25, iter.max = 20)
colombia_climate$k_clust_10<-factor(k_clust$cluster)

#### not essential but we do this to plot the clusters according to coldest to warmest, basically we are re-ordering the cluster levels according to their average annual temperature (bio1)
temp_means<-aggregate(colombia_climate$bio_1, by=list(colombia_climate$k_clust_10), mean) # not essential but we do this to plot the clusters according to coldest to warmest
temp_means<-temp_means[order(temp_means$x, decreasing = T),]
colombia_climate$k_clust_10<-factor(colombia_climate$k_clust_10, levels=temp_means$Group.1)
###

ggplot(colombia_climate, aes(x=x, y=y, fill=k_clust_10))+
  geom_tile()+
  scale_fill_brewer(palette = "Spectral", name="Climate\ncluster")+
  theme_bw()+
  coord_fixed()+
  scale_x_continuous(name="Longitude °")+
  scale_y_continuous(name="Latitude °")


