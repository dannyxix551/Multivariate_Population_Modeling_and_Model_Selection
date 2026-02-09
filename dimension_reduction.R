library(umap)
library(Rtsne)
library(ggplot2)
library(vegan)


# load data ---------------------------------------------------------------
## dimension reduction analysis--> distance as a feature to describe multivariate data\
# used for reducing dimensions into fewer things
# minimize the euclidean distances 
## non metric multidimensioal scaling-nmds--> non-metric multidimensional scaling--> using the rank of distances rather than absolute values--> 

## build pca and use them in stat model--> 





# load the iris dataset
data(iris)
head(iris[, 1:4])

# PCoA ===================================================================
### PCoA (Principal Coordinate Analysis)
# calculate the distance matrix using the Euclidean distance

plot(iris$Sepal.Length, scale(iris$Sepal.Length))
scale(iris[, 1:4])
? dist
dist.mat <- dist(scale(iris[, 1:4]), method = "euclidean")
dist.mat

# perform PCoA
PCoA <- cmdscale(dist.mat)
PCoA_dataframe <- data.frame(PCoA, Species = iris$Species)

# plot the first two principal coordinates with color-coded samples
ggplot(PCoA_dataframe, aes(x = X1, y = X2, color = Species)) +
  geom_point() +
  theme_bw() +
  scale_y_continuous(name = "PCo 2") +
  scale_x_continuous(name = "PCo 1") +
  ggtitle("PCoA plot of iris dataset (euclidean dist)")

#compare with PCA
PCA <- prcomp(scale(iris[, 1:4]))
PCoA_PCA_dataframe <- cbind(PCoA_dataframe, PCA$x)
head(PCoA_PCA_dataframe)
# plot the first two principal coordinates with color-coded samples
ggplot(PCoA_PCA_dataframe, aes(x = PC1, y = PC2, color = Species)) +
  geom_point() +
  theme_bw() +
  scale_y_continuous(name = "PC2") +
  scale_x_continuous(name = "PC1") +
  ggtitle("PCA plot of iris dataset")

ggplot(PCoA_PCA_dataframe, aes(x = PC1, y = X1, color = Species)) +
  geom_point() +
  theme_bw() +
  scale_y_continuous(name = "PCo 1") +
  scale_x_continuous(name = "PC1") +
  ggtitle("PC1 vs PCo1")

ggplot(PCoA_PCA_dataframe, aes(x = PC2, y = X2, color = Species)) +
  geom_point() +
  theme_bw() +
  scale_y_continuous(name = "PCo 2") +
  scale_x_continuous(name = "PC12") +
  ggtitle("PC2 vs PCo2")


## now with distance method="manhattan"
dist.mat_manhattan <- dist(scale(iris[, 1:4]), method = "manhattan")

# FYI: cmdscale can return different numbers of new dimensions (default is 2, PCoA1 and PCoA2...) PCoA1, PCoA2, etc are the same regardless
PCoA_manhattan <- cmdscale(dist.mat_manhattan, k = 2)
cmdscale(dist.mat_manhattan, k = 3)
cmdscale(dist.mat_manhattan, k = 4)
## pca differences across sites?? 
##

PCoA_dataframe_manhattan <- data.frame(PCoA_manhattan, Species = iris$Species)
view(PCoA_dataframe_manhattan)
# plot the first two principal coordinates with color-coded samples
ggplot(PCoA_dataframe_manhattan, aes(x = X1, y = X2, color = Species)) +
  geom_point() +
  theme_bw() +
  scale_y_continuous(name = "PCo 2") +
  scale_x_continuous(name = "PCo 1") +
  ggtitle("PCoA plot of iris dataset (manhattan dist)")


# NMDS --------------------------------------------------------------------


# Run NMDS on the iris dataset
# We will exclude the Species column as it is a factor
iris_mds <- metaMDS(iris[, 1:4], distance = "euclidean")
str(iris_mds)
# Plot the stress
stressplot(iris_mds)

# Create a data frame for plotting
# This includes the NMDS dimensions and the species information
mds_values <- as.data.frame(scores(iris_mds)$sites)
mds_values$Species <- iris$Species

# Plot using ggplot2
ggplot(mds_values, aes(x = NMDS1, y = NMDS2, color = Species)) +
  geom_point() +
  theme_bw() +
  labs(x = "NMDS Dimension 1", y = "NMDS Dimension 2", title = "NMDS of Iris Dataset")

# Run UMAP analysis on the iris dataset
iris_umap <- umap(scale(iris[, 1:4]), n_neighbors = 10)

# Plot the results
ggplot(data.frame(iris_umap$layout, Species = iris$Species),
       aes(x = X1, y = X2, color = Species)) +
  geom_point() +
  theme_bw() +
  scale_y_continuous(name = "UMAP2") +
  scale_x_continuous(name = "UMAP1") +
  ggtitle("UMAP plot of iris dataset (n neigbors = 10)")

# Run UMAP analysis on the iris dataset with n_neighbors=20
iris_umap <- umap(scale(iris[, 1:4]), n_neighbors = 20)

# Plot the results
ggplot(data.frame(iris_umap$layout, Species = iris$Species),
       aes(x = X1, y = X2, color = Species)) +
  geom_point() +
  theme_bw() +
  scale_y_continuous(name = "UMAP2") +
  scale_x_continuous(name = "UMAP1") +
  ggtitle("UMAP plot of iris dataset (n neigbors = 20)")


# Run UMAP analysis on the iris dataset with n_neighbors=50
iris_umap <- umap(scale(iris[, 1:4]), n_neighbors = 50)

# Plot the results
ggplot(data.frame(iris_umap$layout, Species = iris$Species),
       aes(x = X1, y = X2, color = Species)) +
  geom_point() +
  theme_bw() +
  scale_y_continuous(name = "UMAP2") +
  scale_x_continuous(name = "UMAP1") +
  ggtitle("UMAP plot of iris dataset (n neigbors = 50)")


# Run t-SNE analysis on the iris dataset
set.seed(1)
iris_tsne <- Rtsne(
  scale(iris[, 1:4]),
  dims = 2,
  perplexity = 10,
  check_duplicates = FALSE
)

# Plot the results
ggplot(data.frame(iris_tsne$Y, Species = iris$Species),
       aes(x = X1, y = X2, color = Species)) +
  geom_point() +
  theme_bw() +
  scale_y_continuous(name = "t-SNE 2") +
  scale_x_continuous(name = "t-SNE 1") +
  ggtitle("t-SNE plot of iris dataset (perplexity = 10)")

# with different seed...
set.seed(10)
iris_tsne <- Rtsne(
  scale(iris[, 1:4]),
  dims = 2,
  perplexity = 10,
  check_duplicates = FALSE
)

# Plot the results
ggplot(data.frame(iris_tsne$Y, Species = iris$Species),
       aes(x = X1, y = X2, color = Species)) +
  geom_point() +
  theme_bw() +
  scale_y_continuous(name = "t-SNE 2") +
  scale_x_continuous(name = "t-SNE 1") +
  ggtitle("t-SNE plot of iris dataset (perplexity = 10)\nDifferent seed")


# perplexity = 2
set.seed(1)
iris_tsne <- Rtsne(
  scale(iris[, 1:4]),
  dims = 2,
  perplexity = 2,
  check_duplicates = FALSE
)
iris_tsne <- Rtsne(
  scale(iris[, 1:4]),
  dims = 2,
  perplexity = 2,
  check_duplicates = FALSE
)

# Plot the results
ggplot(data.frame(iris_tsne$Y, Species = iris$Species),
       aes(x = X1, y = X2, col = Species)) +
  geom_point() +
  theme_bw() +
  scale_y_continuous(name = "t-SNE 2") +
  scale_x_continuous(name = "t-SNE 1") +
  ggtitle("t-SNE plot of iris dataset (perplexity = 2)")

### examples with cassava dataset
cassava <- read.csv("Week 6/cassava.csv")

#t-SNE example
cassava_tsne <- Rtsne(
  scale(cassava[, 1:19]),
  dims = 2,
  perplexity = 50,
  check_duplicates = FALSE
)

# Plot the results
ggplot(
  data.frame(cassava_tsne$Y, bio1 = cassava$bio1, bio12 = cassava$bio12),
  aes(x = X1, y = X2, col = bio1)
) +
  geom_point() +
  theme_bw() +
  scale_y_continuous(name = "t-SNE 2") +
  scale_x_continuous(name = "t-SNE 1") +
  scale_color_gradient(low = "orange", high = "blue") +
  ggtitle("t-SNE plot of cassava dataset (perplexity = 5)")

# UMAP example
cassava_umap <- umap(scale(cassava[, 1:19]), n_neighbors = 100)

# Plot the results
ggplot(
  data.frame(
    cassava_umap$layout,
    bio1 = cassava$bio1,
    bio12 = cassava$bio12
  ),
  aes(x = X1, y = X2, color = bio1)
) +
  geom_point() +
  theme_bw() +
  scale_y_continuous(name = "UMAP2") +
  scale_x_continuous(name = "UMAP1") +
  scale_color_gradient(low = "orange", high = "blue") +
  ggtitle("UMAP plot of cassava dataset (n neigbors = 100)")

climate_variables <- cassava[, 1:19]
# compare to PCA... which is makes the most sense?
pca_climate <- prcomp(climate_variables, scale = T, center = T)
summary(pca_climate)
# we can extract our PC coordinates for our observations
cassava$pc1 <- pca_climate$x[, 1]
cassava$pc2 <- pca_climate$x[, 2]

ggplot(cassava, aes(x = pc1, y = pc2, col = bio1)) +
  geom_point() +
  theme_bw() +
  scale_color_gradient(low = "orange", high = "blue") +
  scale_y_continuous(name = "PC2") +
  scale_x_continuous(name = "PC1") +
  ggtitle("PCA plot of cassava dataset")

ggplot(cassava, aes(x = pc1, y = pc2, col = bio12)) +
  geom_point() +
  theme_bw() +
  scale_color_gradient(low = "orange", high = "blue") +
  scale_y_continuous(name = "PC2") +
  scale_x_continuous(name = "PC1") +
  ggtitle("PCA plot of cassava dataset")
