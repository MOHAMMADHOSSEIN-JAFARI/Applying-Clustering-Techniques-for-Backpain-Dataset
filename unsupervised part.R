library(tidyverse)
library(corrplot)
library(gridExtra)
library(GGally)
library(knitr)
library(stats)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(factoextra)
library(cluster)
library(devtools)
library(ggbiplot)

### please put the csv file on your working directory

spine <-read.csv("spine.csv", header = TRUE)

spine %>%
  gather(Attributes, value, 1:12) %>%
  ggplot(aes(x=value, fill=Attributes)) +
  geom_histogram(colour="black", show.legend=FALSE, bins=20) +
  facet_wrap(~Attributes, scales="free_x") +
  labs(x="Values", y="Frequency",
       title="Spine Attributes - Histograms") +
  theme_bw()


spine %>%
  gather(Attributes, value, 1:12) %>%
  ggplot(aes(x=value, fill=Attributes)) +
  geom_density(colour="black", alpha=0.5, show.legend=FALSE) +
  facet_wrap(~Attributes, scales="free_x") +
  labs(x="Values", y="Density",
       title="Back Pain Attributes - Density plots") +
  theme_bw()





scaled_spine <-scale(spine)

scaled_spine

summary(spine)

summary(scaled_spine)

head(spine)



# Looking unspervised learning from the observations viewpoints:



##Hierarchical Clustering
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

ac <- function(x) {
  agnes(spine, method = x)$ac
}
sapply(m, ac)



ad <- function(x) {
  agnes(scaled_spine, method = x)$ac
}
sapply(m, ad)

## Dendrogram with ward scaled data:

clust <- agnes(scaled_spine, method = "ward")
pltree(clust, cex = 0.6, hang = -1, main = "Dendrogram")

## Dendrogram with ward original data:

clust <- agnes(spine, method = "ward")
pltree(clust, cex = 0.6, hang = -1, main = "Dendrogram") 

### From now on we work on scaled data: 


## The chosen linkage method can influence a lot the consequences of our clustering. 


# Dendrogram with complete linkage:
clust <- agnes(scaled_spine, method = "complete")
pltree(clust, cex = 0.6, hang = -1, main = "Dendrogram") 

# Dendrogram with average linkage:
clust <- agnes(scaled_spine, method = "average")
pltree(clust, cex = 0.6, hang = -1, main = "Dendrogram")


#Choosing the optimal number of clusters:

#The Gap Statistic Method

gap_stat <- clusGap(scaled_spine, FUN = hcut, nstart = 25, K.max = 10, B = 50)


fviz_gap_stat(gap_stat) # according to this criterion, the optimum number of clusters is 1.

distant_matrix_spine <- dist(spine, method = "euclidean")
distant_matrix_scaled_spine <- dist(scaled_spine, method = "euclidean")

## The following two groups shows how scaling can result in the number of each cluster. 
### Firstly scaled data:
final_clust <- hclust(distant_matrix_scaled_spine, method = "ward.D2" )
groups <- cutree(final_clust, k=2)
table(groups)
## original data:
final_clust <- hclust(distant_matrix_spine, method = "ward.D2" )
groups <- cutree(final_clust, k=2)
table(groups)

#### Elbow Method
fviz_nbclust(scaled_spine, hcut ,method = "wss", k.max = 24) + theme_minimal() + ggtitle("the Elbow Method")# it is 2 here

fviz_nbclust(scaled_spine, kmeans ,method = "wss", k.max = 24) + theme_minimal() + ggtitle("the Elbow Method") ###2 


#K-Means
km=kmeans(spine,2)
autoplot(km, spine, frame=TRUE)


km=kmeans(scaled_spine,2)
autoplot(km, scaled_spine, frame=TRUE)


#### The Silhouette Method
fviz_nbclust(scaled_spine,hcut , method = "silhouette", k.max = 24) + theme_minimal() + ggtitle("The Silhouette Plot")


#PCA
## Looking Unsupervised learning from the view points of Features:
pca <- prcomp(spine, center = TRUE,scale. = TRUE)
summary(pca)
#### as it can be seen,  we need more than one components to have a better understanding of the dataset. 
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)

## Biplot 
ggbiplot(pca)

# Biplot with all the entries 
ggbiplot(pca, labels=rownames(spine))












