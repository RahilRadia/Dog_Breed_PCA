library(data.table)
library(Rtsne)
library(ggplot2)
library(caret)
library(ClusterR)

set.seed(3)

# load in data 
data<-fread("./project/volume/data/raw/data.csv")

id<-data$id
data$id<-NULL


#Initial PCA
pca<-prcomp(data)

#percent variance 
screeplot(pca)

#Summary of PCA
summary(pca)

# see a biplot of the first 2 PCs
biplot(pca)

# use the unclass() function to get the data in PCA space
pca_dt<-data.table(unclass(pca)$x)



# TSNE
set.seed(3)
tsne<-Rtsne(pca_dt,pca = F,perplexity=25,check_duplicates = F)

# coordinates
tsne_dt<-data.table(tsne$Y)
ggplot(tsne_dt,aes(x=V1,y=V2))+geom_point()


#Run model with k = 4 since we know there are 4 breeds in the dataset and therefore 4 clusters as seen in the plot
opt_k<-4
gmm_data<-GMM(tsne_dt[,.(V1,V2)],opt_k)

# log-likelihood to probability
l_clust<-gmm_data$Log_likelihood^10
l_clust<-data.table(l_clust)
net_lh<-apply(l_clust,1,FUN=function(x){sum(1/x)})
cluster_prob<-1/l_clust/net_lh

# we can now plot to see clusters
tsne_dt$Cluster_1_prob<-cluster_prob$V1
tsne_dt$Cluster_2_prob<-cluster_prob$V2
tsne_dt$Cluster_3_prob<-cluster_prob$V3
tsne_dt$Cluster_4_prob<-cluster_prob$V4

ggplot(tsne_dt,aes(x=V1,y=V2,col=Cluster_2_prob))+geom_point()

#Identify cluster probabilities based on given breeds for samples 1, 5, 6
breed_1 <- cluster_prob$V1
breed_2 <- cluster_prob$V2
breed_3 <- cluster_prob$V4
breed_4 <- cluster_prob$V3

#Format to submission file
submission<-data.frame(id, breed_1, breed_2, breed_3, breed_4)
fwrite(submission, "./project/volume/data/processed/submission.csv")