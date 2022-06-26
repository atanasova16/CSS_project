bul <- readxl::read_xlsx("C:/Users/User/Documents/UNITN/Computational social science/bulgaria/final_CITIES.xlsx")

bul$activity_rate <- as.double(bul$activity_rate)
bul$labour_force_thousands <- as.double(bul$labour_force_thousands)
bul$employment_rate <- as.double(bul$employment_rate)
bul$foreign_direct_investment_euro <- as.double(bul$foreign_direct_investment_euro)

apply(bul[,-1], 2, mean)
plot(apply(bul[,-1], 2, mean))


pr.out <- prcomp(bul[,-1] , scale = TRUE)
plot(pr.out)
pr.out$rotation
dim(pr.out$x)
biplot(pr.out , scale = 0)

pr.out$rotation = -pr.out$rotation
pr.out$x = -pr.out$x
biplot(pr.out , scale = 0)

pr.out$sdev

#The variance explained by each principal component is obtained by squaring
#these:
pr.var <- pr.out$sdev^2
pr.var
#explained var
pve <- pr.var / sum(pr.var)
pve

par(mfrow = c(1, 2))
plot(pve , xlab = "Principal Component",
       ylab = "Proportion of Variance Explained", ylim = c(0, 1),
       type = "b")
plot(cumsum(pve), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       ylim = c(0, 1), type = "b")

#s
#las <- glmnet::glmnet(Population2021 ~ .-Region, alpha = 1)

par(mfrow = c(1,1))

comp <- data.frame(pr.out$x[,1:5])
# Plot
plot(comp, pch=16, col=rgb(0,0,0,0.5))

#2-D projections of data which are in a 4-D space. 
#You can see there's a clear outlier in all the dimensions, as well as some bunching together in the different projections.

summary(pr.out)
bul_transform <- as.data.frame(-pr.out$x[,1:5])

fviz_nbclust(bul_transform, kmeans, method = 'wss')
fviz_nbclust(bul_transform, kmeans, method = 'silhouette')
fviz_nbclust(bul_transform, kmeans, method = 'gap_stat')


k = 4
kmeans_bul = kmeans(bul_transform, centers = k, nstart = 20)
fviz_cluster(kmeans_bul, data = bul_transform)


k <- kmeans(comp, 4, nstart=25, iter.max=1000)
library(RColorBrewer)
library(scales)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(comp, col=k$clust, pch=16)

clusters_kmeans4 <- k$cluster


bul_with_labels <- bul[,]
bul_with_labels$kmeans4 <- as.factor(clusters_kmeans4)

scaled <- as.data.frame(scale(bul_with_labels[,-c(1,30)]))
scaled$Region <- bul$Region
scaled$kmeans4 <- as.factor(clusters_kmeans4)


library(randomForest)
rf_bul <- randomForest(kmeans4 ~.-Region, data = scaled, mtry = 6, importance = TRUE)
importance(rf_bul)
varImpPlot(rf_bul)


boxplot(bul_with_labels$connected_to_wastewater_collecting ~ bul_with_labels$kmeans4,
        xlab='Cluster', ylab='connected_to_wastewater_collecting',
        main='connected_to_wastewater_collecting by Cluster')
boxplot(bul$employment_rate ~ bul_with_labels$kmeans4,
        xlab='Cluster', ylab='employment_rate',
        main='employment_rate by Cluster')

#HCLUST WITHOUT PCA
distmatrix <- dist(scaled[,-c(29,30)])
hc <- hclust(distmatrix, method = "complete")
plot(hc, labels = scaled$Region)
labs_hc4 <- cutree(hc, 4)

#HCLUST WITH PCA
distmatrix2 <- dist(bul_transform)
hc2 <- hclust(distmatrix2, method = "complete")
plot(hc2, labels = scaled$Region)
labs_pca_hc4 <- cutree(hc, 4)

#Compare hclust with kmeans both with the usage of PCA
#in hclust, we can change 1 and 2 to correspond to the kmeans
i = labs_pca_hc4 == 1
j = labs_pca_hc4 == 2
labs_pca_hc4[i] = 2
labs_pca_hc4[j] = 1
labs_pca_hc4 == clusters_kmeans4
sum(labs_pca_hc4 == clusters_kmeans4)
table(labs_pca_hc4, clusters_kmeans4)
bul_with_labels$labs_pca_hc4 <- as.factor(labs_pca_hc4)

#To see which cities are classified differently
bul_with_labels$Region[bul_with_labels$kmeans4 == 1 & bul_with_labels$labs_pca_hc4 == 2]

#WHY???


write.csv(bul_with_labels, "C:/Users/User/Documents/UNITN/Computational social science/bulgaria/bul_with_labels.csv")


