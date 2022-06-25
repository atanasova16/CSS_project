bul <- readxl::read_xlsx("C:/Users/User/Documents/UNITN/Computational social science/bulgaria/final_cities.xlsx")

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


k = 5
kmeans_bul = kmeans(bul_transform, centers = k, nstart = 50)
fviz_cluster(kmeans_bul, data = bul_transform)


k <- kmeans(comp, 4, nstart=25, iter.max=1000)
library(RColorBrewer)
library(scales)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(comp, col=k$clust, pch=16)

boxplot(bul$Population2021~ k$cluster,
        xlab='Cluster', ylab='Accommodation',
        main='Plane Accommodation by Cluster')

boxplot(bul$employment_rate~ k$cluster,
        xlab='Cluster', ylab='Accommodation',
        main='Plane Accommodation by Cluster')
k$cluster

