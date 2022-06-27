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


par(mfrow = c(1,1))

comp <- data.frame(pr.out$x[,1:5])

write.csv(comp, "C:/Users/User/Documents/UNITN/Computational social science/bulgariafirst5PCs.csv")

# Plot
plot(comp, pch=16, col=rgb(0,0,0,0.5))

#2-D projections of data which are in a 4-D space. 
#You can see there's a clear outlier in all the dimensions, as well as some bunching together in the different projections.

summary(pr.out)
bul_transform <- as.data.frame(-pr.out$x[,1:5])

library(factoextra)
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
#distmatrix <- dist(scaled[,-c(29,30)])
#hc <- hclust(distmatrix, method = "complete")
#plot(hc, labels = scaled$Region)
#labs_hc4 <- cutree(hc, 4)

#HCLUST WITH PCA
distmatrix2 <- dist(bul_transform)
hc2 <- hclust(distmatrix2, method = "complete")
plot(hc2, labels = scaled$Region)
labs_pca_hc4 <- cutree(hc, 4)

rownames(scaled) <- scaled$Region
rownames(bul_transform) <- scaled$Region
#hc2$labels <- as.vector(bul_with_labels[hc2$order, 'Region'])
fviz_dend(hc2,cex = 0.5, k = 4,rect = TRUE)

#See var importance when hclust
rf_bul2 <- randomForest(labs_pca_hc4 ~.-Region - kmeans4, data = scaled, mtry = 6, importance = TRUE)
importance(rf_bul2)
varImpPlot(rf_bul2)


#Compare hclust with kmeans both with the usage of PCA
#in hclust, we can change 1 and 2 to correspond to the kmeans
#TODO! each time they differ!!!
i = labs_pca_hc4 == 1
j = labs_pca_hc4 == 2
labs_pca_hc4[i] = 2
labs_pca_hc4[j] = 1
labs_pca_hc4 == clusters_kmeans4
sum(labs_pca_hc4 == clusters_kmeans4)
table(labs_pca_hc4, clusters_kmeans4)
bul_with_labels$labs_pca_hc4 <- as.factor(labs_pca_hc4)


#ARI, but first align the clusters labels!!
mclust::adjustedRandIndex(clusters_kmeans4, labs_pca_hc4)
#describe what it is and how it is calculated



scaled$labs_pca_hc4 <- as.factor(labs_pca_hc4)

par(mfrow = c(1,2))

boxplot(bul_with_labels$connected_to_wastewater_collecting ~ bul_with_labels$kmeans4,
        xlab='Cluster', ylab='connected_to_wastewater_collecting',
        main='connected_to_wastewater_collecting by Cluster kmeans')
boxplot(bul_with_labels$connected_to_wastewater_collecting ~ bul_with_labels$labs_pca_hc4,
        xlab='Cluster', ylab='connected_to_wastewater_collecting',
        main='connected_to_wastewater_collecting by Cluster hclust')

boxplot(bul$employment_rate ~ bul_with_labels$kmeans4,
        xlab='Cluster', ylab='employment_rate',
        main='employment_rate by Cluster kmeans')

boxplot(bul$employment_rate ~ bul_with_labels$labs_pca_hc4,
        xlab='Cluster', ylab='employment_rate',
        main='employment_rate by Cluster hclust')

boxplot(bul$activity_rate ~ bul_with_labels$kmeans4,
        xlab='Cluster', ylab='activity_rate',
        main='activity_rate by Cluster kmeans')

boxplot(bul$activity_rate ~ bul_with_labels$labs_pca_hc4,
        xlab='Cluster', ylab='activity_rate',
        main='activity_rate by Cluster hclust')


par(mfrow = c(1,1))

ggplot(bul_with_labels, aes(Population2021, colour = kmeans4)) +
        geom_density()

ggplot(bul_with_labels, aes(x = kmeans4, y = Population2021)) +
        geom_bar(
                aes(color = Region, fill = Region),
                stat = "summary", position = position_stack()
        ) +
        theme_classic()

ggplot(bul_with_labels, aes(x = kmeans4, y = activity_rate)) +
        geom_bar(
                aes(fill = kmeans4),
                #aes(color = Region, fill = Region),
                stat = "summary", position = position_stack()
        ) +
        theme_classic()

ggplot(bul_with_labels, aes(x = kmeans4, y = connected_to_wastewater_collecting)) +
        geom_bar(
                aes(fill = kmeans4),
                #aes(color = Region, fill = Region),
                stat = "summary", position = position_stack()
        ) +
        theme_classic()


#To see which cities are classified differently
bul_with_labels$Region[bul_with_labels$kmeans4 == 1 & bul_with_labels$labs_pca_hc4 == 2]

#WHY???



#Trying lasso regression on one of the most important variables for both kmeans and hclust: employment rate!
library(glmnet)

y <- scaled$employment_rate

tr <- sample(1:nrow(scaled), 0.8*nrow(scaled))
ts <- -tr
y_test <- y[ts]
y_train <- y[tr]

x <- model.matrix(employment_rate ~ .-Region-labs_pca_hc4 - kmeans4, data = scaled)[,-c(16,29,30,31)]

x_train <- model.matrix(employment_rate ~ .-Region-labs_pca_hc4 - kmeans4, data = scaled[tr,])[,-c(16,29,30,31)]
x_test <- model.matrix(employment_rate ~ .-Region-labs_pca_hc4 - kmeans4, data = scaled[ts,])[,-c(16,29,30,31)]


cv.las <- cv.glmnet(x_train, y_train, alpha = 1, nfolds = 5)

las <- glmnet(x_train, y_train, alpha = 1)

plot(cv.las)
bestlam = cv.las$lambda.min

las_predict <- predict(las, s = bestlam, newx = x_test)
mean((las_predict - y_test)^2) #test error

#Fit lasso on full dataset
out <- glmnet(x, y, alpha = 1)
las_pred <- predict(out, type = "coefficients", s = bestlam)[1:28,]
las_pred

las_pred[las_pred != 0] # These are the most important variables
#Again we see that wastewater is very important

plot(out, xvar = "lambda", label = TRUE)


#write.csv(bul_with_labels, "C:/Users/User/Documents/UNITN/Computational social science/bulgaria/bul_with_labels.csv")


