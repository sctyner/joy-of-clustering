#looking at classes

#Hierarchical Clustering
# classification matrix
class.pcs <- data.frame(id = id.new)

# Euclidean Distance
pcs.paint.dist <- dist(pcs.paint.import[, -ncol(pcs.paint.import)])
pcs.hc.wardD <- hclust(pcs.paint.dist, method="ward.D")
plot(pcs.hc.wardD)
#from dendrogram, looks like about 8 clusters is good
class.pcs$hc.wardD.4 <- factor(cutree(pcs.hc.wardD, k = 4))
class.pcs$hc.wardD.5 <- factor(cutree(pcs.hc.wardD, k = 5))
class.pcs$hc.wardD.6 <- factor(cutree(pcs.hc.wardD, k = 6))
class.pcs$hc.wardD.7 <- factor(cutree(pcs.hc.wardD, k = 7))
class.pcs$hc.wardD.8 <- factor(cutree(pcs.hc.wardD, k = 8))
class.pcs$hc.wardD.9 <- factor(cutree(pcs.hc.wardD, k = 9))
class.pcs$hc.wardD.10 <- factor(cutree(pcs.hc.wardD, k = 10))
class.pcs$hc.wardD.11 <- factor(cutree(pcs.hc.wardD, k = 11))
class.pcs$hc.wardD.12 <- factor(cutree(pcs.hc.wardD, k = 12))
class.pcs$hc.wardD.13 <- factor(cutree(pcs.hc.wardD, k = 13)) #**
class.pcs$hc.wardD.14 <- factor(cutree(pcs.hc.wardD, k = 14))
#compare clustering groups
table(class.pcs$hc.wardD.4, class.pcs$hc.wardD.5)
table(class.pcs$hc.wardD.5, class.pcs$hc.wardD.6)
table(class.pcs$hc.wardD.6, class.pcs$hc.wardD.7) # a lot of big groups (around 50) 2 small ones (7,8 obs)
table(class.pcs$hc.wardD.7, class.pcs$hc.wardD.8) # not a fan. one obs becomes its own cluster. 
table(class.pcs$hc.wardD.8, class.pcs$hc.wardD.9) # grp 2 gets split in 2
table(class.pcs$hc.wardD.9, class.pcs$hc.wardD.10) # grp 3 gets split in 2
table(class.pcs$hc.wardD.10, class.pcs$hc.wardD.11) # grp 5 gets split in 2
table(class.pcs$hc.wardD.11, class.pcs$hc.wardD.12) # grp 9 gets split in 2
table(class.pcs$hc.wardD.12, class.pcs$hc.wardD.13) # grp 7 gets split in 2
table(class.pcs$hc.wardD.13, class.pcs$hc.wardD.14)

pcs.hc.wardD2 <- hclust(pcs.paint.dist, method="ward.D2")
plot(pcs.hc.wardD2)
class.pcs$hc.wardD2.4 <- factor(cutree(pcs.hc.wardD2, k = 4))
class.pcs$hc.wardD2.5 <- factor(cutree(pcs.hc.wardD2, k = 5))
class.pcs$hc.wardD2.6 <- factor(cutree(pcs.hc.wardD2, k = 6))
class.pcs$hc.wardD2.7 <- factor(cutree(pcs.hc.wardD2, k = 7))
class.pcs$hc.wardD2.8 <- factor(cutree(pcs.hc.wardD2, k = 8))
class.pcs$hc.wardD2.9 <- factor(cutree(pcs.hc.wardD2, k = 9))
class.pcs$hc.wardD2.10 <- factor(cutree(pcs.hc.wardD2, k = 10))
class.pcs$hc.wardD2.11 <- factor(cutree(pcs.hc.wardD2, k = 11))
class.pcs$hc.wardD2.12 <- factor(cutree(pcs.hc.wardD2, k = 12))
class.pcs$hc.wardD2.13 <- factor(cutree(pcs.hc.wardD2, k = 13))
class.pcs$hc.wardD2.14 <- factor(cutree(pcs.hc.wardD2, k = 14)) #**
class.pcs$hc.wardD2.15 <- factor(cutree(pcs.hc.wardD2, k = 15))
#compare clustering groups
table(class.pcs$hc.wardD2.4, class.pcs$hc.wardD2.5)
table(class.pcs$hc.wardD2.5, class.pcs$hc.wardD2.6)
table(class.pcs$hc.wardD2.6, class.pcs$hc.wardD2.7) # a lot of big groups (around 50) 2 small ones (7,8 obs)
table(class.pcs$hc.wardD2.7, class.pcs$hc.wardD2.8) # not a fan. one obs becomes its own cluster. 
table(class.pcs$hc.wardD2.8, class.pcs$hc.wardD2.9) # grp 2 gets split in 2
table(class.pcs$hc.wardD2.9, class.pcs$hc.wardD2.10) # grp 3 gets split in 2
table(class.pcs$hc.wardD2.10, class.pcs$hc.wardD2.11) # grp 5 gets split in 2
table(class.pcs$hc.wardD2.11, class.pcs$hc.wardD2.12) # grp 9 gets split in 2
table(class.pcs$hc.wardD2.12, class.pcs$hc.wardD2.13) # grp 7 gets split in 2
table(class.pcs$hc.wardD2.13, class.pcs$hc.wardD2.14)
#compare to ward
table(class.pcs$hc.wardD.4, class.pcs$hc.wardD2.4)
table(class.pcs$hc.wardD.5, class.pcs$hc.wardD2.5)
table(class.pcs$hc.wardD.6, class.pcs$hc.wardD2.6)
table(class.pcs$hc.wardD.7, class.pcs$hc.wardD2.7)
pcs.hc.single <- hclust(pcs.paint.dist, method="single")
plot(pcs.hc.single)
#cut tree in various places 
class.pcs$hc.single.height2 <- factor(cutree(pcs.hc.single, h = 1000))
class.pcs[class.pcs$hc.single.height2 == 4,] # these are the same in all clustering methods!!! OMG!!!!
class.pcs[class.pcs$hc.single.height2 == 4,1]
#investigate the very similar ones.
ggplot(data = paintings[[5]], aes(x = -x, y = y)) + 
  geom_tile(fill = as.character(rgb(paintings[[5]][,c('red','green','blue')],max=255))) + theme(aspect.ratio = 1)
ggplot(data = paintings[[6]], aes(x = -x, y = y)) + 
  geom_tile(fill = as.character(rgb(paintings[[6]][,c('red','green','blue')],max=255))) + theme(aspect.ratio = 1)
ggplot(data = paintings[[82]], aes(x = -x, y = y)) + 
  geom_tile(fill = as.character(rgb(paintings[[82]][,c('red','green','blue')],max=255))) + theme(aspect.ratio = 1)
ggplot(data = paintings[[107]], aes(x = -x, y = y)) + 
  geom_tile(fill = as.character(rgb(paintings[[107]][,c('red','green','blue')],max=255))) + theme(aspect.ratio = 1)
ggplot(data = paintings[[147]], aes(x = -x, y = y)) + 
  geom_tile(fill = as.character(rgb(paintings[[147]][,c('red','green','blue')],max=255))) + theme(aspect.ratio = 1)
ggplot(data = paintings[[203]], aes(x = -x, y = y)) + 
  geom_tile(fill = as.character(rgb(paintings[[203]][,c('red','green','blue')],max=255))) + theme(aspect.ratio = 1)
ggplot(data = paintings[[204]], aes(x = -x, y = y)) + 
  geom_tile(fill = as.character(rgb(paintings[[204]][,c('red','green','blue')],max=255))) + theme(aspect.ratio = 1)


# NEVERMIND. THis SUCKS
class.pcs$hc.single.4 <- factor(cutree(pcs.hc.single, k = 4))
class.pcs$hc.single.5 <- factor(cutree(pcs.hc.single, k = 5))
class.pcs$hc.single.6 <- factor(cutree(pcs.hc.single, k = 6))
class.pcs$hc.single.7 <- factor(cutree(pcs.hc.single, k = 7))
class.pcs$hc.single.8 <- factor(cutree(pcs.hc.single, k = 8)) 
class.pcs$hc.single.9 <- factor(cutree(pcs.hc.single, k = 9))
class.pcs$hc.single.10 <- factor(cutree(pcs.hc.single, k = 10))
class.pcs$hc.single.11 <- factor(cutree(pcs.hc.single, k = 11))
class.pcs$hc.single.12 <- factor(cutree(pcs.hc.single, k = 12))
class.pcs$hc.single.13 <- factor(cutree(pcs.hc.single, k = 13))
class.pcs$hc.single.14 <- factor(cutree(pcs.hc.single, k = 14))
class.pcs$hc.single.15 <- factor(cutree(pcs.hc.single, k = 15))
class.pcs$hc.single.16 <- factor(cutree(pcs.hc.single, k = 16))
class.pcs$hc.single.17 <- factor(cutree(pcs.hc.single, k = 17))
class.pcs$hc.single.18 <- factor(cutree(pcs.hc.single, k = 18))
class.pcs$hc.single.19 <- factor(cutree(pcs.hc.single, k = 19))
class.pcs$hc.single.20 <- factor(cutree(pcs.hc.single, k = 20))
class.pcs$hc.single.21 <- factor(cutree(pcs.hc.single, k = 21))
class.pcs$hc.single.height <- factor(cutree(pcs.hc.single, h = 1500))

#compare the groups
table(class.pcs$hc.single.4, class.pcs$hc.single.5)
table(class.pcs$hc.single.5, class.pcs$hc.single.6)
table(class.pcs$hc.single.6, class.pcs$hc.single.7) # a lot of big groups (around 50) 2 small ones (7,8 obs)
table(class.pcs$hc.single.7, class.pcs$hc.single.8) # not a fan. one obs becomes its own cluster. 
table(class.pcs$hc.single.8, class.pcs$hc.single.9) # grp 2 gets split in 2
table(class.pcs$hc.single.9, class.pcs$hc.single.10) # grp 3 gets split in 2
table(class.pcs$hc.single.10, class.pcs$hc.single.11) # grp 5 gets split in 2
table(class.pcs$hc.single.11, class.pcs$hc.single.12) # grp 9 gets split in 2
table(class.pcs$hc.single.12, class.pcs$hc.single.13) # grp 7 gets split in 2
table(class.pcs$hc.single.13, class.pcs$hc.single.14)
table(class.pcs$hc.single.14, class.pcs$hc.single.15)
table(class.pcs$hc.single.15, class.pcs$hc.single.16)
table(class.pcs$hc.single.16, class.pcs$hc.single.17)
table(class.pcs$hc.single.17, class.pcs$hc.single.18)
table(class.pcs$hc.single.18, class.pcs$hc.single.19)
pcs.hc.complete <- hclust(pcs.paint.dist, method="complete")
plot(pcs.hc.complete)
# i like the idea of complete clustering for this project. investigate different cluster sizes
class.pcs$hc.complete.4 <- factor(cutree(pcs.hc.complete, k = 4))
class.pcs$hc.complete.5 <- factor(cutree(pcs.hc.complete, k = 5))
class.pcs$hc.complete.6 <- factor(cutree(pcs.hc.complete, k = 6))
class.pcs$hc.complete.7 <- factor(cutree(pcs.hc.complete, k = 7)) #**
class.pcs$hc.complete.8 <- factor(cutree(pcs.hc.complete, k = 8)) # stop here. creates a cluster of only 1 obs
#class.pcs$hc.complete.9 <- factor(cutree(pcs.hc.complete, k = 9))
#class.pcs$hc.complete.10 <- factor(cutree(pcs.hc.complete, k = 10))
#class.pcs$hc.complete.11 <- factor(cutree(pcs.hc.complete, k = 11))
#class.pcs$hc.complete.12 <- factor(cutree(pcs.hc.complete, k = 12))
#class.pcs$hc.complete.13 <- factor(cutree(pcs.hc.complete, k = 13))
#class.pcs$hc.complete.14 <- factor(cutree(pcs.hc.complete, k = 14)) #don't use. creates a cluster of 2 obs
table(class.pcs$hc.complete.4, class.pcs$hc.complete.5)
which(class.pcs$hc.complete.4 == 3 & class.pcs$hc.wardD.4 == 4)
table(class.pcs$hc.complete.4, class.pcs$hc.wardD.4)
table(class.pcs$hc.complete.5, class.pcs$hc.wardD.5)
table(class.pcs$hc.complete.4, class.pcs$hc.wardD.5)
table(class.pcs$hc.complete.5, class.pcs$hc.complete.6)
table(class.pcs$hc.complete.6, class.pcs$hc.wardD.6)
table(class.pcs$hc.complete.5, class.pcs$hc.wardD.6)
table(class.pcs$hc.complete.6, class.pcs$hc.complete.7) # a lot of big groups (around 50) 2 small ones (7,8 obs)
table(class.pcs$hc.complete.7, class.pcs$hc.wardD.7)
table(class.pcs$hc.complete.7, class.pcs$hc.complete.8) # not a fan. one obs becomes its own cluster. 
table(class.pcs$hc.complete.8, class.pcs$hc.wardD.8)
#table(class.pcs$hc.complete.8, class.pcs$hc.complete.9) # grp 2 gets split in 2
#table(class.pcs$hc.complete.9, class.pcs$hc.complete.10) # grp 3 gets split in 2
#table(class.pcs$hc.complete.10, class.pcs$hc.complete.11) # grp 5 gets split in 2
#table(class.pcs$hc.complete.11, class.pcs$hc.complete.12) # grp 9 gets split in 2
#table(class.pcs$hc.complete.12, class.pcs$hc.complete.13) # grp 7 gets split in 2
#table(class.pcs$hc.complete.13, class.pcs$hc.complete.14) # 2 obs get pulled out of grp 5. stop cutting here.

pcs.hc.average <- hclust(pcs.paint.dist, method="average")
plot(pcs.hc.average)
# 220, 56, 105, 177 seem to be very different from the rest. 
# also like the average clustering idea for this data set. look at different size cluster
  # NOPE. Average clustering is TERRIBLE. 
class.pcs$hc.average.6 <- factor(cutree(pcs.hc.average, k = 6))
class.pcs$hc.average.7 <- factor(cutree(pcs.hc.average, k = 7))
class.pcs$hc.average.8 <- factor(cutree(pcs.hc.average, k = 8))
class.pcs$hc.average.9 <- factor(cutree(pcs.hc.average, k = 9))
class.pcs$hc.average.10 <- factor(cutree(pcs.hc.average, k = 10))
class.pcs$hc.average.11 <- factor(cutree(pcs.hc.average, k = 11))
class.pcs$hc.average.12 <- factor(cutree(pcs.hc.average, k = 12))
class.pcs$hc.average.13 <- factor(cutree(pcs.hc.average, k = 13))
class.pcs$hc.average.14 <- factor(cutree(pcs.hc.average, k = 14))
table(class.pcs$hc.average.6, class.pcs$hc.average.6) # is grouping way too many together
table(class.pcs$hc.average.7, class.pcs$hc.complete.7) # there is one grp of 7 (3,7) that seems to be consistently clustered together
table(class.pcs$hc.average.8, class.pcs$hc.complete.8)
table(class.pcs$hc.average.9, class.pcs$hc.complete.9)
table(class.pcs$hc.average.10, class.pcs$hc.complete.10)
table(class.pcs$hc.average.11, class.pcs$hc.complete.11)
pcs.hc.centroid <- hclust(pcs.paint.dist, method="centroid")
plot(pcs.hc.centroid)
class.pcs$hc.centroid.4 <- factor(cutree(pcs.hc.centroid, k = 4))
class.pcs$hc.centroid.5 <- factor(cutree(pcs.hc.centroid, k = 5))
class.pcs$hc.centroid.6 <- factor(cutree(pcs.hc.centroid, k = 6))
class.pcs$hc.centroid.7 <- factor(cutree(pcs.hc.centroid, k = 7))
class.pcs$hc.centroid.8 <- factor(cutree(pcs.hc.centroid, k = 8))
class.pcs$hc.centroid.9 <- factor(cutree(pcs.hc.centroid, k = 9))
class.pcs$hc.centroid.10 <- factor(cutree(pcs.hc.centroid, k = 10))
class.pcs$hc.centroid.11 <- factor(cutree(pcs.hc.centroid, k = 11))
class.pcs$hc.centroid.12 <- factor(cutree(pcs.hc.centroid, k = 12))
class.pcs$hc.centroid.13 <- factor(cutree(pcs.hc.centroid, k = 13))
class.pcs$hc.centroid.14 <- factor(cutree(pcs.hc.centroid, k = 14))
class.pcs$hc.centroid.15 <- factor(cutree(pcs.hc.centroid, k = 15))
class.pcs$hc.centroid.16 <- factor(cutree(pcs.hc.centroid, k = 16))
class.pcs$hc.centroid.17 <- factor(cutree(pcs.hc.centroid, k = 17))
class.pcs$hc.centroid.18 <- factor(cutree(pcs.hc.centroid, k = 18))
class.pcs$hc.centroid.19 <- factor(cutree(pcs.hc.centroid, k = 19))
class.pcs$hc.centroid.20 <- factor(cutree(pcs.hc.centroid, k = 20))

#compare groups
table(class.pcs$hc.centroid.4, class.pcs$hc.centroid.5)
table(class.pcs$hc.centroid.5, class.pcs$hc.centroid.6)
table(class.pcs$hc.centroid.6, class.pcs$hc.centroid.7) # a lot of big groups (around 50) 2 small ones (7,8 obs)
table(class.pcs$hc.centroid.7, class.pcs$hc.centroid.8) # not a fan. one obs becomes its own cluster. 
table(class.pcs$hc.centroid.8, class.pcs$hc.centroid.9) # grp 2 gets split in 2
table(class.pcs$hc.centroid.9, class.pcs$hc.centroid.10) # grp 3 gets split in 2
table(class.pcs$hc.centroid.10, class.pcs$hc.centroid.11) # grp 5 gets split in 2
table(class.pcs$hc.centroid.11, class.pcs$hc.centroid.12) # grp 9 gets split in 2
table(class.pcs$hc.centroid.12, class.pcs$hc.centroid.13) # grp 7 gets split in 2
table(class.pcs$hc.centroid.13, class.pcs$hc.centroid.14)

pcs.hc.median <- hclust(pcs.paint.dist, method="median")
plot(pcs.hc.median)
class.pcs$hc.median.4 <- factor(cutree(pcs.hc.median, k = 4))
class.pcs$hc.median.5 <- factor(cutree(pcs.hc.median, k = 5))
class.pcs$hc.median.6 <- factor(cutree(pcs.hc.median, k = 6))
class.pcs$hc.median.7 <- factor(cutree(pcs.hc.median, k = 7))
class.pcs$hc.median.8 <- factor(cutree(pcs.hc.median, k = 8))
class.pcs$hc.median.9 <- factor(cutree(pcs.hc.median, k = 9))
class.pcs$hc.median.10 <- factor(cutree(pcs.hc.median, k = 10))
class.pcs$hc.median.11 <- factor(cutree(pcs.hc.median, k = 11))
class.pcs$hc.median.12 <- factor(cutree(pcs.hc.median, k = 12))
class.pcs$hc.median.13 <- factor(cutree(pcs.hc.median, k = 13))
class.pcs$hc.median.14 <- factor(cutree(pcs.hc.median, k = 14))
class.pcs$hc.median.15 <- factor(cutree(pcs.hc.median, k = 15))
class.pcs$hc.median.16 <- factor(cutree(pcs.hc.median, k = 16))
class.pcs$hc.median.17 <- factor(cutree(pcs.hc.median, k = 17))
class.pcs$hc.median.18 <- factor(cutree(pcs.hc.median, k = 18))
class.pcs$hc.median.19 <- factor(cutree(pcs.hc.median, k = 19))
class.pcs$hc.median.20 <- factor(cutree(pcs.hc.median, k = 20))
#compare groups
table(class.pcs$hc.median.4, class.pcs$hc.median.5)
table(class.pcs$hc.median.5, class.pcs$hc.median.6)
table(class.pcs$hc.median.6, class.pcs$hc.median.7) # a lot of big groups (around 50) 2 small ones (7,8 obs)
table(class.pcs$hc.median.7, class.pcs$hc.median.8) # not a fan. one obs becomes its own cluster. 
table(class.pcs$hc.median.8, class.pcs$hc.median.9) # grp 2 gets split in 2
table(class.pcs$hc.median.9, class.pcs$hc.median.10) # grp 3 gets split in 2
table(class.pcs$hc.median.10, class.pcs$hc.median.11) # grp 5 gets split in 2
table(class.pcs$hc.median.11, class.pcs$hc.median.12) # grp 9 gets split in 2
table(class.pcs$hc.median.12, class.pcs$hc.median.13) # grp 7 gets split in 2
table(class.pcs$hc.median.13, class.pcs$hc.median.14)


#graphically investigating the the clusters
library(ggplot2)
merge.clusters <- merge(class.pcs, pcs.paint.import, by = 'id')
ggplot(data = merge.clusters, aes(x = X1, y = X2)) + geom_point(aes(colour = hc.wardD.4))
ggplot(data = merge.clusters, aes(x = X2, y = X3)) + geom_point(aes(colour = hc.wardD.4))
ggplot(data = merge.clusters, aes(x = X1, y = X2)) + geom_point(aes(colour = hc.wardD.5))
ggplot(data = merge.clusters, aes(x = X1, y = X2)) + geom_point(aes(colour = hc.wardD.6))
ggplot(data = merge.clusters, aes(x = X1, y = X2)) + geom_point(aes(colour = hc.wardD.7))
ggplot(data = merge.clusters, aes(x = X1, y = X2)) + geom_point(aes(colour = hc.wardD.8))
ggplot(data = merge.clusters, aes(x = X1, y = X2)) + geom_point(aes(colour = hc.wardD.9))
ggplot(data = merge.clusters, aes(x = X1, y = X2)) + geom_point(aes(colour = hc.wardD.10))
ggplot(data = merge.clusters, aes(x = X4, y = X215)) + geom_point(aes(colour = hc.wardD.11))
ggplot(data = merge.clusters, aes(x = X4, y = X215)) + geom_point(aes(colour = hc.wardD2.11))
ggplot(data = merge.clusters, aes(x = X1, y = X2)) + geom_point(aes(colour = hc.complete.4))
ggplot(data = merge.clusters, aes(x = X2, y = X3)) + geom_point(aes(colour = hc.complete.4))
ggplot(data = merge.clusters, aes(x = X1, y = X2)) + geom_point(aes(colour = hc.complete.5))
ggplot(data = merge.clusters, aes(x = X1, y = X2)) + geom_point(aes(colour = hc.complete.6))
ggplot(data = merge.clusters, aes(x = X1, y = X2)) + geom_point(aes(colour = hc.complete.7))
ggplot(data = merge.clusters, aes(x = X2, y = X3)) + geom_point(aes(colour = hc.complete.7))
ggplot(data = merge.clusters, aes(x = X220, y = X221)) + geom_point(aes(colour = hc.complete.4))
which(class.pcs$hc.complete.8 == 8)
class.pcs[220, 1]

ggparcoord(data = merge.clusters, 63:72, groupColumn = 'hc.wardD.4', alphaLines = 0.25) + theme_bw()
ggparcoord(data = merge.clusters, 63:72, groupColumn = 'hc.complete.4', alphaLines = 0.25) + theme_bw()
ggparcoord(data = merge.clusters, 63:72, groupColumn = 'hc.wardD2.4', alphaLines = 0.25) + theme_bw()
ggparcoord(data = merge.clusters, 63:72, groupColumn = 'hc.complete.12', alphaLines = 0.25) + theme_bw()
ggparcoord(data = merge.clusters, 63:72, groupColumn = 'hc.wardD2.12', alphaLines = 0.25) + theme_bw()
ggparcoord(data = merge.clusters, 63:72, groupColumn = 'hc.wardD.12', alphaLines = 0.25) + theme_bw()
ggparcoord(data = merge.clusters, 63:72, groupColumn = 'hc.average.12', alphaLines = 0.25) + theme_bw()
ggparcoord(data = merge.clusters, 63:72, groupColumn = 'hc.average.12', alphaLines = 0.25) + theme_bw()



##### Comparing the 3 best clusterings
class.pcs$hc.wardD2.14 <- factor(cutree(pcs.hc.wardD2, k = 14)) #**
class.pcs$hc.wardD.13 <- factor(cutree(pcs.hc.wardD, k = 13)) #**
class.pcs$hc.complete.7 <- factor(cutree(pcs.hc.complete, k = 7)) #**
library(fpc)
stats.wardD2 <- cluster.stats(pcs.paint.dist, as.numeric(as.character(class.pcs$hc.wardD2.14)))
stats.wardD <- cluster.stats(pcs.paint.dist, as.numeric(as.character(class.pcs$hc.wardD.13)))
stats.complete <- cluster.stats(pcs.paint.dist, as.numeric(as.character(class.pcs$hc.complete.7)))


library(plyr)
stats.hc <- ldply(stats.wardD[c(13:19,21,25:29,31:32)])
stats.hc$wardD2 <- ldply(stats.wardD2[c(13:19,21,25:29,31:32)])[,2]
stats.hc$complete <- ldply(stats.complete[c(13:19,21,25:29,31:32)])[,2]
names(stats.hc)[1:2] <- c('metric','wardD')
library(tidyr)
stats.hc2 <- gather(stats.hc[-7,], method, value, wardD:complete)
qplot(x = method, y = value, data = stats.hc2) + facet_wrap(~metric, scales = 'free_y') 
