# next try k means
#data is the pcas. pcs.paint.import[, -ncol(pcs.paint.import)]
pcs.km.4 <- kmeans(pcs.paint.import[, -ncol(pcs.paint.import)], 4)
class.pcs$pcs.km.4 <- as.factor(pcs.km.4$cluster)
pcs.km.5 <- kmeans(pcs.paint.import[, -ncol(pcs.paint.import)], 5)
class.pcs$pcs.km.5 <- as.factor(pcs.km.5$cluster)
pcs.km.6 <- kmeans(pcs.paint.import[, -ncol(pcs.paint.import)], 6)
class.pcs$pcs.km.6 <- as.factor(pcs.km.6$cluster)
pcs.km.7 <- kmeans(pcs.paint.import[, -ncol(pcs.paint.import)], 7)
class.pcs$pcs.km.7 <- as.factor(pcs.km.7$cluster)
pcs.km.8 <- kmeans(pcs.paint.import[, -ncol(pcs.paint.import)], 8)
class.pcs$pcs.km.8 <- as.factor(pcs.km.8$cluster)
pcs.km.9 <- kmeans(pcs.paint.import[, -ncol(pcs.paint.import)], 9)
class.pcs$pcs.km.9 <- as.factor(pcs.km.9$cluster)
pcs.km.10 <- kmeans(pcs.paint.import[, -ncol(pcs.paint.import)], 10)
class.pcs$pcs.km.10 <- as.factor(pcs.km.10$cluster)
pcs.km.11 <- kmeans(pcs.paint.import[, -ncol(pcs.paint.import)], 11)
class.pcs$pcs.km.11 <- as.factor(pcs.km.11$cluster)
pcs.km.12 <- kmeans(pcs.paint.import[, -ncol(pcs.paint.import)], 12)
class.pcs$pcs.km.12 <- as.factor(pcs.km.12$cluster)
pcs.km.12$size
pcs.km.13 <- kmeans(pcs.paint.import[, -ncol(pcs.paint.import)], 13)
class.pcs$pcs.km.13 <- as.factor(pcs.km.13$cluster)
pcs.km.13$size
pcs.km.14 <- kmeans(pcs.paint.import[, -ncol(pcs.paint.import)], 14)
class.pcs$pcs.km.14 <- as.factor(pcs.km.14$cluster)
pcs.km.14$size
pcs.km.15 <- kmeans(pcs.paint.import[, -ncol(pcs.paint.import)], 15)
class.pcs$pcs.km.15 <- as.factor(pcs.km.15$cluster)
pcs.km.15$size
pcs.km.16 <- kmeans(pcs.paint.import[, -ncol(pcs.paint.import)], 16)
class.pcs$pcs.km.16 <- as.factor(pcs.km.16$cluster)
pcs.km.16$size
pcs.km.17 <- kmeans(pcs.paint.import[, -ncol(pcs.paint.import)], 17)
class.pcs$pcs.km.17 <- as.factor(pcs.km.17$cluster)
pcs.km.17$size

pcs.km.25 <- kmeans(pcs.paint.import[, -ncol(pcs.paint.import)], 25)

pcs.km.25$size

#from class
library(fpc)
ncl <- NULL
for (i in 4:2) {
  df.km <- kmeans(pcs.paint.import[, -ncol(pcs.paint.import)], i)
  ncl <- rbind(ncl, c(i, cluster.stats(dist(pcs.paint.import[, -ncol(pcs.paint.import)]), df.km$cluster)$wb.ratio, 
                      cluster.stats(dist(pcs.paint.import[, -ncol(pcs.paint.import)]), df.km$cluster)$average.between, 
                      cluster.stats(dist(pcs.paint.import[, -ncol(pcs.paint.import)]), df.km$cluster)$average.within, 
                      cluster.stats(dist(pcs.paint.import[, -ncol(pcs.paint.import)]), df.km$cluster)$max.diameter, 
                      cluster.stats(dist(pcs.paint.import[, -ncol(pcs.paint.import)]), df.km$cluster)$min.separation, 
                      cluster.stats(dist(pcs.paint.import[, -ncol(pcs.paint.import)]), df.km$cluster)$within.cluster.ss, 
                      cluster.stats(dist(pcs.paint.import[, -ncol(pcs.paint.import)]), df.km$cluster)$avg.silwidth, 
                      cluster.stats(dist(pcs.paint.import[, -ncol(pcs.paint.import)]), df.km$cluster)$pearsongamma, 
                      cluster.stats(dist(pcs.paint.import[, -ncol(pcs.paint.import)]), df.km$cluster)$entropy, 
                      cluster.stats(dist(pcs.paint.import[, -ncol(pcs.paint.import)]), df.km$cluster)$ch, cluster.stats(dist(pcs.paint.import[, -ncol(pcs.paint.import)]), df.km$cluster)$widest.gap, 
                      cluster.stats(dist(pcs.paint.import[, -ncol(pcs.paint.import)]), df.km$cluster)$sindex, 
                      cluster.stats(dist(pcs.paint.import[, -ncol(pcs.paint.import)]), df.km$cluster)$noisen))
}
colnames(ncl) <- c("k", "wb.ratio", "avg.bw", "avg.win", "max.diam", "min.sep", "ss.win", "persongamma", "entropy", "ch", "wide.gap", "sindex", "noisen")
ncl <- data.frame(ncl)
qplot(k, wb.ratio, data=ncl, geom="line") + theme_bw() + labs(title = 'Statistic: wb.ratio')
# Re-do this with different cluster statistics
qplot(k, avg.bw, data=ncl, geom="line") + theme_bw() + labs(title = 'Statistic: average between')
qplot(k, avg.win, data=ncl, geom="line") + theme_bw() + labs(title = 'Statistic: average within')
qplot(k, max.diam, data=ncl, geom="line") + theme_bw() + labs(title = 'Statistic: max diameter')
qplot(k, min.sep, data=ncl, geom="line") + theme_bw() + labs(title = 'Statistic: minimum separation')
qplot(k, entropy, data=ncl, geom="line") + theme_bw() + labs(title = 'Statistic: entropy')
qplot(k, ss.win, data=ncl, geom="line") + theme_bw() + labs(title = 'Statistic: ss within')
qplot(k, persongamma, data=ncl, geom="line") + theme_bw() + labs(title = 'Statistic: pearsongamma')
qplot(k, wide.gap, data=ncl, geom="line") + theme_bw() + labs(title = 'Statistic: widest gap')
qplot(k, noisen, data=ncl, geom="line") + theme_bw() + labs(title = 'Statistic: noisen')
qplot(k, sindex, data=ncl, geom="line") + theme_bw() + labs(title = 'Statistic: sindex')







pcs.km.10$tot.withinss

class.pcs[class.pcs$hc.single.height2 == 4,]

#compare groups
table(class.pcs$pcs.km.4, class.pcs$pcs.km.5)
table(class.pcs$pcs.km.5, class.pcs$pcs.km.6)
table(class.pcs$pcs.km.6, class.pcs$pcs.km.7)
table(class.pcs$pcs.km.7, class.pcs$pcs.km.8)
table(class.pcs$pcs.km.8, class.pcs$pcs.km.9)
table(class.pcs$pcs.km.9, class.pcs$pcs.km.10)
table(class.pcs$pcs.km.10, class.pcs$pcs.km.11)
table(class.pcs$pcs.km.11, class.pcs$pcs.km.12)
table(class.pcs$pcs.km.12, class.pcs$pcs.km.13)
table(class.pcs$pcs.km.13, class.pcs$pcs.km.14)

merge.clusters <- merge(class.pcs, pcs.paint.import, by = 'id')
ggplot(data = merge.clusters, aes(x = X1, y = X2)) + geom_point(aes(colour = pcs.km.4))
ggplot(data = merge.clusters, aes(x = X1, y = X2)) + geom_point(aes(colour = pcs.km.5))
ggplot(data = merge.clusters, aes(x = X1, y = X2)) + geom_point(aes(colour = pcs.km.6))
ggplot(data = merge.clusters, aes(x = X1, y = X2)) + geom_point(aes(colour = pcs.km.7))
ggplot(data = merge.clusters, aes(x = X1, y = X2)) + geom_point(aes(colour = pcs.km.8))
ggplot(data = merge.clusters, aes(x = X1, y = X2)) + geom_point(aes(colour = pcs.km.9))
ggplot(data = merge.clusters, aes(x = X1, y = X2)) + geom_point(aes(colour = pcs.km.10))
ggplot(data = merge.clusters, aes(x = X1, y = X2)) + geom_point(aes(colour = pcs.km.11))

library(GGally)
ggparcoord(data = merge.clusters, 109:128, groupColumn = 'pcs.km.4', alphaLines=.7) + theme_bw() + facet_wrap(~pcs.km.4)
ggparcoord(data = merge.clusters, 109:118, groupColumn = 'pcs.km.4', alphaLines=.75) + theme_bw() + facet_wrap(~pcs.km.4)
ggparcoord(data = merge.clusters, 109:113, groupColumn = 'pcs.km.4', alphaLines=.75) + theme_bw() 
ggparcoord(data = merge.clusters, 109:128, groupColumn = 'pcs.km.5', alphaLines=.75) + theme_bw() + facet_wrap(~pcs.km.5)
ggparcoord(data = merge.clusters, 300:310, groupColumn = 'pcs.km.6', alphaLines=.75) + theme_bw() + facet_wrap(~pcs.km.6)
ggparcoord(data = merge.clusters, 300:310, groupColumn = 'pcs.km.7', alphaLines=.75) + theme_bw() + facet_wrap(~pcs.km.7)
ggparcoord(data = merge.clusters, 300:310, groupColumn = 'pcs.km.8', alphaLines=.75) + theme_bw() + facet_wrap(~pcs.km.8)
ggparcoord(data = merge.clusters, 300:310, groupColumn = 'pcs.km.9', alphaLines=.75) + theme_bw() + facet_wrap(~pcs.km.9)
ggparcoord(data = merge.clusters, 300:310, groupColumn = 'pcs.km.10', alphaLines=.75) + theme_bw() + facet_wrap(~pcs.km.10)
ggparcoord(data = merge.clusters, 300:310, groupColumn = 'pcs.km.11', alphaLines=.75) + theme_bw() + facet_wrap(~pcs.km.11)
ggparcoord(data = merge.clusters, 300:310, groupColumn = 'pcs.km.12', alphaLines=.75) + theme_bw() + facet_wrap(~pcs.km.12)
ggparcoord(data = merge.clusters, 300:310, groupColumn = 'pcs.km.13', alphaLines=.75) + theme_bw() + facet_wrap(~pcs.km.13)
ggparcoord(data = merge.clusters, 300:310, groupColumn = 'pcs.km.14', alphaLines=.75) + theme_bw() + facet_wrap(~pcs.km.14)

#comparing k means to others
table(class.pcs$hc.wardD.4, class.pcs$pcs.km.4)
table(class.pcs$hc.wardD.5, class.pcs$pcs.km.5)
table(class.pcs$hc.wardD.6, class.pcs$pcs.km.6)
