#looking at classes

#Hierarchical Clustering
# classification matrix
class.pcs <- data.frame(orig.id = 1:242)

# Euclidean Distance
pcs.paint.dist <- dist(pcs.paint.import[, -ncol(pcs.paint.import)])
pcs.hc.wardD <- hclust(pcs.paint.dist, method="ward.D")
plot(pcs.hc.wardD)
#from dendrogram, looks like about 8 clusters is good
class.pcs$hc.wardD.8 <- factor(cutree(pcs.hc.wardD, k = 8))
pcs.hc.wardD2 <- hclust(pcs.paint.dist, method="ward.D2")
plot(pcs.hc.wardD2)
#from dendrogram, looks like about 9 clusters is good
class.pcs$hc.wardD2.9 <- factor(cutree(pcs.hc.wardD, k = 9))
pcs.hc.single <- hclust(pcs.paint.dist, method="single")
plot(pcs.hc.single)
# discovered multiple paintings! oh no!  
# get rid of the multiples and then redo pca
mults <- c(10, 2, 50, 140, 45, 34, 143, 13, 187, 238, 7, 25, 43, 49, 67, 97, 155, 210, 85, 104)
length(mults)


#from dendrogram, looks like about 9 clusters is good
class.pcs$hc.wardD2.9 <- factor(cutree(pcs.hc.wardD, k = 9))

pcs.hc.complete <- hclust(pcs.paint.dist, method="complete")
pcs.hc.average <- hclust(pcs.paint.dist, method="average")
pcs.hc.centroid <- hclust(pcs.paint.dist, method="centroid")

#For hierarchical clustering on top 225 principle components, using eulidean distance and ward's D with 9 clusters
id1 <- class.pcs$orig.id[class.pcs$hc.wardD.9 == 1]
plots1 <- NULL
for (i in 1:length(id1)){
  plots1[[i]] <- ggplot(data = paintings[[id1[i]]], aes(x = -x, y = y)) + 
  geom_tile(fill = as.character(rgb(paintings[[id1[i]]][,c('red','green','blue')],max=255))) + theme(aspect.ratio = 1)
}

id2 <- class.pcs$orig.id[class.pcs$hc.wardD.9 == 2]
plots2 <- NULL
for (i in 1:length(id2)){
  plots2[[i]] <- ggplot(data = paintings[[id2[i]]], aes(x = -x, y = y)) + 
    geom_tile(fill = as.character(rgb(paintings[[id2[i]]][,c('red','green','blue')],max=255))) + theme(aspect.ratio = 1)
}