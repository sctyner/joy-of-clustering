#get bob ross painting image files from http://www.saleoilpaintings.com/paintings/bob-ross/

library(rvest)
paintings <- html("http://www.saleoilpaintings.com/paintings/bob-ross/bob-ross-sale-3_1.html")
images <- paintings %>% html_nodes("img") %>% html_attr("src") 
remove <- which(images == "/image/buy-art.gif")  #logo for website repeated over and over
paintings2 <- images[-remove]  #get rid of logo
img_urls <- paste("http://www.saleoilpaintings.com",paintings2,sep='')
img_urls2 <- img_urls[grep(".jpg",img_urls)]  #only get jpgs, not gifs
# for (i in 1:length(img_urls2)){
#   destination <- paste("~/Desktop/503/Project/paintings/bobross",i,".jpg",sep='')
#     download.file(url = img_urls2[i], destfile = destination)
# }


#after running python code, read all the txt files into R
library(plyr)
paintings <- NULL
for (i in 1:242){
  file.name <- paste("~/Desktop/503/Project/txtfiles/bobross",i,".txt", sep='')
  text.file <- read.table(file.name, stringsAsFactors=F)
  text.file[,1] <- gsub(":","",text.file[,1])
  coords <- ldply(strsplit(text.file[,1], ","))
  names(coords) <- c('x', 'y')
  coords$x <- as.numeric(coords$x)
  coords$y <- as.numeric(coords$y)
  text.file[,2] <- gsub("\\)", "", gsub("\\(", "", text.file[,2]))
  colors <- ldply(strsplit(text.file[,2], ","))
  names(colors) <- c('red', 'green', 'blue')
  colors$red <- as.numeric(colors$red)
  colors$green <- as.numeric(colors$green)
  colors$blue <- as.numeric(colors$blue)
  paintings[[i]] <- data.frame(coords, colors)
}
paintings2 <- ldply(paintings)
library(ggplot2)
ggplot(data = paintings[[1]], aes(x = -x, y = y)) + 
  geom_tile(fill = as.character(rgb(paintings[[1]][,c('red','green','blue')],max=255))) + theme(aspect.ratio = 1)
ggplot(data = paintings[[40]], aes(x = -x, y = y)) + 
  geom_tile(fill = as.character(rgb(paintings[[40]][,c('red','green','blue')],max=255))) + theme(aspect.ratio = 1)

#gather all paintings into a data frame with 1201 columns. (400*3 for each pixel color value + 1 for id)
paint.df <- data.frame(matrix(0, ncol = 1+400*3, nrow = 242))
column.names <- paste(rep(1:400,each=3),c('r','g','b'),sep='')  # 1200 variables. p >> n
names(paint.df) <- c('id', column.names)

for (id in 1:242){
  for (i in 1:400){
    paint.df[id, 1+(3*(i-1)+1)] <- paintings[[id]][i,3]
    paint.df[id, 1+(3*(i-1)+2)] <- paintings[[id]][i,4]
    paint.df[id, 1+(3*(i-1)+3)] <- paintings[[id]][i,5]
  }
}

paint.df$id <- 1:242

#write.csv(paint.df, file = "~/Desktop/503/Project/clean_paint_data.csv")

#Singular Value Decomposition (for Dimension Reduction)
svd.paint <- svd(paint.df[,-1])
# columns of svd.paint$v are the principal component directions
# z_j = X*v_j are the principal components. these are the cols of the mat below
pcs.paint <- apply(svd.paint$v, 2, function(x) as.matrix(paint.df[,-1])%*%x)
#find the most important PC:
summary(svd.paint$d)
#starting at 226, all are essentially 0. 
pcs.paint.import <- pcs.paint[,1:225]
# cluster on the principal components.
pcs.paint.import <- data.frame(pcs.paint.import)
pcs.paint.import$id <- 1:242

#Hierarchical Clustering
# classification matrix
class.pcs <- data.frame(orig.id = 1:242)

# Euclidean Distance
pcs.paint.dist <- dist(pcs.paint.import[, -226])
pcs.hc.wardD <- hclust(pcs.paint.dist, method="ward.D")
plot(pcs.hc.wardD)
class.pcs$hc.wardD.9 <- factor(cutree(pcs.hc.wardD, k = 9))
pcs.hc.wardD2 <- hclust(pcs.paint.dist, method="ward.D2")
pcs.hc.single <- hclust(pcs.paint.dist, method="single")
pcs.hc.complete <- hclust(pcs.paint.dist, method="complete")
pcs.hc.average <- hclust(pcs.paint.dist, method="average")
pcs.hc.centroid <- hclust(pcs.paint.dist, method="centroid")