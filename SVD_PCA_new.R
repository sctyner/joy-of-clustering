#PCA on the data set with the multiples removed. 
id.new <- c(1:242)[-mults]

svd.paint <- svd(paint.df.cent[-mults,-1])
# columns of svd.paint$v are the principal component directions
# z_j = X*v_j are the principal components. these are the cols of the mat below
pcs.paint <- apply(svd.paint$v, 2, function(x) as.matrix(paint.df.cent[-mults,-1])%*%x)
#find the most important PC:
length(which(svd.paint$d < 1))
#all are important! keep all pc for now 
pcs.paint.import <- pcs.paint
# cluster on the principal components.
pcs.paint.import <- data.frame(pcs.paint.import)
pcs.paint.import$id <- id.new

