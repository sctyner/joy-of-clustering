

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