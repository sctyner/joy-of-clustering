

#Singular Value Decomposition (for Dimension Reduction)
# first have to center the columns 
# record the means for transformation back later: 
means.paint.cols <- apply(paint.df[,-1], 2, mean)
center <- function(x) x - mean(x)
paint.df.cent <- apply(paint.df[,-1], 2, center)
svd.paint <- svd(paint.df.cent[,-1])
# columns of svd.paint$v are the principal component directions
# z_j = X*v_j are the principal components. these are the cols of the mat below
pcs.paint <- apply(svd.paint$v, 2, function(x) as.matrix(paint.df.cent[,-1])%*%x)
#find the most important PC:
toss <- which(svd.paint$d < 1)
#starting at 224, all are essentially 0. 
pcs.paint.import <- pcs.paint[,-toss]
# cluster on the principal components.
pcs.paint.import <- data.frame(pcs.paint.import)
pcs.paint.import$id <- 1:242
