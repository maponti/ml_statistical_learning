# Required packages
require(rgl)
require(tseriesChaos)

# Building up the dataset
X = embedd(sin(2*pi*seq(0,9,len=500)), m=2, d=14)
X = rbind(X, cbind(rnorm(mean=0, sd=0.1, n=500), rnorm(mean=0, sd=0.1, n=500)))
plot(X)
 
# Manually creating the kernel space. This is the impact of the 2-order polynomial kernel
K = cbind(X[,1]^2, sqrt(2)*X[,1]*X[,2], X[,2]^2)
plot3d(K)

# Computing the covariance matrix using the explicit transformation
# of vectors using the kernel function
M_1 = K%*%t(K)

# Computing the covariance matrix for the 2-order polynomial kernel,
# using the implicit application of the kernel function
M_2 = (X%*%t(X))^2
 
cat("Difference among matrices: ", sqrt(sum((M_1 - M_2)^2)), "\n")

