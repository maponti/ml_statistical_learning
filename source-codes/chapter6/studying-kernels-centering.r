require(e1071)
require(tseriesChaos)

# Building the dataset
X = cbind(rnorm(mean=10, sd=0.1, n=1000), rnorm(mean=10, sd=0.1, n=1000))
X = rbind(X, embedd(sin(2*pi*seq(0,9,len=1027)), m=2, d=27) + 10)

# Defining the class labels
Y = c(rep(-1, 1000), rep(+1, 1000))

# Plotting the input space
par(mfrow=c(1,2))
plot(X, xlim=c(0, max(X)), ylim=c(0, max(X)), col=Y+2)

# Using an homogeneous second-order polynomial kernel
model1 = svm(x = X, y = as.factor(Y), scale=FALSE, 
	    kernel="polynomial", degree=2, coef0=0, cost=1000, cross=10)
cat("Accuracy using the second-order polynomial kernel on the uncentered space: ", model1$tot.accuracy, "\n")

# Using a radial kernel
model2 = svm(x = X, y = as.factor(Y), scale=FALSE, 
	    kernel="radial", gamma=0.25, cost=1000, cross=10)
cat("Accuracy using the radial kernel on the uncentered space: ", model2$tot.accuracy, "\n")

# Centering the dataset
X = apply(X, 2, function(column) { column - mean(column) } )
plot(X, xlim=c(min(X), max(X)), ylim=c(min(X), max(X)), col=Y+2)

# The homogeneous second-order polynomial kernel is applied again
model3 = svm(x = X, y = as.factor(Y), scale=FALSE, 
	    kernel="polynomial", degree=2, coef0=0, cost=1000, cross=10)
cat("Accuracy using the second-order polynomial kernel on the centered space: ", model3$tot.accuracy, "\n")
