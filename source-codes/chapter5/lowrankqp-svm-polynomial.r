# This package is used to build up the radial dataset
require(tseriesChaos)

# Loading the source code to start the optimization process
source("lowrankqp-svm-simple.r")

# This function builds up a radial dataset
radialDataset <- function() {
	
	# Building up the training set with 1000 examples
	train <- rbind(cbind(rnorm(mean=0, sd=0.1, n=1000), rnorm(mean=0, sd=0.1, n=1000)))
	train <- rbind(train, embedd(2*sin(2*pi*seq(0,9,length=1027))+
				     rnorm(mean=0, sd=0.1, n=1027), m=2, d=27))
	train <- cbind(train, c(rep(-1, 1000), rep(+1, 1000)))

	# Building up the test set with 10 examples
	test <- rbind(cbind(rnorm(mean=0, sd=0.1, n=10), rnorm(mean=0, sd=0.1, n=10)))
	test <- rbind(test, embedd(2*sin(2*pi*seq(0,9,length=37))+
				   rnorm(mean=0, sd=0.1, n=37), m=2, d=27))
	test <- cbind(test, c(rep(-1, 10), rep(+1, 10)))

	return (list(train=train, test=test))

}

# This function is used to test the SVM optimization with a radial dataset
testRadialDataset <- function(C=10) {

	# Building up the radial dataset
	dataset = radialDataset()

	# Running the SVM optimizer, so we can estimate adequate values for vector alpha.
	# Notice we are now using a second-order polynomial kernel.
	model = svm.polynomial(dataset$train[,1:2], dataset$train[,3], C=C, 
			       polynomial.order=2, threshold = 1e-3)

	# Plotting all values contained in vector alpha in order to check them out
	# and conclude on which are the most relevant ones
	plot(model$all.alphas)
	locator(1)

	# Plotting the data space in black and support vectors in red
	plot(dataset$train[,1:2])
	points(dataset$train[model$support.vectors,1:2], col=2)
	locator(1)

	# Printing labels -1 and +1 for verification
	labels = discrete.classification(model, dataset$test[,1:2])
	result = cbind(dataset$test[,3], labels)
	colnames(result) = c("Expected class", "Obtained class")
	cat("Discrete classification:\n")
	print(result)

	# Printing the continuous classification out
	labels = continuous.classification(model, dataset$test[,1:2])
	result = cbind(dataset$test[,3], labels)
	colnames(result) = c("Expected class", "Obtained class")
	cat("Continuous classification:\n")
	print(result)

	# Plotting the hiperplane found
	plotHyperplane(model, x.axis=c(-5,5), y.axis=c(-5,5), resolution=100, continuous=FALSE)
}
