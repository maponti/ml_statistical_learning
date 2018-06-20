# Loading package LowRankQP
library(LowRankQP)

# This is the main function which is responsible for solving the optimization
# problem using linear or a kth-order polynomial kernel. It receives the training
# set X and its corresponding classes Y in {-1, +1}. We also set the upper limit C
# for every value contained in vector alpha.
svm.polynomial <- function(X, Y, C = Inf, polynomial.order = 2, threshold = 1e-8) {

	# Building up matrix Q. Observe the kernel function is defined
	# in here. If polynomial.order=1, then we are considering the
	# original input space of examples in X, otherwise we are applying
	# some nolinear space transformation.
	Qmat <- (Y %*% t(Y)) * (1+(X %*% t(X)))^polynomial.order

	# Defining d as a vector containing values equal to -1
	# to ensure the problem solved by LowRankQP is the same as ours
	dvec <- rep(-1, nrow(X))

	# Defining matrix A as the transpose of vector y
	Amat <- t(Y)

	# Defining b as a zero vector
	bvec <- 0

	# Setting the upper limit vector with values defined by C
	uvec <- rep(C, nrow(X))

	# Running the LowRankQP function to find vector alpha for which
	# constraints are satisfied. Thus, we minimize the functional 
	# defined by LowRankQP
	res <- LowRankQP(Qmat, dvec, Amat, bvec, uvec, method="CHOL")

	# This is vector alpha found after the optimization process
	alphas <- res$alpha

	# Here we define which are the support vectors using the values 
	# in vector alpha. Values above some threshold are taken as more 
	# relevant (remember these are the KKT multipliers) to define 
	# constraints
	support.vectors <- which(alphas > threshold)

	# Finally, we define the identifiers of support vectors so we
	# know who they are
	support.alphas <- alphas[support.vectors]

	# Now we define the margin using the support vectors
	margin <- support.vectors

	# and then compute the value for b
	b <- Y[margin] - t(support.alphas*Y[support.vectors]) %*% (1+(X[support.vectors,] %*% t(X[margin,])))^polynomial.order

	# Returning the whole model found during the optimization process
	return (list(X=X, Y=Y, polynomial.order=polynomial.order, support.vectors=support.vectors, support.alphas=support.alphas, b=mean(b), all.alphas=as.vector(alphas)))
}

# This is a simple function to provide the discrete classification for unseen examples
discrete.classification <- function(model, testSet) {

	# Creating a vector to store labels
	all.labels = c()

	# For every unseen example in this test set
	for (i in 1:nrow(testSet)) {

		# Use the model found through function svm.polynomial to
		# obtain the classification output
		label = sum(model$all.alphas * model$Y * (1+(testSet[i,] %*% t(model$X)))^model$polynomial.order) + model$b

		# If label >= 0, so the test example lies on the positive side 
		# of the hyperplane, otherwise it lies on the negative one
		if (label >= 0) 
			label = 1
		else
			label = -1

		# Storing labels
		all.labels = c(all.labels, label)
	}

	# Returning the labels found
	return (all.labels)
}

# This is a simple function to provide the continuous classification for unseen examples
continuous.classification <- function(model, testSet) {

	# Creating a vector to store labels
	all.labels = c()

	# For every unseen example in this test set
	for (i in 1:nrow(testSet)) {

		# Use the model found through function svm.polynomial to
		# obtain the classification output
		label = sum(model$all.alphas * model$Y * (1+(testSet[i,] %*% t(model$X)))^model$polynomial.order) + model$b

		# Storing labels
		# The signal associated with this value indicates the label, i.e., - corresponds 
		# to class -1 and + to class +1. In addition, the magnitude of this variable
		# `label' informs us how close or far the unseen example is from the hyperplane
		all.labels = c(all.labels, label)
	}

	# Returning the labels found
	return (all.labels)
}

# This is a simple function to plot the hyperplane found, but only for bidimensional training 
# and test sets
plotHyperplane <- function(model, x.axis=c(-1,1), y.axis=c(-1,1), resolution=100, continuous=TRUE) {

	# Producing a set of values for the two dimensions of the training/test sets
	x = seq(x.axis[1], x.axis[2], len=resolution)
	y = seq(y.axis[1], y.axis[2], len=resolution)

	# This is a matrix to store what we refer to plot set.
	# It is bidimensional as the training and test sets
	plotSet = NULL
	for (i in 1:length(x)) {
		for (j in 1:length(y)) {
			plotSet = rbind(plotSet, c(x[i], y[j]))
		}
	}

	# This is a matrix to save labels for plotting
	labels = NULL
	if (continuous) {
		# Running the continuous classification
		labels = matrix(continuous.classification(model, plotSet), nrow=length(x), ncol=length(y), byrow=T)
	} else {
		# or the discrete classification
		labels = matrix(discrete.classification(model, plotSet), nrow=length(x), ncol=length(y), byrow=T)
	}

	# Plotting the hyperplane found
	filled.contour(x,y,labels)
}

# This function produces very simple linearly separable training/test sets
simpleDataset <- function() {

	# Building up the training set with 100 examples
	train <- cbind(rnorm(mean=0, sd=1, n=100), rnorm(mean=0, sd=1, n=100))
	train <- rbind(train, cbind(rnorm(mean=10, sd=1, n=100), rnorm(mean=10, sd=1, n=100)))
	train <- cbind(train, c(rep(-1, 100), rep(1, 100)))

	# Building up the test set with 10 examples
	test <- cbind(rnorm(mean=0, sd=1, n=10), rnorm(mean=0, sd=1, n=10))
	test <- rbind(test, cbind(rnorm(mean=10, sd=1, n=10), rnorm(mean=10, sd=1, n=10)))
	test <- cbind(test, c(rep(-1, 10), rep(1, 10)))

	# Returning both sets
	return (list(train=train, test=test))
}

# This is a very simple function to test function svm.polynomial
testSimpleDataset <- function() {

	# Building up a very simple linearly separable training set
	dataset = simpleDataset()

	# Optimizing values for alpha, given this simple dataset
	model = svm.polynomial(dataset$train[,1:2], dataset$train[,3], C=10000, polynomial.order=1)

	# Plotting all values in vector alpha to check them out
	# and observe which are the most relevant ones
	plot(model$all.alphas)
	locator(1)

	# Plotting data space in black and support vectors in red
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
	plotHyperplane(model, x.axis=c(-1,11), y.axis=c(-1,11), resolution=100, continuous=FALSE)
}

