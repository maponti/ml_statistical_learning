
# Computing the L2-norm between vector x_q and x_i
euclidean <- function(x_i, x_q) {
	sqrt(sum((x_i-x_q)^2))
}

# This is the weighing function
w_i <- function(dist, sigma) {
	exp(-dist^2/(2*sigma^2))
}

# This is the DWNN algorithm. It receives the training set,
# the test set and then sigma.
dwnn <- function(training.set, test.set, sigma = 1) {

	# Number of input attributes (we consider only the
	# last one as the output class)
	nAttrs = ncol(training.set)-1
	class = ncol(training.set)

	obtained = rep(0, nrow(test.set))

	# For every example in the test set
	for (q in 1:nrow(test.set)) {
		x_q = as.vector(test.set[q,1:nAttrs])
		num = 0
		den = 0

		# Computing the output class based on every
		# example i in the training set
		for (i in 1:nrow(training.set)) {
			# Computing the L2-norm
			dist = euclidean(training.set[i,1:nAttrs], x_q)

			# Computing the weight
			weight = w_i(dist, sigma)
			num = num + weight * training.set[i, class]
			den = den + weight
		}

		# The output class according to DWNN
		produced_output = num / den
		obtained[q] = produced_output
	}

	# List of DWNN results
	ret = list()

	# The obtained class after executing DWNN
	ret$obtained = obtained

	# The absolute error in terms of the expected class
	# versus the obtained one
	ret$absError = abs(test.set[,class] - obtained)

	# Here we save the expected class for later use
	# (if necessary)
	ret$expected = test.set[,class]

	return (ret)
}

# Test the identity function
testIdentity <- function(sigma=0.01) {

	# Defining the training set
	training.set = cbind(seq(-5,5,by=1), seq(-5,5,by=1))

	# Defining the test set
	test.set = cbind(seq(-5.5,5.5,by=1), seq(-5.5,5.5,by=1))

	results = dwnn(training.set, test.set, sigma)

	# Plotting the training set
	plot(training.set, xlab="x_i (input value)", ylab="y_i (expected class)")
	obtained.result = cbind(test.set[,1], results$obtained)
	# Plotting the DWNN results for the unseen example (in red)
	points(obtained.result, col=2)

	return (results)
}
