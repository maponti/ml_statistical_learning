
# This function builds up a very simple linearly separable dataset
simpleDataset <- function() {

	# Producing a two-dimensional dataset using the Normal distribution.
	# Negative examples are defined to have mean (0,0) with standard deviation (1,1).
	# Positive examples are defined to have mean (10,10) with standard deviation (1,1).

	# These are the training examples
	train <- cbind(rnorm(mean=0, sd=1, n=100), rnorm(mean=0, sd=1, n=100))
	train <- rbind(train, cbind(rnorm(mean=10, sd=1, n=100), rnorm(mean=10, sd=1, n=100)))
	train <- cbind(train, c(rep(-1, 100), rep(1, 100)))

	# These are the test examples
	test <- cbind(rnorm(mean=0, sd=1, n=10), rnorm(mean=0, sd=1, n=10))
	test <- rbind(test, cbind(rnorm(mean=10, sd=1, n=10), rnorm(mean=10, sd=1, n=10)))
	test <- cbind(test, c(rep(-1, 10), rep(1, 10)))

	# Returning the training and test sets using a list
	return (list(train=train, test=test))
}

# This function outputs the classification (labels) for a given set.
# In our case, we use it to print out the labels predicted for the test set.
discrete.classification <- function(X, Y, alpha, b, X.test, Y.test, threshold = 1e-5) {
	all.labels = NULL
	alphas = diag(alpha)
	alphas[alphas < threshold] = 0

	for (i in 1:nrow(X.test)) {
		label = sum(alphas * as.vector(Y) * (X.test[i,] %*% t(X))) + b
		if (label >= 0) 
			label = 1
		else
			label = -1
		expected_label = Y.test[i,]
		all.labels = rbind(all.labels, cbind(expected_label, label))
	}

	colnames(all.labels) = c("Expected class", "Predicted class")

	return (all.labels)
}

# This function implements the Primal-Dual Path Following algorithm
# for the simple dataset (linearly separable).
ipm.svm <- function() {

	# Building up the linearly separable dataset
	dataset = simpleDataset()

	# Creating matrix X to represent all vector x_i (training examples)
	X = dataset$train[,1:2]

	# This vector y with labels -1 and +1 
	# (we in fact created a matrix with a single column)
	Y = matrix(dataset$train[,3], ncol=1)

	# Number of training examples
	npoints = nrow(X)

	# Computing matrix Q as we formulated
	Q = (Y %*% t(Y)) * (X %*% t(X))

	# Defining values to start the execution
	C = 1		 		# Upper limit for alpha_i
	eta = 0.1	 		# This will be used to adapt the variables of our problem
	b = runif(min=-1, max=1, n=1)	# Initial b
	iteration = 1 	 		# Counter of iteration
	threshold = 1e-5 		# This parameter defines the stop criterion

	# Vector filled out with ones (one's vector)
	e = rep(1, nrow(Q))

	# Identity matrix
	I = diag(rep(1, nrow(Q)))

	# Setting all alphas as half of C into a diagonal matrix
	Alpha = diag(rep(C/2, nrow(Q)))

	# Defining the diagonal matrix Xi using the same values of alphas,
	# what makes constraints respected according to the values of the 
	# slack variables 
	Xi = Alpha

	# Computing the diagonal matrix S using the first equation, as follows:
	# Q alpha + y b + Xi - s - e = 0
	# Q alpha + y b + Xi - e = s
	S = diag(as.vector(Q%*%diag(Alpha) + Y*b + diag(Xi) - e))

	# This value found for S helps us to compute equation:
	#
	# S alpha - mu = 0
	# S alpha = mu
	#
	# allowing to find the current Gap for our solution
	# (please refer to the first Primal-Dual Path Following algorithm
	#  used to tackle the linear optimization problem)
	gap = e%*%S%*%Alpha%*%e

	# This is the initial mu
	mu = as.numeric(gap)

	# Factor to reduce mu along iterations and eventually get closer to barriers.
	# This is necessary if the solution is close to one of the barrier terms.
	reducing.factor = 0.9		

	# Identity matrix
	I = diag(nrow(Q))

	# Building up the Jacobian matrix.
	# First and second rows will not change anymore, 
	# therefore they are initialized before the iterative process.

	# Jacobian matrix: first row
	A = matrix(0, nrow=(3*npoints+1), ncol=(3*npoints+1))
	A[1:nrow(Q), 1:ncol(Q)] = Q
	A[1:nrow(Q), ncol(Q)+1] = Y
	A[1:nrow(Q), (ncol(Q)+2):(2*npoints+1)] = -I
	A[1:nrow(Q), (2*npoints+2):(3*npoints+1)] = I

	# Jacobian matrix: second row
	A[nrow(Q)+1, 1:length(Y)] = -t(Y)

	while (gap > threshold) {

		# Jacobian matrix: third row
		A[(npoints+2):(2*npoints+1), 1:npoints] = S
		A[(npoints+2):(2*npoints+1), (npoints+2):(2*npoints+1)] = Alpha

		# Jacobian matrix: fourth row
		A[(2*npoints+2):(3*npoints+1), 1:npoints] = -Xi
		A[(2*npoints+2):(3*npoints+1), (2*npoints+2):(3*npoints+1)] = diag(rep(C,npoints))-Alpha

		# Building up vector b
		B = matrix(0, nrow=2*npoints+1, ncol=1)

		# First function
		f1 = - Q%*%diag(Alpha) - Y*b - diag(Xi) + diag(S) + e

		# Second function
		f2 = diag(Alpha)%*%Y

		# Third function
		f3 = -diag(S%*%Alpha) + mu

		# Fourth function
		f4 = -(diag(rep(C,npoints))-Alpha)%*%diag(Xi) + mu

		B[1:npoints] = f1
		B[npoints+1] = f2
		B[(npoints+2):(2*npoints+1)] = f3
		B[(2*npoints+2):(3*npoints+1)] = f4

		# Solving the system (this solver comes with the package base)
		d = solve(A, B)

		# Cutting out the corresponding Deltas for Alpha, b, S and Xi
		# to be later used as updating factors
		d_alpha = d[1:npoints]
		d_b = d[npoints+1]
		d_S = d[(npoints+2):(2*npoints+1)]
		d_Xi = d[(2*npoints+2):(3*npoints+1)]

		# Updating the variables for our problem.
		# Parameter eta defines the update step
		diag(Alpha) = diag(Alpha) + eta * d_alpha
		b = b + eta * d_b
		diag(S) = diag(S) + eta * d_S
		diag(Xi) = diag(Xi) + eta * d_Xi

		# Counting iterations
		iteration = iteration + 1

		# Recalculating the Gap for the next iteration
		gap = e%*%S%*%Alpha%*%e

		# We decrease the value of mu to allow our algorithm to get
		# closer to barriers whenever necessary
		mu = mu * reducing.factor

		cat("Current Gap is ", gap, "\n")
	}

	# Plotting the dataset and the support vectors.
	# Support vectors correspond to every x_i that was found to help
	# our algorithm define the maximal-margin hyperplane
	colors = rep(1, nrow(Q))
	ids = which(diag(Alpha) > 1e-5)
	colors[ids] = 2
	plot(X, col=colors, main="Dataset and support vectors")
	locator(1)

	# Plotting the classification results.
	# Creating matrix X to represent all vectors x_i (test examples)
	X.test = dataset$test[,1:2]

	# This vector y contains labels -1 and +1
	# (we created a matrix with a single column)
	Y.test = matrix(dataset$test[,3], ncol=1)

	print(discrete.classification(X, Y, Alpha, b, X.test, Y.test, threshold = 1e-5))
}
