source("perceptron.r")

perceptron.simple.error <- function(range.start=-1, range.end=1, mu=1e-10) {

	# Defining the table with examples
	table = cbind(seq(0, 0.5, length=100), rep(0, 100)) # negative examples
	table = rbind(table, cbind(seq(0.5+mu, 1, length=100), rep(1, 100))) # positive examples

	# We will now define the same range for the free variables weight and theta.
	# This range will contain 100 discretized values
	range_for_free_variables = seq(range.start, range.end, length=100)
	weight = range_for_free_variables
	theta = range_for_free_variables
	
	# Sum of errors while varying weight and theta
	error_function = matrix(0, nrow=length(weight), ncol=length(theta))

	# For each weight
	for (w in 1:length(weight)) {
		# For each theta
		for (t in 1:length(theta)) {
			# Compute all net values
			net = cbind(table[,1], rep(1, nrow(table))) %*% c(weight[w], theta[t])
			# Defining a vector to save the Perceptron outputs
			hat_y = rep(0, length(net))
			# Producing the output classes
			for (n in 1:length(net)) {
				# We removed function g(net) to improve illustration
				hat_y[n] = net[n] 
			}

			# These are the expected classes
			y = table[,2]
			# Expected minus the obtained classes to provide the error
			error = y - hat_y
			# Saving the total error in the matrix
			error_function[w, t] = sum(error) # This brings up amortization problems
							  # because positive and negative terms
							  # will cancel each other
		}
	}

	# Plotting the error
	filled.contour(error_function)
}

perceptron.simple.squared.error <- function(range.start=-10, range.end=10, mu=1e-10) {

	# Defining the table with examples
	table = cbind(seq(0, 0.5, length=100), rep(0, 100))
	table = rbind(table, cbind(seq(0.5+mu, 1, length=100), rep(1, 100)))

	# We will now define the same range for the free variables weight and theta.
	# This range will contain 100 discretized values
	range_for_free_variables = seq(range.start, range.end, length=50)
	weight = range_for_free_variables
	theta = range_for_free_variables
	
	# Sum of squared errors while varying weight and theta
	error_function = matrix(0, nrow=length(weight), ncol=length(theta))

	# For each weight
	for (w in 1:length(weight)) {
		# For each theta
		for (t in 1:length(theta)) {
			# Compute all net values
			net = cbind(table[,1], rep(1, nrow(table))) %*% c(weight[w], theta[t])
			# Defining a vector to save the Perceptron outputs
			hat_y = rep(0, length(net))
			# Producing the output classes
			for (n in 1:length(net)) {
				# We removed function g(net) to improve illustration
				hat_y[n] = net[n]
			}

			# These are the expected classes
			y = table[,2]
			# Expected minus the obtained classes to provide the error,
			# which is then squared to avoid negative and positive values
			# that amortize each other
			squared.error = (y - hat_y)^2
			# Saving the total squared error in the matrix
			error_function[w, t] = sum(squared.error)
		}
	}

	# We apply a log on the squared error function to improve illustration,
	# otherwise we do not see the paraboloid as clear as in this form.
	filled.contour(log(error_function))
}

