source("perceptron.r")

# This is an example of learning the problem AND
#
# To run this example:
#
#	1) Open the R Statistical Software
#	2) source("perceptron-AND.r")
#	3) perceptron.run.AND()
#
perceptron.run.AND <- function() {

	# This is a table with all possible examples
	# for the problem AND. In this case we will
	# use the same table for training and testing,
	# just because we know all possible binary
	# combinations
	table = matrix(c(0, 0, 0,  # 0 AND 0 = 0
			 0, 1, 0,  # 0 AND 1 = 0
			 1, 0, 0,  # 1 AND 0 = 0
			 1, 1, 1), # 1 AND 1 = 1
			nrow=4,
			ncol=3,
			byrow=TRUE)

	# Training the Perceptron to find weights and theta
	training.result = perceptron.train(table)

	# Testing the Perceptron with the weights and theta found
	perceptron.test(table, training.result$weights, training.result$theta)

	# Plotting the hyperplane found
	perceptron.hyperplane.plot(training.result$weights, training.result$theta)
}

# This function plots the hyperplane found for a given 
# classification task with two input variables only.
# Variables range.start and range.end define the interval of values
# for every one of those input variables composing the problem.
# The problem AND has two variables composing each example i,
# which are x_i,1 and x_i,2
perceptron.hyperplane.plot <- function(weights, theta, range.start=0, range.end=1) {

	# Variable weights define the number of input variables we have,
	# so we can use this information to create axes in a multidimensional
	# input space in order to see how inputs modify the output class
	# provided by the Perceptron
	nVars = length(weights)

	# We will now define the same range for every input variable.
	# This range will contain 100 discretized values
	range_of_every_input_variable = seq(range.start, range.end, length=100)

	x_1 = range_of_every_input_variable
	x_2 = range_of_every_input_variable
	
	# Function outer combines every possible value for variable x_1
	# against every possible value for x_2. Observe they are continuous values
	# which were never seen (we expect either 0 or 1) by this Perceptron during 
	# the training stage. Also observe value 1 inside the cbind, which refers
	# to the 1 * theta while computing function net. Operation %*% corresponds
	# to the dot product.
	all_nets = outer(x_1, x_2, function(x, y) { cbind(x, y, 1) %*% c(weights, theta) } )

	# This variable all_nets contains all net values for every combination between
	# variables x_1 and x_2. Variable y will contain the Perceptron outputs after
	# applying the heaviside function
	y = matrix(0, nrow=nrow(all_nets), ncol=ncol(all_nets))
	for (row in 1:nrow(all_nets)) {
		for (col in 1:ncol(all_nets)) {
			y[row, col] = g(all_nets[row, col])
		}
	}

	# Plotting the hyperplane found by the Perceptron
	filled.contour(x_1, x_2, y)
}

