source("perceptron.r")

# This function plots the hyperplane found for this simplest
# problem which considers a single input variable.
# Variables range.start and range.end define the interval of
# values for the single input variable composing the problem.
# This simple problem has a single variable composing every
# example i, which is x_i,1
perceptron.simple.hyperplane.plot <- function(weight, theta, range.start=0, range.end=1) {

	# Number of variables is 1
	nVars = 1

	# We will now define the same range for the input variable.
	# This range will contain 100 discretized values
	range_of_every_input_variable = seq(range.start, range.end, length=100)
	x_1 = range_of_every_input_variable
	
	# Computing net for every input value of variable x_i,1
	all_nets = cbind(x_1, 1) %*% c(weight, theta)

	# This variable all_nets contains all net values for all values assumed
	# by variable x_1. Variable hat_y will contain the Perceptron outputs after
	# applying the heaviside function
	hat_y = rep(0, length(all_nets))
	for (i in 1:length(all_nets)) {
		hat_y[i] = g(all_nets[i])
	}

	# Variable hyperplane will contain two columns, the first corresponds to
	# the input value of x_i,1 and the second to the class produced by the
	# Perceptron
	hyperplane = cbind(x_1, hat_y)

	# Plotting the hyperplane found by the Perceptron
	plot(hyperplane)

	return (hyperplane)
}

