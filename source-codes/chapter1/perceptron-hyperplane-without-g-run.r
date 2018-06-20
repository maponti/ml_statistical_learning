source("perceptron-hyperplane-without-g.r")

# This function is used to run the training stage for the simplest
# classification task. Each training will produce a different pair
# of weight and theta, which are then used to plot function net
run.several.times <- function(times=5) {

	# Saving the results for each function net
	net.functions = list()

	# For each execution
	for (i in 1:times) {
		# Call training
		training.execution = perceptron.run.simple()

		# Obtaining function net
		net.functions[[i]] = perceptron.simple.hyperplane.plot.without.g(
					training.execution$weight, 
					training.execution$theta)
	}

	# Plotting
	plot(net.functions[[1]], col=1)
	for (i in 2:times) {
		points(net.functions[[i]], col=i)
	}
}
