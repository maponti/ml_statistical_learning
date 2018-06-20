source("dwnn.r")

# Test the sinusoidal function
testSin <- function(sigma=0.01) {

	# Producing data
	data = sin(2*pi*seq(0,2, length=100)) + rnorm(mean=0, sd=0.1, n=100)
	training.ids = sample(1:length(data), size=50)
	test.ids = setdiff(1:length(data), training.ids)

	# Defining the training set
	training.set = cbind(training.ids, data[training.ids])

	# Defining the test set
	test.set = cbind(test.ids, data[test.ids])

	# Running DWNN
	results = dwnn(training.set, test.set, sigma)

	# Plotting the training set
	plot(training.set, xlab="x_i (input value)", ylab="y_i (expected class)")
	obtained.result = cbind(test.set[,1], results$obtained)

	# Plotting the DWNN results for unseen examples (in red)
	points(obtained.result, col=2)

	return (results)
}
