source("first-classification-algorithm.r")

test.complex <- function() {

	# Generating the positive examples
	dataset = cbind(rnorm(mean=0, sd=0.25, n=200), rnorm(mean=0, sd=0.25, n=200), rep(1, 200))

	# Generating the negative examples
	negative.set = 5*sin(2*pi*seq(0,9,len=200))+rnorm(mean=0, sd=0.5, n=200)
	dataset = rbind(dataset, cbind(negative.set[1:(length(negative.set)-5)], 
				negative.set[6:length(negative.set)], rep(-1, length(negative.set)-5)))

	# Plotting the dataset
	plot(dataset[,1:2], col=dataset[,3]+2)
	cat("Click on the chart to continue...\n")
	locator(1)

	# Setting the training set size
	train.size = round(nrow(dataset)/2)

	# Sampling half of this dataset for training
	id = sample(1:nrow(dataset), size=train.size)

	# Building up the training set
	train.set = dataset[id,]

	# Building up the test set
	test.set = dataset[-id,]

	# Calling our classification algorithm to check results
	results = first.classification.algorithm(train.set, test.set)

	return (results)
}
