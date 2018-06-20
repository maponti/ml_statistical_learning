source("first-classification-complex.r")

require(rgl)

Transformation <- function(vec) {
	class = vec[3]
	# Observe the expected class will be the same
	return (c(vec[1]^2, sqrt(2)*vec[1]*vec[2], vec[2]^2, class))
}

test.complex.kernel <- function() {

	# Generating the positive examples
	dataset = cbind(rnorm(mean=0, sd=0.25, n=200), rnorm(mean=0, sd=0.25, n=200), rep(1, 200))

	# Generating the negative examples
	negative.set = 5*sin(2*pi*seq(0,9,len=200))+rnorm(mean=0, sd=0.5, n=200)
	dataset = rbind(dataset, cbind(negative.set[1:(length(negative.set)-5)], 
				negative.set[6:length(negative.set)], rep(-1, length(negative.set)-5)))

	# Plotting the original dataset
	plot(dataset[,1:2], col=dataset[,3]+2)
	cat("Click on the chart to continue...\n")
	locator(1)

	# Applying the kernel function to map every example into the features space
	new.dataset = NULL
	for (i in 1:nrow(dataset)) {
		new.dataset = rbind(new.dataset, Transformation(dataset[i,]))
	}

	print(new.dataset)
	# Plotting the transformed dataset
	plot3d(new.dataset[,1:3], col=new.dataset[,4]+2)

	# Setting the training set size
	train.size = round(nrow(new.dataset)/2)

	# Sampling half of this new.dataset for training
	id = sample(1:nrow(new.dataset), size=train.size)

	# Building up the training set
	train.set = new.dataset[id,]

	# Building up the test set
	test.set = new.dataset[-id,]

	# Calling our classification algorithm to check results
	results = first.classification.algorithm(train.set, test.set)

	return (results)
}
