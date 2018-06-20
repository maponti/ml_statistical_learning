
first.classification.algorithm <- function(training.set, test.set) {
	# Defining the column id that represents the expected class
	classAttributeId = ncol(training.set)

	# Setting X and Y for training
	training.X = training.set[,1:(classAttributeId-1)]
	training.Y = training.set[,classAttributeId]

	# Setting X and Y for testing
	test.X = test.set[,1:(classAttributeId-1)]
	test.Y = test.set[,classAttributeId]

	# The final results are saved in this variable
	results = NULL

	cat("# Outcome\tExpected class\n")
	# For every unseen example in the test set
	for (unseen in 1:nrow(test.X)) {

		# These variables count the number of positive and
		# negative examples in the training set
		m_positive = 0
		m_negative = 0

		# To sum up the dot product of the unseen example
		# against every other example contained in the positive
		# and the negative classes
		sum_positive = 0
		sum_negative = 0

		# Apply the equations for the unseen example test.X[unseen,]
		# given the training set
		for (i in 1:nrow(training.X)) {

			if (training.Y[i] == +1) {
				sum_positive = sum_positive + test.X[unseen,] %*% training.X[i,]
				m_positive = m_positive + 1
			}

			if (training.Y[i] == -1) {
				sum_negative = sum_negative + test.X[unseen,] %*% training.X[i,]
				m_negative = m_negative + 1
			}
		}

		# These variables store the squared number of positive and 
		# negative examples in the training set. They are required
	        # to compute term b
		m_squared_positive = 0
		m_squared_negative = 0

		# To sum up the dot product of the unseen example
		# against every example contained in the positive
		# and the negative classes. They ares used to compute term b
		sum_b_positive = 0
		sum_b_negative = 0

		# Starting the computation of term b
		for (i in 1:nrow(training.X)) {
			for (j in 1:nrow(training.X)) {

				if (training.Y[i] == -1 && training.Y[j] == -1 ) {
					sum_b_negative = sum_b_negative + training.X[i,] %*% training.X[j,]
					m_squared_negative = m_squared_negative + 1
				}

				if (training.Y[i] == +1 && training.Y[j] == +1 ) {
					sum_b_positive = sum_b_positive + training.X[i,] %*% training.X[j,]
					m_squared_positive = m_squared_positive + 1

				}
			}
		}

		# Finally, we have term b.
		# We do not square variables m_squared_negative and m_squared_positive
		# because they were already squared due to the double loops used above
		b = 1/2 * (1/m_squared_negative * sum_b_negative - 1/m_squared_positive * sum_b_positive)

		# Now term y is computed to answer whether the unseen example will be
		# classified either as positive or negative
		y = sign(1/m_positive * sum_positive - 1/m_negative * sum_negative + b)

		# Saving the output class and the expected one, respectively
		results = rbind(results, cbind(y, test.Y[unseen]))

		# Printing out the results
		cat(y, "", test.Y[unseen], "\n")
	}

	return (results)
}

test.first <- function() {

	# Generating the positive examples
	dataset = cbind(rnorm(mean=0, sd=1, n=100), rnorm(mean=0, sd=1, n=100), rep(1, 100))

	# Generating the negative examples
	dataset = rbind(dataset, cbind(rnorm(mean=10, sd=1, n=100), rnorm(mean=10, sd=1, n=100), rep(-1, 100)))

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

	# Calling our classification algorithm to check the results
	results = first.classification.algorithm(train.set, test.set)

	return (results)
}
