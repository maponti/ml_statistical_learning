
# Estimating the Shattering coefficient (or function) for an input space given
# by the cartesian product of R real lines. For instance, for R^2 you use R=2.
# Parameter iter means the number of iterations used to assess every linear
# hyperplane in order to check out how many different classifications are found.
# Parameters n.start and n.end set the sample size n for the estimation.
# Term p sets the number of hyperplanes. Observe the range for vector w, term b
# and to generate the data sample (matrix sample) is fixed, but the user is suggested
# to adapt it to analyze a broader space.
shattering.coefficient.estimator <- function(iter=1000, n.start=1, n.end=100, p=1, R=2) {

	shatter = NULL
	cat("#Sample size\tNumber of different classifications found...\n")

	# For every sample size
	for (i in n.start:n.end) {
		sample = NULL

		# Produce some random data in the input space
		for (j in 1:R) {
			sample = cbind(sample, rnorm(mean=0, sd=1, n=i))
		}

		shatter.ways = list()

		# Attempt to find different classifications
		# provided by a single linear hyperplane
		for (j in 1:(i^2*iter)) {
			
			combined.labels = rep(0, nrow(sample))
			for (k in 1:p) {
				# Randomly sets vector w which is normal to the hyperplane
				w = runif(min=-10, max=10, n=R)

				# Randomly sets term b to define the intersection of the
				# hyperplane with the input variables
				b = runif(min=-5, max=5, n=1)

				# Providing the outcomes giving this random hyperplane
				labels = sample %*% w + b

				# If the outcome is equal to zero or greater
				# we will assume the positive class (or label)
				id = which(labels >= 0)

				# Otherwise the negative class
				nid = which(labels < 0)

				# Setting the positive and negative outcomes
				labels[id] = 2^k-2
				labels[nid] = 2^k-1

				# Combining hyperplanes
				combined.labels = combined.labels + labels
			}

			# Defining a key such as in a hashtable so we
			# can inform that this particular classification happened
			key = paste(combined.labels, sep="#", collapse="")
			shatter.ways[[key]] = 1
		}

		# Printing results out
		cat(i, " ", length(shatter.ways), "\n")
		shatter = rbind(shatter, c(i, length(shatter.ways)))
	}

	return (shatter)
}

