# This package is necessary to execute function embedd()
require(tseriesChaos)

# Producing a 2-column dataset with examples (x(t), x(t+1))
X = embedd(ts(sunspot.year), m=2, d=1)

# Finding the minimal and maximal values for this time series
minValue = min(sunspot.year)
maxValue = max(sunspot.year)

# Spliting the values into 5 intervals so we will use each of
# them to compute probabilities
nIntervals = 5

# Compute the probability for every interval
intervals = seq(minValue, maxValue, length=nIntervals+1)
for (i in 1:nIntervals) {
	startInterval = intervals[i]
	endInterval = intervals[i+1]

	cat("Interval from [", startInterval, ", ", endInterval, "] has the following probabilities for a next value\n")
	for (j in 1:nIntervals) {
		# Defining the current value for startInterval <= x(t) < endInterval
		ids = which(X[,1] >= startInterval & X[,1] < endInterval)

		# Counting occurrences inside every interval for x(t+1)
		inside = sum(X[ids,2] >= intervals[j] & X[ids,2] < intervals[j+1])

		# Estimating the probabilities for x(t+1)
		probability = inside / nrow(X[ids,])

		cat("\tProbability of the next value in interval [", intervals[j], ", ", intervals[j+1], "] = ", probability, "\n")
	}
}
