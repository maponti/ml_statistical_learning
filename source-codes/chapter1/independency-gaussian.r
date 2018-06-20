# This package is necessary to execute function embedd()
require(tseriesChaos)

# Producing a 2-column dataset with examples (x(t), x(t+1))
gaussian = rnorm(mean=0, sd=1, n=1000000)
X = embedd(ts(gaussian), m=2, d=1)

# Finding the minimal and maximal values for this time series
minValue = min(gaussian)
maxValue = max(gaussian)

# Spliting the values into 25 intervals so we will use every
# interval to compute probabilities
nIntervals = 7

histograms = list()
# Compute the probability for every interval
intervals = seq(minValue, maxValue, length=nIntervals+1)
for (i in 1:nIntervals) {
	startInterval = intervals[i]
	endInterval = intervals[i+1]

	cat("Interval from [", startInterval, ", ", endInterval, "] has the following probabilities for a next value\n")
	histograms[[i]] = c(0)
	for (j in 1:nIntervals) {
		# Defining the current value for startInterval <= x(t) < endInterval
		ids = which(X[,1] >= startInterval & X[,1] < endInterval)

		# Counting occurrences inside every interval for x(t+1)
		inside = sum(X[ids,2] >= intervals[j] & X[ids,2] < intervals[j+1])

		# Estimating the probabilities for x(t+1)
		probability = inside / nrow(X[ids,])
		histograms[[i]] = c(histograms[[i]], probability)

		cat("\tProbability of the next value in interval [", intervals[j], ", ", intervals[j+1], "] = ", probability, "\n")
	}
}

par(mfrow=c(2,3))
hist(gaussian, freq=T, breaks=seq(min(gaussian), max(gaussian), length=nIntervals+1))
sampid = sample(1:nIntervals, size=5)
for (i in 1:5) {
	len = length(histograms[[ sampid[i] ]])
	plot(histograms[[ sampid[i] ]][2:len], t="s", ylab="Probability", xlab="Intervals", ylim=c(0, max(histograms[[ sampid[i] ]][2:len])))
}
