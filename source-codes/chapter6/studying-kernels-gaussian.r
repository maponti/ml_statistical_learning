# Assessing the Gaussian kernel
require(e1071)
require(tseriesChaos)

X = cbind(rnorm(mean=10, sd=0.1, n=1000), rnorm(mean=10, sd=0.1, n=1000))
X = rbind(X, embedd(sin(2*pi*seq(0,9,len=1027)), m=2, d=27) + 10)

Y = c(rep(-1, 1000), rep(+1, 1000))

par(mfrow=c(1,2))
plot(X, xlim=c(0, max(X)), ylim=c(0, max(X)), col=Y+2)

results = c()
values = seq(1e-8, 0.01, length=100)

for (gamma in values) {
	cat("Gamma = ", gamma, "\n")
	results = c(results, svm(x = X, y = as.factor(Y), scale=FALSE, 
		    	kernel="radial", gamma=gamma, cost=10, cross=10)$tot.accuracy)
}
     
