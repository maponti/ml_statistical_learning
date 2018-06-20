# Assessing your data
require(e1071)
require(tseriesChaos)

X1 = embedd(sin(2*pi*seq(0,9,len=1000)), m=2, d=27)
ids = which(X1[,1] < -0.5 & X1[,2] < -0.5)
X1 = X1[-ids,]

X2 = embedd(sin(2*pi*seq(0,9,len=1000)), m=2, d=27)
ids = which(X2[,1] > 0.5 & X2[,2] > 0.5)
X2 = X2[-ids,]
X2[,1]=X2[,1]+0.3
X2[,2]=X2[,2]-0.75

X = rbind(X1, X2)
Y = c(rep(-1, nrow(X1)), rep(+1, nrow(X2)))
plot(X, col=Y+2)

results = matrix(0, nrow=10, ncol=5)
costs = seq(1e-7, 0.1, length=5)

for (d in 1:10) {
	cat("Running for degree ", d, "\n")
	for (id in 1:length(costs)) {
		cat("\tCost = ", costs[id], "\n")
		results[d, id] = svm(x = X, y = as.factor(Y), scale=FALSE, kernel="polynomial", 
			    	degree=d, coef0=1, cost=costs[id], cross=10)$tot.accuracy
	}
}

