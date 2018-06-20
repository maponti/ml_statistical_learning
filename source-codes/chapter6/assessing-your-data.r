require(e1071)
require(tseriesChaos)

# Building the dataset
X1 = embedd(sin(2*pi*seq(0,9,len=1000)), m=2, d=27)
ids = which(X1[,1] < -0.5 & X1[,2] < -0.5)
X1 = X1[-ids,]

X2 = embedd(sin(2*pi*seq(0,9,len=1000)), m=2, d=27)
ids = which(X2[,1] > 0.5 & X2[,2] > 0.5)
X2 = X2[-ids,]
X2[,1]=X2[,1]+0.3
X2[,2]=X2[,2]-0.75

X = rbind(X1, X2)

# Defining the class labels
Y = c(rep(-1, nrow(X1)), rep(+1, nrow(X2)))

# Plotting the input space
plot(X, col=Y+2)

# Assessing several polynomial kernels
results = matrix(0, nrow=7, ncol=5)
coeffs = seq(0, 1, length=5)
for (d in 1:7) {
	cat("Running for degree ", d, "\n")
	col = 1
	for (c in coeffs) {
		results[d, col] = svm(x = X, y = as.factor(Y), scale=FALSE, kernel="polynomial", 
			    	degree=d, coef0=c, cost=10, cross=10)$tot.accuracy
		col = col + 1
	}
}

column.names = c()
for (c in coeffs) { column.names = c(column.names, paste("coef=", c, sep="")) }

order.names = c()
for (d in 1:7) { order.names = c(order.names, paste("order=", d, " :", sep="")) }

results = as.data.frame(results)
colnames(results) = column.names
rownames(results) = order.names

# Printing out the accuracies
print(results)
