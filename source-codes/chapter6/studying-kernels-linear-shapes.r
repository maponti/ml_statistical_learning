require(e1071)
require(tseriesChaos)

# Building the dataset
X = cbind(rnorm(mean=-1, sd=0.1, n=1000), rnorm(mean=-1, sd=0.1, n=1000))
X = rbind(X, cbind(rnorm(mean=1, sd=0.1, n=1000), rnorm(mean=1, sd=0.1, n=1000)))

# Defining the class labels
Y = c(rep(-1, 1000), rep(+1, 1000))

# Plotting the input space
plot(X, xlim=c(min(X), max(X)), ylim=c(min(X), max(X)), col=Y+2)

# Using a linear kernel
model1 = svm(x = X, y = as.factor(Y), scale=FALSE, 
	    kernel="linear", cost=1000, cross=10)

cat("Accuracies for each one of the ten classifiers found:\n")
print(model1$accuracies)

# Using a second-order polynomial kernel
model2 = svm(x = X, y = as.factor(Y), scale=FALSE, 
	    kernel="polynomial", degree=2, coef0=0, cost=1000, cross=10)

cat("Accuracies for each one of the ten classifiers found:\n")
print(model2$accuracies)


