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

# Defining the class labels
X = rbind(X1, X2)
Y = c(rep(-1, nrow(X1)), rep(+1, nrow(X2)))

# Plotting the input space
plot(X, col=Y+2)

# Using a linear kernel
model1 = svm(x = X, y = as.factor(Y), scale=FALSE, 
	    kernel="linear", cost=10, cross=10)
cat("Accuracy using a linear kernel: ", model1$tot.accuracy, "\n")

# Using a second-order polynomial kernel
model2 = svm(x = X, y = as.factor(Y), scale=FALSE, 
	    kernel="polynomial", degree=2, coef0=0, cost=10, cross=10)
cat("Accuracy using a second-order polynomial kernel: ", model2$tot.accuracy, "\n")

# Using a third-order polynomial kernel
model3 = svm(x = X, y = as.factor(Y), scale=FALSE, 
	    kernel="polynomial", degree=3, coef0=0, cost=10, cross=10)
cat("Accuracy using a third-order polynomial kernel: ", model3$tot.accuracy, "\n")

# Using a fourth-order polynomial kernel
model4 = svm(x = X, y = as.factor(Y), scale=FALSE, 
	    kernel="polynomial", degree=4, coef0=0, cost=10, cross=10)
cat("Accuracy using a fourth-order polynomial kernel: ", model4$tot.accuracy, "\n")

# Using a third-order polynomial kernel with coef0=1
model5 = svm(x = X, y = as.factor(Y), scale=FALSE, 
	    kernel="polynomial", degree=3, coef0=1, cost=10, cross=10)
cat("Accuracy using a third-order polynomial kernel with coef0=1: ", model5$tot.accuracy, "\n")
