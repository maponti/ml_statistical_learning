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

# Using tune.svm to study kernels
model = tune.svm(x = X, y = as.factor(Y), kernel="polynomial", 
		 	degree=1:7, coef0=seq(0, 1, length=5), cost=10)

# Printing out the results in terms of errors
print(model$performances)
