
# This is the MLP sigmoid activation function
f <- function(net) {
	ret = 1.0 / (1.0 + exp(-net))
	return (ret)
}

# This function is used to build up the MLP architecture, i.e., the
# neurons contained in the hidden and the output layers with their
# respective weights and thetas randomically initialized.
mlp.architecture <- function(input.layer.size = 2,
		hidden.layer.size = 2,
		output.layer.size = 1,
		f.net = f) {

	# Here we create a list to contain the layers information
	layers = list()

	# This is the hidden layer in which weights and thetas
	# were initialized in a random manner (using runif) in
	# interval [-1,1]. Term input.layer.size+1 refers to
	# the number of neurons in the input layer (a weight per unit),
	# plus an additional element to define theta
	layers$hidden = matrix(runif(min=-1, max=1, 
				  n=hidden.layer.size*(input.layer.size+1)), 
			       nrow=hidden.layer.size, 
			       ncol=input.layer.size+1)

	# The same as the hidden layer happens here, but for the output layer
	layers$output = matrix(runif(min=-1, max=1, 
				  n=output.layer.size*(hidden.layer.size+1)), 
			       nrow=output.layer.size, 
			       ncol=hidden.layer.size+1)

	# Defining a list to return everything:
	# - the number of units or neurons at the input layer
	# - the number of units at the hidden layer
	# - the number of units at the output layer
	# - layers information (including weights and thetas)
	# - the activation function used is also returned
	ret = list()
	ret$input.layer.size = input.layer.size
	ret$hidden.layer.size = hidden.layer.size
	ret$output.layer.size = output.layer.size
	ret$layers = layers
	ret$f.net = f.net

	return (ret)
}

# This function produces the MLP output after providing input values.
# Term architecture refers to the model produced by function mlp.architecture.
# Term dataset corresponds to the examples used as input to the MLP.
# Term p is associated to the identifier of the current example being forwarded.
forward <- function(architecture, dataset, p) {

	# Orgazining the dataset as input examples x
	x = matrix(dataset[,1:architecture$input.layer.size], ncol=architecture$input.layer.size)
	# Orgazining the dataset as expected classes y associated to input examples x
	y = matrix(dataset[,(architecture$input.layer.size+1):ncol(dataset)], nrow=nrow(x))

	# Submitting the p-th input example to the hidden layer
	net_h = architecture$layers$hidden %*% c(as.vector(ts(x[p,])), 1)
	f_net_h = architecture$f.net(net_h)

	# Submitting the hidden layer outputs as inputs for the output layer
	net_o = architecture$layers$output %*% c(f_net_h, 1)
	f_net_o = architecture$f.net(net_o)

	# Here we have the final results produced by the MLP
	ret = list()
	ret$f_net_h = f_net_h
	ret$f_net_o = f_net_o

	return (ret)
}

# This function is responsible for training, i.e., adapting weights and thetas
# for every neuron (or unit) at the hidden and the output layer. It basically
# applies the Gradient Descent Method.
backpropagation <- function(architecture, dataset, eta=0.1, threshold=1e-3) {

	x = matrix(dataset[,1:architecture$input.layer.size], ncol=architecture$input.layer.size)
	y = matrix(dataset[,(architecture$input.layer.size+1):ncol(dataset)], nrow=nrow(x))

	cat("Input data...\n")
	print(x)

	cat("Expected output...\n")
	print(y)

	cat("Enter to start running...")
	readline()

	squared_error = threshold * 2

	# This loop will run until the average squared error is below
	# some threshold value.
	while (squared_error > threshold) {

		# Initializing the squared error to measure the loss for
		# all examples in the training set
		squared_error = 0

		# For every example at index (row) p
		for (p in 1:nrow(x)) {

			# Applying the input example at index p
			f = forward(architecture, dataset, p)

			# Getting results to adapt weights and thetas
			error = (y[p,] - f$f_net_o)

			# Computing term delta for the output layer
			# which simplifies next computations involved 
			# in the Gradient Descent method
			delta_o = error * f$f_net_o * (1-f$f_net_o)

			# This is the squared error used as stop criterion.
			# Term sum(error^2) is used because the last layer
			# (i.e., the output layer) may have more than a single
			# neuron. We also use a power of two to ensure negative
			# and positive values do not nullify each other.
			squared_error = squared_error + sum(error^2)

			# Computing term delta for the hidden layer
			w_o = architecture$layers$output[,1:architecture$hidden.layer.size]
			delta_h = (f$f_net_h * (1 - f$f_net_h)) * sum(as.vector(delta_o) * as.vector(w_o))

			# Adapting weights and thetas at the output layer
			architecture$layers$output = 
				architecture$layers$output + eta * delta_o %*% c(f$f_net_h, 1)

			# Adapting weights and thetas at the hidden layer
			architecture$layers$hidden =
				architecture$layers$hidden + eta * delta_h %*% c(x[p,], 1)
		}

		# Dividing the total squared error by nrow to find the average
		# which we decided to use as stop criterion
		squared_error = squared_error / nrow(x)

		# Printing the average squared error out
		cat("Squared error = ", squared_error, "\n")
	}
	
	# Returning the trained architecture, which can now
	# be used for execution.
	return (architecture)
}

# This function is used to test the MLP
mlp.test <- function(architecture, dataset, debug=T) {

	# Orgazining the dataset as input examples x
	x = matrix(dataset[,1:architecture$input.layer.size], ncol=architecture$input.layer.size)
	# Orgazining the dataset as expected classes y associated to input examples x
	y = matrix(dataset[,(architecture$input.layer.size+1):ncol(dataset)], nrow=nrow(x))

	cat("Enter to start testing...")
	readline()

	output = NULL

	# For every example at index (row) p
	for (p in 1:nrow(x)) {

		# Applying the input example at index p
		f = forward(architecture, dataset, p)

		# If debug is true, show all information regarding classification
		if (debug) {
			cat("Input pattern = ", as.vector(x[p,]), 
			    " Expected output = ", as.vector(y[p,]), 
				" Obtained output = ", as.vector(f$f_net_o), "\n")
		}

		# Concatenating all output values as rows in a matrix,
		# so we can check them out later.
		output = rbind(output, as.vector(f$f_net_o))
	}

	# Returning results
	return (output)
}

# This function is useful to produce a discrete (either yes or no) hyperplane
# to shatter the input space of examples
discretize.hiperplane <- function(img, range = c(0.45, 0.55)) {
	ids_negative = which(img < range[1])
	ids_positive = which(img > range[2])
	ids_hiperplane = which(img >= range[1] & img <= range[2])

	img[ids_negative] = 0
	img[ids_positive] = 1
	img[ids_hiperplane] = 0.5

	img
}

# This is a function to train and test the XOR problem
xor.test <- function(eta=0.1, threshold=1e-3) {

	# Loading the dataset "xor.dat"
	dataset = as.matrix(read.table("xor.dat"))

	# Building up the MLP architecture with random weights and thetas.
	# Observe we have two units at the input layer (what is the number
	# of input variables), we have two units at the hidden layer (so we
	# will have two hyperplanes to shatter the space of examples as
	# expected), and we have a single unit at the output layer to provide
	# the answer as 0 or 1 (actually values in range [0,1])
	model = mlp.architecture(input.layer.size = 2, hidden.layer.size = 2, output.layer.size = 1, f.net = f)

	# Now we train the architecture "model" to build up the "trained.model"
	trained.model = backpropagation(model, dataset,eta=eta, threshold=threshold)

	# Then we test the "trained.model" using the same XOR dataset.
	# For more complex problems, we will use unseen examples.
	mlp.test(trained.model, dataset)

	# Building up hyperplanes to plot
	x = seq(-0.1,1.1,length=100)
	hiperplane_1 = outer(x,x, function(x,y) { cbind(x,y,1) %*% trained.model$layers$hidden[1,] } )
	hiperplane_2 = outer(x,x, function(x,y) { cbind(x,y,1) %*% trained.model$layers$hidden[2,] } )

	cat("Press enter to plot both hiperplanes...")
	readline()

	# Plotting the hyperplanes built at the hidden layer
	filled.contour(discretize.hiperplane(hiperplane_1) + discretize.hiperplane(hiperplane_2))
}

