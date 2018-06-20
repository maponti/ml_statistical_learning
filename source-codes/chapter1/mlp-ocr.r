source("mlp.r")

# This MLP solves the Optical Character Recognition (OCR) problem.
ocr.test <- function(eta=0.1, threshold=1e-3) {

	# Loading the dataset
	dataset = as.matrix(read.table("ocr-asvector.dat"))

	# Loading a test set with unseen examples
	test.dataset = as.matrix(read.table("test-ocr-asvector.dat"))

	# Building up the architecture with 70 units at the input layer,
	# 5 units (so 5 hyperplanes) at the hidden layer and 2 at the output
	# layer.
	model = mlp.architecture(input.layer.size = 10*7, 
				 hidden.layer.size = 5, 
				 output.layer.size = 2, f.net = f)

	# Training
	trained.model = backpropagation(model, 
				dataset,
				eta=eta, 
				threshold=threshold)

	# Testing for unseen examples
	mlp.test(trained.model, test.dataset)
}

