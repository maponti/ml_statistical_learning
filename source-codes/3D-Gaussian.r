source("mlp.r")

for (sample.size in seq(1000, 1000000, step=10000)) {
	sigma <- matrix(c(4,2,2,3), ncol=2)
	P_X_Y.sample <- rmvnorm(n=sample.size, mean=c(1,2), sigma=sigma)

	# Here we train MLP to infer classifier f
	model = mlp.architecture(input.layer.size = 1, 
				 hidden.layer.size = 5, 
				 output.layer.size = 1, 
				 f.net = f)
	trained.model = backpropagation(model, P_X_Y.sample, 
					eta=0.1, threshold=1e-3)
	outcomes = mlp.test(trained.model, P_X_Y.sample, debug=T)

	# Then we have the empirical risk for classifier f
	Y = P_X_Y.sample[,2]
	R_emp_f = sum((outcomes - Y)^2)

	# This is the expected risk for f
	# ???????? PRECISO PENSAR MAIS AQUI

}

