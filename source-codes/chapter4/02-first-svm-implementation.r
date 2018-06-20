
find_w_b <- function(alphas, x, y) {

	# Computing vector w
	w = 0
	for (i in 1:length(alphas)) {
		w = w + alphas[i] * y[i] * x[i,]
	}

	# Computing term b
	positive_support_vector = x[y == +1,]
	negative_support_vector = x[y == -1,]
	b = 1/2* ((positive_support_vector + negative_support_vector) %*% w)

	cat("Vector w\n")
	print(w)

	cat("Real number b\n")
	print(b)

	ret = list()
	ret$w = w
	ret$b = b

	ret
}

classify <- function(w, b, newX) {
	ret = w %*% newX + b
	ret
}

x = NULL
x = rbind(c(+1, +1))
x = rbind(x, c(-1, -1))

y = c(+1, -1)

alphas = c(0.25, 0.25)

W_of_alpha = 0
for (i in 1:2) {
	for (j in 1:2) {
		W_of_alpha = W_of_alpha + alphas[i]*alphas[j]*y[i]*y[j]*x[i,]%*%x[j,]
	}
}

result = -1/2 * W_of_alpha + sum(alphas)

cat("This is the result for W(alpha), which we want to maximize!\n")
print(result)

find_w_b(alphas, x, y)

