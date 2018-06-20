
n = 1:1000
delta = 0.01
nu_f = 0.075

# Using the SVM Generalization Bound for the perfectly separable training set
Radius = 7.386189
rho = 4.242641
R_f_SVM_Generalization_Bound_Perfect = nu_f + sqrt(3.28275/n * log(n) + 18.42068/n)

# Using the SVM Generalization Bound for the training set with some class overlapping
Radius = 3.886189
rho = 0.7071068
R_f_SVM_Generalization_Bound_Mix = nu_f + sqrt(62.17902/n * log(n) + 18.42068/n)

# Approximating the Shattering coefficient using the common Generalization Bound
Shattering_SVM = 1/3 * n * (n^2 - 3*n + 8)
R_f_common_Generalization_Bound = nu_f + sqrt(-4/n * (log(delta) - log(Shattering_SVM)))

plot(R_f_SVM_Generalization_Bound_Perfect, t="l")
lines(R_f_SVM_Generalization_Bound_Mix, col=2)
lines(R_f_common_Generalization_Bound, col=3)
