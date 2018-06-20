
n = 1:1000
delta = 0.01
nu_f = 0

# Using the SVM Generalization Bound for the perfectly separable training set
Radius = 104.4398
rho = 41.08596
R_f_SVM_Generalization_Bound_Perfect = nu_f + sqrt(0.4949589/n * log(n) + 18.42068/n)

# Using the common Generalization Bound for the 5-hyperplane MLP on the original 2-dimensional input space
Shattering_5_MLP = 2 *(1/32 *(n - 2)^5 * (n - 1)^5 + (n - 1)^5 + 1)
R_f_5_MLP_common_Generalization_Bound = nu_f + sqrt(-4/n * (log(delta) - log(Shattering_5_MLP)))

plot(R_f_SVM_Generalization_Bound_Perfect, t="l")
lines(R_f_5_MLP_common_Generalization_Bound, col=2)
