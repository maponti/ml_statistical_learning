
n = 1:1000
delta = 0.01

# Using the SVM Generalization Bound
Radius = 7.386189
rho = 4.242641
R_f_SVM_Generalization_Bound = sqrt(3.28275/n * log(n) + 18.42068/n)

# Approximating the Shattering coefficient using the common Generalization Bound
Shattering_SVM = 1/3 * n * (n^2 - 3*n + 8)
R_f_common_Generalization_Bound = sqrt(-4/n * (log(delta) - log(Shattering_SVM)))

plot(R_f_SVM_Generalization_Bound, t="l")
lines(R_f_common_Generalization_Bound, col=2)

