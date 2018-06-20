
# Sample size variation
n = 1:1000
delta=0.01

# Empirical risks
R_emp_f_5_MLP = 0.05
R_emp_f_10_MLP = 0.04
R_emp_f_SVM = 0.1

# Shattering coefficients
Shattering_5_MLP = 2 *(1/32 *(n - 2)^5 * (n - 1)^5 + (n - 1)^5 + 1)
Shattering_10_MLP = 2 *(((n - 2)^10 * (n - 1)^10)/1024 + (n - 1)^10 + 1)
Shattering_SVM = 1/3 * n * (n^2 - 3*n + 8)

# Computing the Generalization Bounds
R_f_5_MLP = R_emp_f_5_MLP + sqrt(-4/n * (log(delta) - log(Shattering_5_MLP)))
R_f_10_MLP = R_emp_f_10_MLP + sqrt(-4/n * (log(delta) - log(Shattering_10_MLP)))
R_f_SVM = R_emp_f_SVM + sqrt(-4/n * (log(delta) - log(Shattering_SVM)))

plot(R_f_5_MLP, t="l", col=1, ylim=c(0, max(c(R_f_5_MLP, R_f_10_MLP))))
lines(R_f_10_MLP, col=2)
lines(R_f_SVM, col=3)

