# This package is required to find matrix inverses (function ginv).
# The use of inverses is only for didactical purposes.
require(MASS)

########## PRIMAL FORM ###########

# Defining vector c to multiply 2*x1+3*x2+0*x3+0*x4
c = matrix(c(2,3,0,0), nrow=1)

# Defining matrix A which defines the constraint functions:
#
#  2*x1 +   x2 + x3 = 8
#    x1 + 2*x2 + x4 = 6
#
A = matrix(c(2,1,1,0,
	     1,2,0,1), nrow=2, byrow=T)

# Defining the right-side terms for constraint functions
b = matrix(c(8,6), ncol=1)

# Defining some initialization for x1, x2, x3 and x4.
# This is made respecting the constraint functions
# for the primal form as follows:
#
# i) We simply decided to set x1=1, which respects the constraint x1 >=0
# ii) Then, we applied x1=1 into the first constraint function:
#
#	2*x1 +   x2 + x3 = 8
#	2*(1)+   x2 + x3 = 8
# iii) So, we also decided to set x2=1 (respecting the constraint x2>=0), thus:
#
#	2*(1)+   x2 + x3 = 8
#	2*(1)+  (1) + x3 = 8
#	3 + x3 = 8
#	x3 = 8 - 3 = 5
#
# iv) Allowing us to find x3=5, which also respects the constraint x3 >= 0
#
# v) Next, we had to find an acceptable value for x4 which must be x4 >= 0,
#    as defined by the primal form. So we got the second constraint function:
#
#    x1 + 2*x2 + x4 = 6
#
#    Given x1=1, x2=1 and x3=5 we found:
# 
#    x1  + 2*x2  + x4 = 6
#    (1) + 2*(1) + x4 = 6
#    3 + x4 = 6
#    x4 = 6 - 3 = 3
#
# vi) Next, we organized x1, x2, x3 and x4 in a diagonal matrix as follows.
X = diag(c(1,1,5,3))

########## DUAL FORM ###########

# From the dual side, we had to set pi1, pi2. For that, we simply selected
# two values as follows:
Pi = matrix(c(2,2), ncol=1)

# Then, we ensured the constraint functions for the dual form were respected.
#
# i) First constraint function:
#
#	2*pi1 + pi2 - z1 = 2
#	2*(2) + (2) - z1 = 2
#	6 - z1 = 2
#	- z1 = 2 - 6
#	z1 = 4
#
#	respecting the constraint z1 >= 0, which is defined for this dual form.
#
# ii) Second constraint function:
#
#	pi1 + 2*pi2 - z2 = 3
#
#	substituing pi1=2 and pi2=2, we have:
#
#	(2) + 2*(2) - z2 = 3
#	6 - z2 = 3
#	- z2 = 3 - 6
#	z2 = 3
#
#	respecting the constraint z2 >= 0, which is defined for this dual form.
#
# iii) Third constraint function:
#
#	pi1 - z3 = 0
#	(2) - z3 = 0
#	z3 = 2
#
#	respecting the constraint z3 >= 0, which is defined for this dual form.
#
# iv) Fourth constraint function:
#
#	pi2 - z4 = 0
#	(2) - z4 = 0
#	z4 = 2
#
#	respecting the constraint z4 >= 0, which is defined for this dual form.
#
# v) Next, we simply set the diagonal matrix Z with those values we found (remember
#    they respect all constraint functions, otherwise we would not obtain the solution).
Z = diag(c(4,3,2,2))

# Starting mu with some positive value.
mu = 1.4375

# Defining a column vector filled with 1s.
e = matrix(rep(1,4), ncol=1)

# Variable eta defines the rate of change for the primal and dual variables along iterations.
eta = 0.995

# Defining vectors Delta_x, Delta_pi and Delta_z. Combined they define the column vector
# on which the Jacobian matrix will be applied to.
dX = rep(0, 4)
dPi = rep(0, 2)
dZ = rep(0, 4)

# Setting a counter to know the current iteration of this algorithm.
counter = 1

# Defining a stop criterion. While the gap term is greater than such threshold,
# this algorithm keeps running, otherwise it will stop and print the solution out.
threshold = 1e-5

# Computing the current gap term for the solution we defined, i.e., for the current
# values of x1, x2, x3 and x4 in the primal form and z1, z2, z3 and z4 in the dual.
gap = t(e) %*% X %*% Z %*% e

# While the gap is greater than acceptable, run:
while (gap > threshold) {

	# Printing out the current iteration and the gap
	cat("Iteration: ", counter, " with Gap = ", gap, "\n")

	# Solving the linear system of equations
	deltaD = t(A)%*%dPi - dZ
	dPi = ginv(A%*%ginv(Z)%*%X%*%t(A))%*%(-b+mu*A%*%ginv(Z)%*%e+A%*%ginv(Z)%*%X%*%deltaD)
	dZ = -deltaD + t(A)%*%dPi
	dX = ginv(Z)%*%(e%*%mu-X%*%Z%*%e-X%*%dZ)

	# Changing variables for the next iteration (only the diagonal in here).
	# The algorithm walks according to the gradient vector
	X = X + eta * diag(as.vector(dX))
	Pi = Pi + eta * dPi
	Z = Z + eta * diag(as.vector(dZ))

	# Computing the gap again to verify if we will carry on running
	gap = t(e) %*% X %*% Z %*% e

	# Reducing the influence of the barrier term, so we can get closer to a vertex
	# if the solution is eventually there (in this case, it is!)
	mu = as.numeric(gap / counter^2)

	# Counting the number of iterations
	counter = counter + 1
}

cat("Constraint functions must be equal to zero:\n")

cat("Primal feasibility:\n")
print(A%*%diag(X)-b)

cat("Dual feasibility:\n")
print(t(A)%*%Pi-diag(Z)-t(c))

cat("u-complementary slackness:\n")
print(diag(X)%*%Z-mu)

cat("Values found for X:\n")
print(diag(X))

cat("Values found for Z:\n")
print(diag(Z))

