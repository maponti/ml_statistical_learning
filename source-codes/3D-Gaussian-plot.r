require(mvtnorm)
require(ggplot2)
require(hexbin)

sigma <- matrix(c(4,2,2,3), ncol=2)
P_X_Y.sample <- rmvnorm(n=1000000, mean=c(1,2), sigma=sigma)
P_X_Y.sample = as.data.frame(P_X_Y.sample)
colnames(P_X_Y.sample) = c("X", "Y")
d <- ggplot(P_X_Y.sample, aes(X, Y))
d + geom_hex(bins=100)

