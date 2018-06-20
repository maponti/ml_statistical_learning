library(calibrate)

#dataset = cbind(rnorm(mean=-1, sd=1, n=5), rnorm(mean=-1, sd=1, n=5))
#dataset = rbind(dataset, cbind(rnorm(mean=1, sd=1, n=5), rnorm(mean=1, sd=1, n=5)))
#dataset = cbind(dataset, c(rep(-1, 5), rep(1, 5)))

dataset = as.matrix(read.table("plot2.dat"))

plot(dataset[,1:2], col=dataset[,3]+2)
for (i in 1:nrow(dataset)) {
	textxy(dataset[i,1]+dataset[i,3]*0.1, dataset[i,2]+dataset[i,3]*0.1, i)
}

