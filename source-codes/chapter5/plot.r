library(calibrate)

#dataset = cbind(rnorm(mean=-10, sd=1, n=5), rnorm(mean=-10, sd=1, n=5))
#dataset = rbind(dataset, cbind(rnorm(mean=10, sd=1, n=5), rnorm(mean=10, sd=1, n=5)))
#dataset = cbind(dataset, c(rep(-1, 5), rep(1, 5)))
#
## Setting nearby points to make the ideal hyperplane more obvious
#dataset[1,1:2] = c(-7,-7)
#dataset[6,1:2] = c(7,7)

dataset = as.matrix(read.table("plot.dat"))

plot(dataset[,1:2], col=dataset[,3]+2)
for (i in 1:nrow(dataset)) {
	textxy(dataset[i,1]+dataset[i,3]*0.1, dataset[i,2]+dataset[i,3]*0.1, i)
}

write.table(dataset, "plot.dat", row.names=F, col.names=F)


alphas = c(0.005102041, 0, 0, 0, 0, 0.005102041, 0, 0, 0, 0)
y = dataset[,3]

print(colSums(alphas * y * dataset[,1:2]))
