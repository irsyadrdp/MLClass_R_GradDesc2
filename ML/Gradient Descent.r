# fungsi error cost
cost <- function(x, y, theta){
	sum( (x %*% theta - y)^2 ) / (2*length(y))
}

# tingkat pembelajaran & batas iterasi
alpha <- 0.01
iterasi <- 1000

# keep history
temp_cost <- double(iterasi)
temp_theta <- list(iterasi)

# inisialisasi koefisien
theta <- matrix(c(0,0), nrow=2)

# koefisien intercept
X <- cbind(1, matrix(x))

# gradient descent
for (i in 1:iterasi) {
	error <- (X %*% theta - y)
	delta <- t(X) %*% error / length(y)
	theta <- theta - alpha * delta
	temp_cost[i] <- cost(X, y, theta)
	temp_theta[[i]] <- theta
}

# plot data
plot(x,y, col=rgb(0.2,0.4,0.6,0.4), main='Linear regression dengan gradient descent')
for (i in c(1,3,6,10,14,seq(20,iterasi,by=10))) {
	abline(coef=temp_theta[[i]], col=rgb(0.8,0,0,0.3))
}
abline(coef=theta, col='blue')

#plot error cost
plot(temp_cost, type='line', col='blue', lwd=2, main='Fungsi Cost', ylab='Cost', xlab='Iterasi')
