#Membaca data dari excel
	#dataPer <- read.xlsx("Dataset_Perumahan.xlsx", 1, header=FALSE, startRow = 3)
	dataPer = read.table(file="datasets.txt", sep="\t", header=TRUE)	

	#menghapus nama perumahan
	dataPer$X1 <- NULL 
	
#Cek klasifikasi seluruh variabel
	str(dataPer)
	
#Mengubah ke numerical data
#sapply -> ketika menginginkan fungsi diterapkan kesetiap elemen dan kembali sebagai vector
	dataPer <- sapply(dataPer, function(x)	if(is.factor(x)){
												as.numeric(x)
											}else{
												x
											})
			
#variabel tampungan			
terbesar <- max(dataPer)
terkecil <- min(dataPer)

#Normalisasi data
	dataPerNorm <- apply(dataPer, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))

#Deklarasi X & Y
x <- dataPerNorm[ , 1 : ncol(dataPerNorm) - 1] 
y <- dataPerNorm[ , ncol(dataPerNorm)]

#Koefisien intercept
X <- cbind(1, x)

#Theta
theta <- matrix(0, nrow = ncol(dataPerNorm))
	
#Learning rate & batas iterasi
alpha <- 0.01
iterasi <- 1000

# keep history
cost_history <- double(iterasi)
theta_history <- list(iterasi)

# fungsi error cost
cost <- function(X, y, theta) {
	sum( (X %*% theta - y)^2 ) / (2*length(y))
}

# Gradient Descent
for (i in 1:iterasi) {
	error <- (X %*% theta - y)
	delta <- t(X) %*% error / length(y)
	theta <- theta - alpha * delta
	cost_history[i] <- cost(X, y, theta)
	theta_history[[i]] <- theta
}

plot(cost_history, type='line', col='blue', lwd=2, main='Fungsi Cost', ylab='cost', xlab='Iterasi')

#Fungsi tes model
test <- function(theta, x){

	y = theta[1] + (theta[2] * x[1]) + (theta[3] * x[2]) + (theta[4] * x[3]) + (theta[5] * x[4]) + (theta[6] * x[5]) + (theta[7] * x[6])
		+ (theta[8] * x[7]) + (theta[9] * x[8]) + (theta[10] * x[9]) + (theta[11] * x[10]) + (theta[12] * x[11]) + (theta[13] * x[12])
		+ (theta[14] * x[13])
	
	return(y)
}

dataTes <- read.xlsx("Dataset_Perumahan.xlsx", 2, header=FALSE, startRow = 3)
	#menghapus nama perumahan
	dataTes$X1 <- NULL
	
	#sapply -> ketika menginginkan fungsi diterapkan kesetiap elemen dan kembali sebagai vector
	dataTes <- sapply(dataTes, function(x)	if(is.factor(x)){
												as.numeric(x)
											}else{
												x
											})
	
	#Normalisasi data
	dataTesNorm <- apply(dataTes, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))

	#hasil
	hasil <- matrix(0, nrow = nrow(dataTesNorm))
	
	#Mencocokan dengan model
	for (i in 1:nrow(dataTesNorm)){
		hasil[i] <- test(theta, dataTesNorm[i , ])
	}
	
	#Denormalisasi hasil
	hasil <- sapply(hasil, function(X) (X*(terbesar-terkecil)+terkecil))
	#Membaca data dari excel
	dataPer <- read.xlsx("Dataset_Perumahan.xlsx", 1, header=FALSE, startRow = 3)
	#menghapus nama perumahan
	dataPer$X1 <- NULL 
	
#Cek klasifikasi seluruh variabel
	str(dataPer)
	
#Mengubah ke numerical data
#sapply -> ketika menginginkan fungsi diterapkan kesetiap elemen dan kembali sebagai vector
	dataPer <- sapply(dataPer, function(x)	if(is.factor(x)){
												as.numeric(x)
											}else{
												x
											})
			
#variabel tampungan			
terbesar <- max(dataPer)
terkecil <- min(dataPer)

#Normalisasi data
	dataPerNorm <- apply(dataPer, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))

#Deklarasi X & Y
x <- dataPerNorm[ , 1 : ncol(dataPerNorm) - 1] 
y <- dataPerNorm[ , ncol(dataPerNorm)]

#Koefisien intercept
X <- cbind(1, x)

#Theta
theta <- matrix(0, nrow = ncol(dataPerNorm))
	
#Learning rate & batas iterasi
alpha <- 0.01
iterasi <- 1000

# keep history
cost_history <- double(iterasi)
theta_history <- list(iterasi)

# fungsi error cost
cost <- function(X, y, theta) {
	sum( (X %*% theta - y)^2 ) / (2*length(y))
}

# Gradient Descent
for (i in 1:iterasi) {
	error <- (X %*% theta - y)
	delta <- t(X) %*% error / length(y)
	theta <- theta - alpha * delta
	cost_history[i] <- cost(X, y, theta)
	theta_history[[i]] <- theta
}

plot(cost_history, type='line', col='blue', lwd=2, main='Fungsi Cost', ylab='cost', xlab='Iterasi')

#Fungsi tes model
test <- function(theta, x){

	y = theta[1] + (theta[2] * x[1]) + (theta[3] * x[2]) + (theta[4] * x[3]) + (theta[5] * x[4]) + (theta[6] * x[5]) + (theta[7] * x[6])
		+ (theta[8] * x[7]) + (theta[9] * x[8]) + (theta[10] * x[9]) + (theta[11] * x[10]) + (theta[12] * x[11]) + (theta[13] * x[12])
		+ (theta[14] * x[13])
	
	return(y)
}

#dataTes <- read.xlsx("Dataset_Perumahan.xlsx", 2, header=FALSE, startRow = 3)
dataTes = read.table(file="datasets.txt", sep="\t", header=TRUE)
	#menghapus nama perumahan
	dataTes$X1 <- NULL
	
	#sapply -> ketika menginginkan fungsi diterapkan kesetiap elemen dan kembali sebagai vector
	dataTes <- sapply(dataTes, function(x)	if(is.factor(x)){
												as.numeric(x)
											}else{
												x
											})
	
	#Normalisasi data
	dataTesNorm <- apply(dataTes, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))

	#hasil
	hasil <- matrix(0, nrow = nrow(dataTesNorm))
	
	#Mencocokan dengan model
	for (i in 1:nrow(dataTesNorm)){
		hasil[i] <- test(theta, dataTesNorm[i , ])
	}
	
	#Denormalisasi hasil
	hasil <- sapply(hasil, function(X) (X*(terbesar-terkecil)+terkecil))
	plot(hasil, col='blue', lwd=2, main='Hasil Perkiraan Sesuai Model', ylab='harga', xlab='Perumahan')