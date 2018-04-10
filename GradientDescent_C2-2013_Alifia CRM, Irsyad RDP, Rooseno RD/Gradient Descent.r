####################################################################################################
#LEARNING DATA
####################################################################################################

###Baca file, normalisasi data file, deklarasi x,y,m(banyak data),alpha dan iterasi
	dataRumah = read.table(file="Z:/TEMP FD/Study/ML/Tugas ML Gradient Descent [desktop version]/GradientDescent_C2-2013_Alifia CRM, Irsyad RDP, Rooseno RD/datasets.txt", sep="\t", header=TRUE)	
	##semua data sudah dalam numeric dan integer
	
	##Feature Scalling
	dataNormal = apply(dataRumah, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))
		
	x = dataNormal [ , 1 : ncol(dataNormal)-1] 	#data input
	y = dataNormal [ , ncol(dataNormal)]			#data output (harga)
	m = length(y)
	#alpha = 0.001
	#iterasi = 500
	alpha = 0.01
	iterasi = 500
	
###Inisialisasi nilai theta awal, nantinya akan berubah seiring iterasi/learning process berlangsung
	theta = matrix(runif(1,0,1), nrow = ncol(dataNormal))	#secara random
	#theta = matrix(0, nrow = ncol(dataNormal))				#di-nol-kan semua

###xnol & jacobian(square error function) & hypothesis function
	xnol = 1
	xinput = cbind(xnol,x)	
	## untuk dikali dengan theta0(theta0 selalu dikali 1 --> theta0*1+theta1*x1+....)
	
	##hypothesis function
	hypoFunc = function(xinput, theta){
		xinput %*% theta
	}
	#plot(hypoFunc(xinput, theta), type='line', col='green', lwd=2, main='Y-->Harga', ylab='Harga', xlab='Iterasi')

	##square error function
	jacobian = function(xinput , y, theta) {
		sum( (xinput %*% theta - y)^2 ) / (2*m)
	}

###Gradient Descent & history error & plot error
	dataerror = double(iterasi)
	for (i in 1:iterasi) {
		theta = theta - alpha/m * (t(xinput) %*% (hypoFunc(xinput,theta) - y))
		dataerror[i] = jacobian(xinput, y, theta)
	}
	plot(dataerror, type='line', col='blue', lwd=2, main='MSE', ylab='error', xlab='iterasi')	
	#errornya sudah bagus karena berkurang terus dengan alpha=0.001 dan iterasi sebanyak 500 kali





####################################################################################################
#TES DATA
####################################################################################################

###Baca file data tes, normalisasi data tes
	dataTes = read.table(file="Z:/TEMP FD/Study/ML/Tugas ML Gradient Descent [desktop version]/GradientDescent_C2-2013_Alifia CRM, Irsyad RDP, Rooseno RD/datates.txt", sep="\t", header=TRUE)	
	dataTesNormal = apply(dataTes, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))	#feature scalling
	
	xtes = dataTesNormal [ , 1 : ncol(dataTesNormal)-1] 	#data tes input	
	mtes = length(xtes[,1]) #banyak data tes
	
	#x awal
	xnoltes = 1
	xinputtes = cbind(xnoltes,x)
	
	##cek data
	yHasil = double(m)
	cekData = function(xinputtes, theta){
		xinputtes %*% theta
	}
	yHasil = cekData(xinputtes, theta)
	
###bandingkan dengan dataset
	cbind(yHasil,hypoFunc(xinput,theta),y)
	
###denormalisasi
	terbesar <- max(dataRumah)
	terkecil <- min(dataRumah)
	fixYHasil <- sapply(yHasil, function(X) (X*(terbesar-terkecil)+terkecil))
	plot(fixYHasil, col='green', lwd=2, main='Hasil Cek Harga Data', ylab='Harga', xlab='Data ke')
	#harganya masih ada yg salah karena ada nilai yang minus
	
	fixYHasil