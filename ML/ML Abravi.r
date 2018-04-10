
#buka file csv
dataX = read.table(file="daftar harga2.csv", sep=",", header=TRUE)
dataX
#hypotesys function
hypoFunc = function(baris){
	x1=baris[1]; x6=baris[6];
	x2=baris[2]; x7=baris[7]; 
	x3=baris[3]; x8=baris[8];
	x4=baris[4]; x9=baris[9];
	x5=baris[5]; x10=baris[10];
	
	#runif(14, 0, 1);
	x11=baris[11];
	x12=baris[12];
	x13=baris[13];
	x14=baris[14];
	
	t1=runif(1,0,1)
	t2=runif(1,0,1)
	t3=runif(1,0,1)
	t4=runif(1,0,1)
	t5=runif(1,0,1)
	t6=runif(1,0,1)
	t7=runif(1,0,1)
	t8=runif(1,0,1)
	t9=runif(1,0,1)
	t10=runif(1,0,1)
	t11=runif(1,0,1)
	t12=runif(1,0,1)
	t13=runif(1,0,1)
	t14=runif(1,0,1)
	
	hTheta =(t1*x1) + (t2*x3) + (t3*x4) + (t4*x5) + (t5*x6) + (t6*x7) + (t8*x8) + (t9*x9)
			+ (t10*x10) + (t11*x11) + (t12*x12) + (t13*x13) +(t14*x14)
			
	return (hTheta)
	
	}
	

#fiture scaling ( skala 0 - 1 f)	
fScale <- function(x)
	{
		apply(x, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))
	}	
	
dataX=fScale(dataX)
	###JACOBIAN
	
	jacobian <- function()
	{
		sumArray = 0
		m = nrow(dataX)
		for(i in 1:m)
		{	
		Y = dataX[i, 15]
			hTheta = hypoFunc(dataX[i,])
			min_hTheta = (hTheta-Y)^2
			
			sumArray = sumArray + min_hTheta 
		}
		
		jacob = (1/(2*m))*sumArray
		return(jacob)
	}
	
	dataX
	Error=0.5
	Error = jacobian()
	Error
	
	akurasi = 100 - jacobian()
	akurasi
	

finding<-function(Error){	
ma = 50
stepErr <- c(1:100)
i=0
	while(Error>1.5)
	{
		Error = jacobian()
		print(Error)
		
		akurasi = 100 - jacobian()
		print(akurasi)
		stepErr[i]=Error
		i = i + 1
	}
	
	print(stepErr)
	plot(stepErr)
}	
	
err =	finding(Error)
