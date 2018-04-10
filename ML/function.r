#1. Luas bangunan	(5)
#2. Luas tanah 			(5)
#3. Jumlah lantai		(4)
#4. Lokasi					(3)
#5. Tipe pengamanan	(2)
		#6. Pagar					(1)
#7. Bentuk tanah		(1)
		#8. Lebar jalan			(1)
#9. Daya listrik			(1)
#10. Carport				(2)
#11. Material				(3)
#12. Air						(3)
#13. Jumlah kamar	(4)
#14.	Jumlah WC		(2)

### Data rumah yang diambil dari file
dataRumah = read.table(file="datasets.txt", sep="\t", header=TRUE)

### HYPHOTESIS FUNCTION
	hyphotesisFunc <- function(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12)
	{
		hTheta = ((5*x1)+(5*x2)+(4*x3)+(3*x4)+(2*x5)+(1*x6)+(1*x7)+(2*x8)+(3*x9)+(3*x10)+(4*x11)+(2*x12))
		return (hTheta)	
	}


### FEATURES SCALLING
	
	featuresScalling <- function(x,i)
	{
		SD = sd(0:1)									#standar deviasi
		CM = colMeans(dataRumah[i])		#mean per colom

		FS = (x - CM) /SD
		return(FS)
	}
	
###JACOBIAN
	jacobian <- function(m)
	{
		sumArray = 0
		
		for(i in 1:m)
		{
			luasBangunan = featuresScalling(dataRumah[i,1],1)
			luasTanah = featuresScalling(dataRumah[i, 2],2)
			jumlahLantai = featuresScalling(dataRumah[i, 3],3)
			lokasi = featuresScalling(dataRumah[i, 4],4)
			pengamanan = featuresScalling(dataRumah[i, 5],5)
			bentukTanah = featuresScalling(dataRumah[i, 6],6)
			dayaListrik = featuresScalling(dataRumah[i, 7],7)
			carport = featuresScalling(dataRumah[i, 8],8)
			material = featuresScalling(dataRumah[i, 9],9)
			air = featuresScalling(dataRumah[i, 10],10)
			kamar = featuresScalling(dataRumah[i, 11],11)
			wc = featuresScalling(dataRumah[i, 12],12)
			
			Y = dataRumah[i, 13]
			hTheta = hyphotesisFunc(luasBangunan, luasTanah, jumlahLantai, lokasi, pengamanan, bentukTanah, dayaListrik, carport, material, air, kamar, wc)
			min_hTheta = (hTheta-Y)*(hTheta-Y)
			
			sumArray = sumArray + min_hTheta 
		}
		
		jacob = (1/(2*m))*sumArray
		return(jacob)
	}
	
	
	