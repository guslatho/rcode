#gFiveNumber. This module was created to calculate a 5-number summary of any vector input given. 
#To use it, use the gFiveNumber function with any vector data as the x argument.

gFiveNumber<-function(x){

	cat("~~~ gFiveNumber v.1 (gusFiveNumberSummary version 0.1)~~~\n")
	cat("Built by Gus Lathouwers, thank you for trying this module!\n")
	cat("Starting calculations...\n")
	
	x<-sort(x)
	
	q0<-x[1]
	q1<-0						#semi-quartile range (p75)
	q2<-0 					#median
	q3<-0						#semi-quartile range (p75)
	q4<-x[length(x)]			
	medp<-length(x)/2				#median position in vector
	quap<-length(x)/4				#check for interquartile

	if(medp%%1!=0){ 				#if vector is uneven
		q2<-x[medp+0.5]
	} else {					#if vector is even
		q2<-(x[medp]+x[medp+1])/2
	}
	cat("Median calculated successfully.\n")

	if(quap%%1==0.5||quap%%1==0.75){	#p25 is uneven (1 number return)	
		q1<-(x[round(quap+0.5)])
		q3<-(x[round(quap+medp+0.5)])
	} else {					#p25 is even (average 2 number return)
		q1<-(x[round(quap)]+x[(round(quap))+1])/2
		q3<-(x[round(quap+medp)]+x[round(quap+medp+1)])/2
	}
	iqr<-q3-q1

	cat("Quartiles calculated successfully.\n")

	cat("All data processed successfully. Output is as follows:\n")
	cat("P0  (Q0):",q0,"\n")
	cat("P25 (Q1):",q1,"\n")
	cat("P50 (Q2):",q2,"\n")
	cat("P75 (Q3):",q3,"\n")
	cat("P100(Q4):",q4,"\n")
	cat("IQR     :",iqr,"\n")

	
}

#Random vectors to test:
#gFiveNumber(c(1,2,3,4,5,6,7))
#gFiveNumber(c(2.5,3.3,5,5,7,5,6.6,5.2123))
#gFiveNumber(c(5,5,4.2222,104,104,104))