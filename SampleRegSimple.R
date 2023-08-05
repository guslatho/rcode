#Quick sample code for easy simple bivariate regression function

SampleRegSimple <- function(x,y,p){

	#~ functie voor afronden

	r3 <- function(x){return(round(x,digits=3))}

	#~ vars definieren: mean, samplelength, sd berekenen

	xvar <- x			
	yvar <- y

	xmean <- mean(xvar)	
	ymean <- mean(yvar)

	samplen <- length(xvar)	

	xsd <- sqrt(  (sum((xvar-mean(xvar))^2))/(samplen-1)  )
	ysd <- sqrt(  (sum((yvar-mean(yvar))^2))/(samplen-1)  )

	#~ zscores van x en y berekenen en in vectors zetten. correlatie berekenen, tscore

	xzsc <- (xvar-xmean)/xsd 
	yzsc <- (yvar-ymean)/ysd

	cor <-         (sum(xzsc*yzsc)) / (samplen-1)        
	t <- round(    (cor*sqrt(samplen-2))/sqrt(1-(cor^2)) ,digits=3)
	tcv <- round(  qt(p/2,samplen-1,lower.tail=T)        ,digits=3)
	r2 <- 	   cor^2				           	

	#~ regressie ss ms en df berekenen. sst= alle variatie in y, ssreg = variatie verklaard in y door x

	sst <- var(yvar)*(samplen-1)		  
	ssreg <- r2*sst					 
	ssres <- sst-ssreg

	dft <- samplen-1
	dfreg <- 1
	dfres <- samplen-1-1		#let op: eerste 1 is aantal var (1 in simple), 2e is standaard -1

	msreg <- ssreg/dfreg
	msres <- ssres/dfres

	freg <- msreg/msres

	#~ coefficienten
	
	b <- sum((x-mean(x))*(y-mean(y)))/sum((x-mean(x))^2)
	a <- mean(y)-(b*mean(x))
	beta <- b*(xsd/ysd)
	
	#~ afronden
	cor <-r3(cor)
	t <-r3(t)
	tcv <-r3(tcv)
	r2 <-r3(r2)

	sst <-r3(sst)
	ssreg <-r3(ssreg)
	ssres <-r3(ssres)
	msreg <-r3(msreg)
	msres <-r3(msres)
	freg <-r3(freg)

	b <-r3(b)
	a <-r3(a)
	beta <-r3(beta)

	results <- paste("*gSimpleReg test results*\n",
			     " CORRELATION RESULTS\n",
			     "r=", cor, "\n",
			     "given t=", t, "the alternative hypothesis of p<",p," returns as ", t>tcv, "(tcv=",tcv,")\n",
			     "R2 (r squared)=", r2, "\n",
			     "sst=",sst,"ssregression=",ssreg,"ssresidual=",ssres,"\n",
			     " REGRESSION RESULTS\n",
			     "ss regression is",ssreg,", ss residual is",ssres,", ss totaled is",sst,",\n",
			     "ms regression is",msreg,", ms residual is",msres,", F value is",freg,",\n",
			     " COEFFICIENTS\n",
			     "a:",a,"b:",b,"beta:",beta,"\n",
				"\n")
	

	return(cat(results))
	

}