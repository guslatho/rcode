#~~~ gAnovRep, gusAnovaRepeated, v.01 ~~~
#Thank you for trying this module!

#INSTRUCTIONS
#For the gAnovRep function to run, at least three variabels are mandatory:
#-subjectid: A list with the subjectid corresponding to each observation
#-xvar1: The first independent variabel (1 independent variabel must be submitted), can be within or between
#-yvar: The dependent variabel
#Additionally, a second variabel can be submitted optionally (can be either a within or between variabel)

#SAMPLE DATA SET 
#A sample dataset is included below for reference:
#ganovtest<-data.frame(id=factor(c(rep("1",times=3),rep("2",times=3),rep("3",times=3),rep("4",times=3),rep("5",times=3),rep("6",times=3))),
#			 day=c(rep(c(1,2,3),times=6)),
#			 week=factor(c(rep(c(0),times=9),rep(c(1),times=9))),
#			 score=c(42,42,45,36,41,43,39,35,40,51,55,59,45,50,55,44,49,56))

#FUTURE AIMS
#To be included is functionality for sets containing missing values, unbalanced designs, support for covariance

#FEEDBACK
#All feedback can be submitted at guslathouwers@gmail.com

gAnovRep <- function (subjectid="absent", xvar1="absent", xvar2="absent", yvar="absent", alpha=0.05){

	#~ THIS MODULE DOES NOT REQUIRE ANY OUTSIDE LIBRARIES TO FUNCTION
	#~ To start, define a number of small functions used throughout the code, one for rounding, one for shorthand p value notation, one for converting vector to factor

	r2<-function(x){return(round(x,digits=2))}
	gp<-function(x){if(x<0.001){return("<0.001")}else{return(round(x,digits=3))}}
	gSupFact <- function(fact){dumfact<-fact;if(is.factor(fact)==F){dumfact<-factor(dumfact)};return(dumfact)}

	#~ Define variables that are easier to manage
	#~ Define the error variable: vector containing different strings. Any error encountered is coded into the vector

	subj <- subjectid
	x1 <- xvar1
	x2 <- xvar2
	y <- yvar
	error <- rep("0",times=10)

	#~ ~~~~~~~ ERROR CHECK BLOCK ~~~~~~~~
	#~ Error checks. First one checks for NA values (not supported currently). Subsequent error checks are for making sure submitted variabel is factor/vector, balanced design check

	if(any(is.na(y)==T)||any(is.na(x1)==T)||any(is.na(x2)==T)||any(is.na(subj)==T)){error[1]<-"-gAnovRep does not currently support missing values!\n"
		y[which((is.na(y))==T)]<-0
		x1[which((is.na(x1))==T)]<-0
		x2[which((is.na(x2))==T)]<-0
		subj[which((is.na(subj))==T)]<-0
	}
	if((!is.vector(x1)&&!is.factor(x1))||(length(x1)<=1)||(x1[1]=="absent")){error[2]<-"-Independent variable 1 (xvar1) is missing or incorrectly defined. The first independent variable (xvar1) must always be defined as either a vector or factor, and must contain at least 2 observations (n<1).\n"}
	if((!is.vector(subj)&&!is.factor(subj))||(length(subj)<=1)||(subj[1]=="absent")){error[3]<-"-Subject number variable (subjectid) is missing or incorrectly defined. A subject id variable must be entered as a vector or factor, and must contain at least 2 observations (n<1).\n"}
	if(((!is.vector(x2)&&!is.factor(x2))||(length(x2)<=1))&&(x2[1]!="absent")){error[4]<-"-Independent variable 2 (xvar2) is incorrectly defined! Independent variables must be input in the vector or factor format, and must contain at least 2 observations (n<1).\n"}
	if((!is.vector(y)&&!is.factor(y))||(length(y)<=1)||(y[1]=="absent")){error[5]<-"-The dependent variable (yvar) is missing or incorrectly defined! The dependent variable must be formatted as a vector or factor, and must contain at least 2 observations (n<1).\n"}
	if((((length(y)!=length(x1))&&(x1[1]!="absent"))||((length(y)!=length(subj))&&(subj[1]!="absent"))||((x2[1]!="absent")&&(length(y)!=length(x2))))&&(y[1]!="absent")){error[6]<-"-One or more variables that was input do not have the same number of observations.\n"}

	#~ Apply supfact to recode any vectors that aren't factor into factor format. Subsequently perform check for design balance
	subj <- gSupFact(subj)
	x1 <- gSupFact(x1)
	x2 <- gSupFact(x2)

	balancedlist <- list(subj, x1, x2)
	balancedflag <- T	

	for(s in 1:3){
		if(balancedlist[[s]][1]=="absent"){next}  #Skip the current loop if variabel is absent
		currentitem <- balancedlist[[s]]
		itemlevels <- length(levels(currentitem))
		for(b in 1:(itemlevels-1)){
			levellength <- length (currentitem[currentitem==levels(currentitem)[b]])
			subseqlength <- length (currentitem[currentitem==levels(currentitem)[b+1]])
			if(levellength!=subseqlength){balancedflag=F}
		}		
	}

	if(balancedflag==F){error[6]<-"-Condition entry length of one or more variables differs, gAnovRepeat\n currently only supports balanced design calculations.\n"}

	#~ Error check completed. Remove all non-errors (aka "0"-entries) in the error vector. Function terminates if any errors are present
	error<-error[-which(error==0)]
	
	if(any(error!=0)){
		return(cat("~~~ gAnovRep v.1 (gusAnovaRepeated version 0.1)~~~\n(Built by Gus Lathouwers, thank you for trying this module!)\nOne or more errors occurred when processing input variables.\nThe following error(s) have occurred:\n",
			      error))
	}

	#~ This small block is is for defining some variables to be used in subsequent blocks (groupsize, groupconditions etc.)

	subjn <- length(levels(subj))
	x1groups <- length(levels(x1))
	x1groupsize <- length(y)/x1groups 
	x2groups <- length(levels(x2))
	x2groupsize <- length(y)/x2groups
	grandmean <- mean(y)

	#~ ~~~~~~~~ DESIGN CHECK ~~~~~~~~
	#~ This block of code checks which analysis (e.g. one way repeated, mixed) is appropriate for the dataset submitted
	#~ Due to nature of repeated measures anova, only one test can be performed given combination of subjectid, x1, x2, y
	#~ Output is "design" vector which stores the types of variabels submitted (x1 and x2)
	#~ w = within, b = between, m = missing. E.g., "b","w" denotes mixed design (first variabel between, second within)
	#~ code probably not optimized
	design<-c("x","x")

	if(x2[1]=="absent"){
		for(k in 1:(x1groups-1)){
			if(sum(as.integer(subj[x1==levels(x1)[k]]))==sum(as.integer(subj[x1==levels(x1)[k+1]]))){
				design[1]<-"w"
			} else {
				design[1]<-"b"
			}
		}
		design[2]<-"m"
	} else {
		for(k in 1:(x2groups-1)){
			if(sum(as.integer(subj[x2==levels(x2)[k]]))==sum(as.integer(subj[x2==levels(x2)[k+1]]))){
				design[2]<-"w"
			} else {
				design[2]<-"b"
			}
		}
		for(k in 1:(x1groups-1)){
			if(sum(as.integer(subj[x1==levels(x1)[k]]))==sum(as.integer(subj[x1==levels(x1)[k+1]]))){
				design[1]<-"w"
			} else {
				design[1]<-"b"
			}
		}

	}

	#~ ~~~~~~~~ SS CALCULATIONS BLOCK ~~~~~~~~
	sst <- sum((y-mean(y))^2)	#ss total

	#~ SS for 1-way Repeated Measures anova
	ssb1 <- 0				#ss between factor 1
	ssw1 <- 0				#ss within (aka sst-ssb1)
 	 sss1 <- 0				#subsegment of ssw1, ss subjects
	 sse1 <- 0				#subsegment of ssw1, ss subjects error

	#~ SS for 1-way Repeated Measures anova (for x2, useful for mixed design calculations)
	ssb2 <- 0				#ss between factor 2
	ssw2 <- 0				#ss within (aka sst-ssb2)
	 sss2 <- 0				#subsegment of ssw2, ss subjects
	 sse2 <- 0				#subsegment of ssw2, ss subjects error

	#~ SS for 2-way Repeated Measures anova, mixed design (factor 1 within)
	ssb1x2 <- 0				#ss interaction
	sswm <- 0		  		#ss within applicable to mixed design (aka sst-ssb1-ssb2-ssb1x2)
	 sssm1 <- 0				#subsegment of sswm, ss subjects (factor 1)
	 ssem1 <- 0				#subsegment of sswm, ss subjects error (factor 1)

	#~ SS for 2-way Repeated Measures anova, mixed design (factor 2 within). 
	 sssm2 <- 0				#subsegment of sswm, ss subjects (factor 2)
	 ssem2 <- 0				#subsegment of sswm, ss subjects error (factor 2)

	#~ grand mean for subjects
	sssg <- 0				#ss subject grand

	#~ SS for 2-way Repeated Measures anove, within (2 within factors). 
	sse2w1 <- 0				#ss error (subject) for ssb1
	sse2w2 <- 0				#ss error (subject) for ssb2
	sse2wi <- 0				#ss error (subject) for ssb1x2

	#~ For loops to calculate the different SS. X1 SS calculated first
	for(k in 1:x1groups){
		currentgroup <- levels(x1)[k]
		currentvector <- y[x1==currentgroup]
		currentmean <- mean(currentvector)
		ssw1 <- ssw1 + sum((currentvector-currentmean)^2)
		ssb1 <- ssb1 + ((currentmean-grandmean)^2)*x1groupsize

		#~ Mixed model SS calculations
		for(i in 1:subjn){
			ivector <- y[x1==levels(x1)[k]&subj==i]
			imean <- mean(ivector)
				if(is.nan(imean)){next}
			sssm2 <- sssm2 + sum((imean-currentmean)^2)*x2groups
			sss2 <- sss2 + sum((imean-grandmean)^2)*x2groups
		}			
	}

	#~ For loops for x2
	for(j in 1:x2groups){
		currentgroup <- levels(x2)[j]
		currentvector <- y[x2==currentgroup]
		currentmean <- mean(currentvector)
		ssw2 <- ssw2 + sum((currentvector-currentmean)^2)
		ssb2 <- ssb2 + ((currentmean-grandmean)^2)*x2groupsize

		#~ Mixed model SS calculations
		for(i in 1:subjn){
			ivector <- y[x2==levels(x2)[j]&subj==i]
			imean <- mean(ivector)
				if(is.nan(imean)){next}
			sssm1 <- sssm1 + sum((imean-currentmean)^2)*x1groups
			sss1 <- sss1 + sum((imean-grandmean)^2)*x1groups
		}
		
		#~ Mixed model within calculations
		for(k in 1:x1groups){
			currentmw <- y[x2==levels(x2)[j]&x1==levels(x1)[k]]
			currentmwmean <- mean(currentmw)
			sswm <- sswm + sum((currentmw-currentmwmean)^2)
		}
	}
	
	#~ SS for subjects grand mean
	for(i in 1:subjn){
		sssg <- sssg + ((mean(y[subj==i])-grandmean)^2)* (x1groups*x2groups)
	}

	#~ Misc SS calculations
	ssb1x2 <- sst - ssb1 - ssb2 - sswm
	sse1 <- ssw1 - sss1
	sse2 <- ssw2 - sss2
	ssem1 <- sswm - sssm1
	ssem2 <- sswm - sssm2
	sse2w1 <- sss2 - ssb1 - sssg 
	sse2w2 <- sss1 - ssb2 - sssg 
	sse2wi <- sst - ssb2 - ssb1 - sssg - ssb1x2 - sse2w2 - sse2w1

	#~ ~~~~~~~~ DF CALCULATIONS BLOCK ~~~~~~~~
	dft <- length(y)-1			#N-1(zie ss blok voor beschrijvingen)

	dfb1 <- x1groups-1			#k-1
	dfw1 <- (dft+1)-x1groups		#N-k
 	 dfs1 <- x1groupsize-1			#nk-1
	 dfe1 <- dfs1*dfb1			#(nk-1)(k-1)

	dfb2 <- x2groups-1			#j-2
	dfw2 <- (dft+1)-x2groups		#N-j
	 dfs2 <- x2groupsize-1			#nj-1
	 dfe2 <- dfs2*dfb2			#(nj-1)(j-1)

	dfb1x2 <- dfb1*dfb2			#(j-1)(k-1)
	dfwm <- (dft+1)-(dfb1+1)*(dfb2+1)	#N-jk
	 dfem1 <- (dfb2+1)*(dfb1)* 
	  ((length(y)/x1groups/x2groups)-1)	#(j)(k-1)(nkj-1) code not optimized
	 dfsm1 <- dfwm-dfem1			#(N-jk)-((j)(k-1)(nkj-1))

	 dfem2 <- (dfb1+1)*(dfb2)*
	  ((length(y)/x1groups/x2groups)-1)	#(k)(j-1)(nkj-1) code not optimized
	 dfsm2 <- dfwm-dfem2			#(N-jk)-((k)(j-1)(nkj-1))

	dfsg <- subjn-1				#N-1

	dfe2w2 <- dfs1 - dfb2 - dfsg 		#2-way Within Repeated ssb1 error
	dfe2w1 <- dfs2 - dfb1 - dfsg 		#2-way Within Repeated ssb2 error
	dfe2wi <- dft - dfb2 - dfb1 - dfsg	- dfb1x2 - dfe2w2 - dfe2w1		
							#2-way Within Repeated interaction error

	#~ ~~~~~~~~ MEAN SQUARE CALCULATIONS BLOCK ~~~~~~~~

	mst<-sst/dft

	msb1<-ssb1/dfb1
	msw1<-ssw1/dfw1
	 mss1<-sss1/dfs1
	 mse1<-sse1/dfe1

	msb2<-ssb2/dfb2
	msw2<-ssw2/dfw2
	 mss2<-sss2/dfs2
	 mse2<-sse2/dfe2

	msb1x2<-ssb1x2/dfb1x2
	mswm<-sswm/dfwm
	 mssm1<-sssm1/dfsm1
	 msem1<-ssem1/dfem1

	 mssm2<-sssm2/dfsm2
	 msem2<-ssem2/dfem2

	mssg<-sssg/dfsg

	mse2w1<-sse2w1/dfe2w1
	mse2w2<-sse2w2/dfe2w2
	mse2wi<-sse2wi/dfe2wi

	#~ ~~~~~~~~ F CALCULATIONS BLOCK ~~~~~~~~

	onewbF <- r2(msb1/mse1)		#1-way between F
	
	twowmb1F1 <- r2(msb1/msem1)	#2-way mixed between 1 F (model 1) (within)
	twowmb2F1 <- r2(msb2/mssm1)	#2-way mixed between 2 F (model 1)
	twowmbiF1 <- r2(msb1x2/msem1)	#2-way mixed interaction F (model 1)

	twowmb1F2 <- r2(msb1/mssm2)	#2-way mixed between 1 F (model 2)
	twowmb2F2 <- r2(msb2/msem2)	#2-way mixed between 2 F (model 2) (within)
	twowmbiF2 <- r2(msb1x2/msem2)	#2-way mixed interaction F (model 2)

	twowrb1 <- r2(msb1/mse2w1) 	#2-way repeated between 1 F
	twowrb2 <- r2(msb2/mse2w2)	#2-way repeated between 2 F
	twowri <- r2(msb1x2/mse2wi)	#2-way repeated interaction F


	#~ ~~~~~~~~ ROUNDING BLOCK ~~~~~~~~

	sst<-r2(sst);mst<-r2(mst)

	ssb1<-r2(ssb1);msb1<-r2(msb1)
	ssw1<-r2(ssw1);msw1<-r2(msw1)
 	 sss1<-r2(sss1);mss1<-r2(mss1)
	 sse1<-r2(sse1);mse1<-r2(mse1)

	ssb2<-r2(ssb2);msb2<-r2(msb2)			
	ssw2<-r2(ssw2);msw2<-r2(msw2)
	 sss2<-r2(sss2);mss2<-r2(mss2)
	 sse2<-r2(sse2);mse2<-r2(mse2)	

	ssb1x2<-r2(ssb1x2);msb1x2<-r2(msb1x2)
	sswm<-r2(sswm);mswm<-r2(mswm)
	 sssm1<-r2(sssm1);mssm1<-r2(mssm1)		
	 ssem1<-r2(ssem1);msem1<-r2(msem1)

	 sssm2<-r2(sssm2);mssm2<-r2(mssm2)	
	 ssem2<-r2(ssem2);msem2<-r2(msem2)	

	sssg<-r2(sssg);mssg<-r2(mssg)		

	sse2w1<-r2(sse2w1);mse2w1<-r2(mse2w1)		
	sse2w2<-r2(sse2w2);mse2w2<-r2(mse2w2)		
	sse2wi<-r2(sse2wi);mse2wi<-r2(mse2wi)	

	#~ ~~~~~~~~ OUTPUT GENERATION BLOCK ~~~~~~~~

	#Output for 1-way Repeated Measures Anova (x1=Within, x2=Missing)
	if(design[1]=="w"&&design[2]=="m"){
		datafr<-data.frame(SS=c(ssb1,ssw1,sss1,sse1,sst),
			        MS=c(msb1,msw1,mss1,mse1,mst),
			        Df=c(dfb1,dfw1,dfs1,dfe1,dft),
				   F=c(onewbF,"","","",""),
			       Fcv=c(r2(qf(alpha,dfb1,dfe1,lower.tail=F)),"","","",""),
			         p=c(gp((pf(onewbF,dfb1,dfe1,lower.tail=F))),"","","",""))
		row.names(datafr)<-c("X1","Within","-Subject","-Subject Error","Total")
	}

	#Output for 2-way Repeated Measures Anova, mixed (x1=Within, x2=Between)
	if(design[1]=="w"&&design[2]=="b"){
		datafr<-data.frame(SS=c(ssb1,ssb2,ssb1x2,sswm,sssm1,ssem1,sst),
			        MS=c(msb1,msb2,msb1x2,mswm,mssm1,msem1,mst),
			        Df=c(dfb1,dfb2,dfb1x2,dfwm,dfsm1,dfem1,dft),
				   F=c(twowmb1F1,twowmb2F1,twowmbiF1,"","","",""),
			       Fcv=c(r2(qf(alpha,dfb1,dfsm1,lower.tail=F)),r2(qf(alpha,dfb2,dfem1,lower.tail=F)),r2(qf(alpha,dfb1x2,dfem1,lower.tail=F)),"","","",""),
			         p=c(gp((pf(twowmb1F1,dfb1,dfsm2,lower.tail=F))),gp((pf(twowmb2F1,dfb2,dfem2,lower.tail=F))),gp((pf(twowmbiF1,dfb1x2,dfem2,lower.tail=F))),"","","",""))
		row.names(datafr)<-c("X1 (within)","X2","Interaction","Within","-Subject","-Subject Error","Total")
	}

	#Output for 2-way Repeated Measures Anova, mixed (x1=Between, x2=Within)
	if(design[1]=="b"&&design[2]=="w"){
		datafr<-data.frame(SS=c(ssb1,ssb2,ssb1x2,sswm,sssm2,ssem2,sst),
			        MS=c(msb1,msb2,msb1x2,mswm,mssm2,msem2,mst),
			        Df=c(dfb1,dfb2,dfb1x2,dfwm,dfsm2,dfem2,dft),
				   F=c(twowmb1F2,twowmb2F2,twowmbiF2,"","","",""),
			       Fcv=c(r2(qf(alpha,dfb1,dfsm2,lower.tail=F)),r2(qf(alpha,dfb2,dfem2,lower.tail=F)),r2(qf(alpha,dfb1x2,dfem2,lower.tail=F)),"","","",""),
			         p=c(gp((pf(twowmb1F2,dfb1,dfsm2,lower.tail=F))),gp((pf(twowmb2F2,dfb2,dfem2,lower.tail=F))),gp((pf(twowmbiF2,dfb1x2,dfem2,lower.tail=F))),"","","",""))
		row.names(datafr)<-c("X1","X2 (within)","Interaction","Within","-Subject","-Subject Error","Total")
	}

	#Output for 2-way Repeated Measures Anova, 2 Within Facotrs (x1=Within, x2=Within)
	if(design[1]=="w"&&design[2]=="w"){
		datafr<-data.frame(SS=c(sssg,(sst-sssg),ssb1,ssb2,ssb1x2,sse2w1,sse2w2,sse2wi,sst),
			        MS=c(mssg,"",msb1,msb2,msb1x2,mse2w1,mse2w2,mse2wi,mst),
			        Df=c(dfsg,(dft-dfsg),dfb1,dfb2,dfb1x2,dfe2w1,dfe2w2,dfe2wi,dft),
				   F=c("","",twowrb1,twowrb2,twowri,"","","",""),
			       Fcv=c("","",r2(qf(alpha,dfb1,dfe2w1,lower.tail=F)),
					 r2(qf(alpha,dfb2,dfe2w2,lower.tail=F)),
					 r2(qf(alpha,dfb1x2,dfe2wi,lower.tail=F)),"","","",""),
			         p=c("","",gp((pf(twowrb1,dfb1,dfe2w1,lower.tail=F))),
					 gp((pf(twowrb2,dfb2,dfe2w2,lower.tail=F))),
					 gp((pf(twowri,dfb1x2,dfe2wi,lower.tail=F))),"","","",""))
		row.names(datafr)<-c("Between (error)","Within","-X1","-X2","-Interaction","-X1 Error","-X2 Error","-Interaction Error","Total")
	}
	
	#Generic output message
	finaloutput<-c("~~~ gAnovRep v.1 (gusAnovaRepeated version 0.1)~~~\n(Built by Gus Lathouwers, thank you for trying this module!)\ngAnovRep has successfully computed. Below are the results:\n",
		    "The submitted design is as follows: xvar1 is a [",design[1],"] variable. xvar2 is a [",design[2],"] variable.\n",
		    "(Legacy: [w] = within, [b] = between, [m] = missing)\n",
		    "Anova results:\n")

	#Print output message followed by dataframe
	cat(finaloutput)
	return(datafr)
}