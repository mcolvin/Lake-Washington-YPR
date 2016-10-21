figures<- function(n){

if(n==1)
	{
	hist(dat$Length)	
	
	spp<- aggregate(Length~Spp..Code,dat, length)
	plot(Weight..g.~Length..mm.,agedat,las=1,ylab="Weight (g)",xlab="Length (mm)")
	plot(Length..mm.~Age,agedat,las=1,ylab="Length (mm)",xlab="Age")
	}

}