figures<- function(n){

if(n==1)
	{
	hist(dat$Length)	
	
	spp<- aggregate(Length~Spp..Code,dat, length)
	plot(Weight..g.~Length..mm.,agedat,las=1,ylab="Weight (g)",xlab="Length (mm)")
	plot(Length..mm.~Age,agedat,las=1,ylab="Length (mm)",xlab="Age")
	}

}

presPlot()
plot(Length..mm.~Age,agedat,
    xlab="Age",ylab="Length(mm)",las=1)
text(4, 120, 
    expression(L(Age) ==344  %.%(1- plain(e)^{-0.4 %.%(Age-1.0)})),
    cex=1.3)

 plot(Weight..g.~Length..mm.,agedat,
    xlab="Length(mm)",ylab="Weight (g)",las=1)
text(150, 320, 
    expression(Weight ==10^{-6.2} %.%Length^{3.61}))

    lm(log10(Weight..g.)~log10(Length..mm.),agedat,subset=
        Weight..g.>0)
    
    log10(agedat$Weight..g.)