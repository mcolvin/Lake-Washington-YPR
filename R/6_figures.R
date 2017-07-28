figures<- function(n){

if(n==1)
	{
    par(mfrow=c(1,3))    
    ###################################################################    
    # AGE LENGTH
    ###################################################################
	plot(Length..mm.~Age,agedat,
        xlab="Age",ylab="Length (mm)",
        las=1,type='n')
 	points(Length..mm.~I(Age-0.1),agedat,
        subset=  Species =="bc",pch=19)      
  	points(Length..mm.~I(Age+0.1),agedat,
        subset=  Species =="wc",pch=1)      
    legend('bottomright',
        c("Black Crappie","White Crappie"),
        pch=c(19,1),bty='n')


    ###################################################################    
    # LENGTH WEIGHT
    ###################################################################
    plot(Weight..g.~Length..mm.,
        data=agedat,
        xlab="Length(mm)",
        ylab="Weight (g)",
        las=1,type='n')
    points(Weight..g.~Length..mm.,
        data=agedat,
        subset=Species=='bc',pch=19)
    points(Weight..g.~Length..mm.,
        data=agedat,
        subset=Species=='wc',pch=1)
    legend('topleft',
        c("Black Crappie","White Crappie"),
        pch=c(19,1),lty=c(1,2),bty='n')
    fit3<-lm(lnW~lnL*Species,
        data=agedat,subset=Weight..g.>0) 
    # CURVE FOR BLACK CRAPPIE LW
    curve(exp(coef(fit2)[1]+coef(fit2)[2]*log(x)),
        add=TRUE)       
     # CURVE FOR WHITE CRAPPIE LW
    curve(exp(coef(fit3)[1]+
        coef(fit3)[2]*log(x)+
        coef(fit3)[3]+
        coef(fit3)[4]*log(x)),
        add=TRUE,lty=2) 

        
    ###################################################################    
    # FECUNDITY
    ###################################################################
    plot(totalEggs/1000~length,
        data=egg,pch=19,las=1,
        ylab="Fecundity (x1000)",
        xlab="Total length (mm)") 
    curve(exp(coef(fit_fecundity)[1]+
        log(x)*coef(fit_fecundity)[2])/1000,add=TRUE)        
    }
if(n==2)
    {
        

    }
    
if(n==3)
    {# PLOT OF FECUNDITY

    }
}