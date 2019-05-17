figures<- function(n){

if(n==1)
	{## FIGURE 1
    par(mfrow=c(3,1))    
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
    curve(exp(coef(fit3)[1]+coef(fit3)[2]*log(x)),
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
    fit_fecundity<-lm(log(totalEggs)~log(length), egg)   
    plot(totalEggs/1000~length,
        data=egg,pch=19,las=1,
        ylab="Fecundity (x1000)",
        xlab="Total length (mm)") 
    curve(exp(coef(fit_fecundity)[1]+
        log(x)*coef(fit_fecundity)[2])/1000,add=TRUE)        
    }
if(n==2)
    {## FIGURE 2
    #######################################################################
    # YPR 
    #######################################################################

    par(mfrow=c(3,3),mar=c(1,1,0.5,.5),oma=c(3,3,2,1),las=1)
    ## f below = 0
    xx<-dcast(out,u_above~limit,
        value.var="Y",
        mean,
        subset=.(cm==0.1 &  maximum_cf_below== 0.0))
    matplot(xx[,1],xx[,-1],type='l',xaxt='n',lty=c(1,2,3),col="black")    
    axis(side=1,at=axTicks(1),labels=FALSE)
    mtext(side=3,"cm=0.1",line=0.5)

    xx<-dcast(out,u_above~limit,
        value.var="Y",
        mean,
        subset=.(cm==0.2 &  maximum_cf_below== 0.0))
    matplot(xx[,1],xx[,-1],type='l',xaxt='n',yaxt='n',lty=c(1,2,3),col="black")    
    axis(side=1,at=axTicks(1),labels=FALSE)  
    axis(side=2,at=axTicks(2),labels=FALSE)  
    mtext(side=3,"cm=0.2",line=0.5)

    xx<-dcast(out,u_above~limit,
        value.var="Y",
        mean,
        subset=.(cm==0.3 &  maximum_cf_below== 0.0))
    matplot(xx[,1],xx[,-1],type='l',xaxt='n',yaxt='n',lty=c(1,2,3),col="black")   
    axis(side=1,at=axTicks(1),labels=FALSE)  
    axis(side=2,at=axTicks(2),labels=FALSE)  
    mtext(side=3,"cm=0.3",line=0.5)
    legend("bottomright",c("254 mm", "279.4 mm","304.8 mm"),bty='n',lty=c(1:3),col="black")
    ## f below = 0.1
    xx<-dcast(out,u_above~limit,
        value.var="Y",
        mean,
        subset=.(cm==0.1 &  maximum_cf_below== 0.1))
    matplot(xx[,1],xx[,-1],type='l',xaxt='n',lty=c(1,2,3),col="black")   
    axis(side=1,at=axTicks(1),labels=FALSE)  

    xx<-dcast(out,u_above~limit,
        value.var="Y",
        mean,
        subset=.(cm==0.2 &  maximum_cf_below== 0.1))
    matplot(xx[,1],xx[,-1],type='l',yaxt='n',xaxt='n',lty=c(1,2,3),col="black")    
    axis(side=1,at=axTicks(1),labels=FALSE)  
    axis(side=2,at=axTicks(2),labels=FALSE)    

    xx<-dcast(out,u_above~limit,
        value.var="Y",
        mean,
        subset=.(cm==0.3 &  maximum_cf_below== 0.1))
    matplot(xx[,1],xx[,-1],type='l',yaxt='n',xaxt='n',lty=c(1,2,3),col="black")    
    axis(side=1,at=axTicks(1),labels=FALSE)  
    axis(side=2,at=axTicks(2),labels=FALSE)    


    ## f below = 0.2
    xx<-dcast(out,u_above~limit,
        value.var="Y",
        mean,
        subset=.(cm==0.1 &  maximum_cf_below== 0.2))
    matplot(xx[,1],xx[,-1],type='l',lty=c(1,2,3),col="black")    

    xx<-dcast(out,u_above~limit,
        value.var="Y",
        mean,
        subset=.(cm==0.2 &  maximum_cf_below== 0.2))
    matplot(xx[,1],xx[,-1],type='l',yaxt='n',lty=c(1,2,3),col="black")   
    axis(side=2,at=axTicks(2),labels=FALSE)     

    xx<-dcast(out,u_above~limit,
        value.var="Y",
        mean,
        subset=.(cm==0.3 &  maximum_cf_below== 0.2))
    matplot(xx[,1],xx[,-1],type='l',yaxt='n',lty=c(1,2,3),col="black")   
    axis(side=2,at=axTicks(2),labels=FALSE)   

    mtext(side=2,"Yield per 1000 recruits (kg)",line=1.5,outer=T,
        las=3,cex=1.2)
    mtext(side=1,"Exploitation above minimum length limit",line=2,outer=T,
        las=1,cex=1.2)
	
        

    }
    
if(n==3)
    {
    
    #######################################################################
    # SPR 
    #######################################################################
    #spr_0<-out[out$F_below==0 &
    #    out$F_above==0,]


    par(mfrow=c(3,3),mar=c(1,1,0.5,.5),oma=c(3,3,2,1),las=1)
    ## f below = 0
    xx<-dcast(out,u_above~limit,
        value.var="E",
        mean,
        subset=.(cm==0.1 &  maximum_cf_below== 0.0))
    xx[,-1]<- xx[,-1]/xx[rep(1,nrow(xx)),-1]
    matplot(xx[,1],xx[,-1],type='l',xaxt='n',lty=c(1,2,3),col="black")    
    axis(side=1,at=axTicks(1),labels=FALSE)
    mtext(side=3,"cm=0.1",line=0.5)

    xx<-dcast(out,u_above~limit,
        value.var="E",
        mean,
        subset=.(cm==0.2 &  maximum_cf_below== 0.0))
    xx[,-1]<- xx[,-1]/xx[rep(1,nrow(xx)),-1]
    matplot(xx[,1],xx[,-1],type='l',xaxt='n',yaxt='n',lty=c(1,2,3),col="black")    
    axis(side=1,at=axTicks(1),labels=FALSE)  
    axis(side=2,at=axTicks(2),labels=FALSE)  
    mtext(side=3,"cm=0.2",line=0.5)

    xx<-dcast(out,u_above~limit,
        value.var="E",
        mean,
        subset=.(cm==0.3 &  maximum_cf_below== 0.0))
    xx[,-1]<- xx[,-1]/xx[rep(1,nrow(xx)),-1]
    matplot(xx[,1],xx[,-1],type='l',xaxt='n',yaxt='n',lty=c(1,2,3),col="black")   
    axis(side=1,at=axTicks(1),labels=FALSE)  
    axis(side=2,at=axTicks(2),labels=FALSE)  
    mtext(side=3,"cm=0.3",line=0.5)
    legend("topright",c("254 mm", "279.4 mm","304.8 mm"),bty='n',lty=c(1:3),col="black")
    ## f below = 0.1
    xx<-dcast(out,u_above~limit,
        value.var="E",
        mean,
        subset=.(cm==0.1 &  maximum_cf_below== 0.1))
    xx[,-1]<- xx[,-1]/xx[rep(1,nrow(xx)),-1]
    xx[,-1]<- xx[,-1]/xx[rep(1,nrow(xx)),-1]
    matplot(xx[,1],xx[,-1],type='l',xaxt='n',lty=c(1,2,3),col="black")   
    axis(side=1,at=axTicks(1),labels=FALSE)  

    xx<-dcast(out,u_above~limit,
        value.var="E",
        mean,
        subset=.(cm==0.2 &  maximum_cf_below== 0.1))
    xx[,-1]<- xx[,-1]/xx[rep(1,nrow(xx)),-1]
    matplot(xx[,1],xx[,-1],type='l',yaxt='n',xaxt='n',lty=c(1,2,3),col="black")    
    axis(side=1,at=axTicks(1),labels=FALSE)  
    axis(side=2,at=axTicks(2),labels=FALSE)    

    xx<-dcast(out,u_above~limit,
        value.var="E",
        mean,
        subset=.(cm==0.3 &  maximum_cf_below== 0.1))
    xx[,-1]<- xx[,-1]/xx[rep(1,nrow(xx)),-1]
    matplot(xx[,1],xx[,-1],type='l',yaxt='n',xaxt='n',lty=c(1,2,3),col="black")    
    axis(side=1,at=axTicks(1),labels=FALSE)  
    axis(side=2,at=axTicks(2),labels=FALSE)    


    ## f below = 0.2
    xx<-dcast(out,u_above~limit,
        value.var="E",
        mean,
        subset=.(cm==0.1 &  maximum_cf_below== 0.2))
    xx[,-1]<- xx[,-1]/xx[rep(1,nrow(xx)),-1]
    matplot(xx[,1],xx[,-1],type='l',lty=c(1,2,3),col="black")    

    xx<-dcast(out,u_above~limit,
        value.var="E",
        mean,
        subset=.(cm==0.2 &  maximum_cf_below== 0.2))
    xx[,-1]<- xx[,-1]/xx[rep(1,nrow(xx)),-1]
    matplot(xx[,1],xx[,-1],type='l',yaxt='n',lty=c(1,2,3),col="black")   
    axis(side=2,at=axTicks(2),labels=FALSE)     

    xx<-dcast(out,u_above~limit,
        value.var="E",
        mean,
        subset=.(cm==0.3 &  maximum_cf_below== 0.2))
    xx[,-1]<- xx[,-1]/xx[rep(1,nrow(xx)),-1]   
    matplot(xx[,1],xx[,-1],type='l',yaxt='n',lty=c(1,2,3),col="black")   
    axis(side=2,at=axTicks(2),labels=FALSE)   

    mtext(side=2,"Spawning potential ratio (SPR)",line=1.5,outer=T,
        las=3,cex=1.2)
    mtext(side=1,"Exploitation above minimum length limit",line=2,outer=T,
        las=1,cex=1.2)
	

    }
}