#######################################################################
# NOTES
#######################################################################

# WEIGHTS WERE ONLY TAKEN ON AGED FISH

## TO DO
### FIT CHI TO AGE LENGTH DATA
### INCORPORATE RATIO OF WHITE TO BLACK CRAPPIE IN YPR
### ADD SPR TO ANALSYSIS


#######################################################################
# RATIO OF BLACK TO WHITE CRAPPIE
#######################################################################

## 1410 is white; 1409 is black
crappie_composition<- prop.table(table(dat[dat$Spp..Code%in% c(1409,1410),]$Spp..Code))


#######################################################################
# LENGTH-WEIGHT
#######################################################################
## WEIGHT ONLY TAKEN FOR FISH THAT WERE AGED
fit1<-lm(lnW~lnL,
    data=agedat,subset=Weight..g.>0)
fit2<-lm(lnW~lnL+Species,
    data=agedat,subset=Weight..g.>0) 
fit3<-lm(lnW~lnL*Species,
    data=agedat,subset=Weight..g.>0) 

AIC(fit1);AIC(fit2);AIC(fit3) 
# STRONG EVIDENCE FOR DIFFERENCE AMONG BLACK AND WHITE CRAPPIE  

## BLACK,WHITE
a<-c(exp(coef(fit3)[1]),exp(coef(fit3)[1]+coef(fit3)[3]))
b<-c(coef(fit3)[2],coef(fit3)[2]+coef(fit3)[4])
 

#######################################################################
# AGE-LENGTH
#######################################################################
## CHI METHOD

## BIN AGED LENGTHS INTO INCHES
bins<-seq(0,400,by=25.4)
binlab<- paste(bins[-length(bins)],bins[-1],sep="-")
agedat$l_bin<-cut(agedat$Length..mm.,bins,binlab)
dat$l_bin<- cut(dat$Length,bins,binlab)


top<-table(dat[dat$Spp..Code==1409,]$l_bin)/nrow(dat[dat$Spp..Code==1409,])
bot<-table(agedat[agedat$Species=="wc",]$l_bin)/nrow(agedat[agedat$Species=="wc",])
rw<- data.frame(l_bin=levels(dat$l_bin),rw= as.numeric(top/bot))
rw$rw<- ifelse(is.na(rw$rw),1, rw$rw)
agedat_wc<-subset(agedat,Species=="wc")
agedat_wc<- merge(agedat_wc, rw, by=c("l_bin"),all.x=TRUE)
    
top<-table(dat[dat$Spp..Code%in%c(1409,1410),]$l_bin)/nrow(dat[dat$Spp..Code%in%c(1409,1410),])
bot<-table(agedat$l_bin)/nrow(agedat)
rw<- data.frame(l_bin=levels(dat$l_bin),rw= as.numeric(top/bot))
rw$rw<- ifelse(is.na(rw$rw),1, rw$rw)
agedat<- merge(agedat, rw, by=c("l_bin"),all.x=TRUE)    
    

    
vbgf_fit  <- try(
    nls(Length..mm.~ Linf * (1- exp(-k*(Age-t0))),
    agedat,start=list(
        Linf=305, 
        k=0.2, 
        t0=0.0),
    weights= agedat$rw,
    control=list(maxiter=3000)),
    silent=T)	

#######################################################################
# LENGTH-FECUNDITY
#######################################################################
    
fit_fecundity<-lm(log(totalEggs)~log(length),
    egg)   
    
    
#######################################################################
# YPR AND SPR
#######################################################################    
input<-list()    
input$spp_ratio<- c(0.75, 0.25) # PROPORTION WHITE AND BLACK CRAPPIE
input$mll<- "10, 11, 12"
input$conditional_mortality<- "0.1, 0.2, 0.3"
input$maxAge<- max(na.omit(agedat$Age))
input$a<- a # from fit 3; black then white
input$b<- b# from fit 3; black then white

input$a_fecundity<- coef(fit_fecundity)[1]
input$b_fecundity<- coef(fit_fecundity)[2]

input$linf<- coef(vbgf_fit)["Linf"]
input$k<-  coef(vbgf_fit)["k"]
input$t0<- coef(vbgf_fit)["t0"]
input$R<- 1000

## BELOW MINIMUM LENGTH LIMITS
### MINIMUM SIZE OF FISH HARVESTED BELOW MLL
input$minimum_harvested<- 9  
### MAXIMUM CONDITIONAL FISHING MORTALITY TO EVALUATE BELOW MLL
input$maximum_cf_below<- 0.2 

# MAKE COMBINATIONS OF MORTALITY AND 
out<-combos(input)

input$minimum_harvested<- out$min_age_harvested[1]
input$tr<- out$tr[1]
input$M<- out$M_above[1]
input$F_above<- out$F_above[1]
input$F_below<- out$F_below[1]



## SOLVE THE SYSTEM FOR ALL 
out <- ode(
    y = c(WC=input$R*input$spp_ratio[1],BC=input$R*input$spp_ratio[2], 
        Y=0), 
    times = seq(1,input$maxAge+1,by=0.1), 
    func = biomass_yield, 
    parms = input,
    method="lsoda")
out<-as.data.frame(out)
# GET YIELD
return(out$Y[nrow(out)]/1000)
}





#sim$Y<- apply(sim,1,yield_sim)

	
sim<- sim[order(sim$M_above, sim$maximum_cf_below),]
xyplot(Y~cf, sim, subset=limit==254, group=M_above+maximum_cf_below,
	type='l')

par(mfrow=c(2,1),mar=c(2,4,0,0),oma=c(1,1,1,1))
plot(N~time,sim,type='l',las=1,ylab="Abundance")
plot(Y/1000~time,sim,type='l',las=1,
	ylab="Yield (kg)")

dev.off()	
plot(Biomass/1000~time,sim,type='l',
	ylab="Biomass yield (kg)")	
plot(Lt~time,sim,type='l')
plot(Wt~time,sim,type='l')
	
# USE AREA UNDER THE CURVE METHOD
# TO FIND YIELD
auc(sim$time, sim$Biomass)/1000
# YIELD BY ODE
sim$Y[nrow(sim)]/1000
	
	
	
	
	
	
	
	
yield<- integrate(biomass_yield, 
	lower = 0, 
	upper = maxAge+1,
	input=sim)$value
	
	
	
	
	
	