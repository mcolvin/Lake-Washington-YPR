#######################################################################
#
# NOTES
#
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
a<-c((coef(fit3)[1]),(coef(fit3)[1]+coef(fit3)[3]))
b<-c(coef(fit3)[2],coef(fit3)[2]+coef(fit3)[4])
 

#######################################################################
# AGE-LENGTH
#######################################################################
## CHI METHOD

agedat<-subset(agedat,weight>0)



## CATCH DATA
catch<-  dat[,c(17,18)]
names(catch)<- c("spp","len")
names(agedat)[c(1,3)]<- c("spp","len")
catch<-rbind(catch,agedat[,c(1,3)])
catch[catch$spp==1410,]$spp<-'wc'
catch[catch$spp==1409,]$spp<-'bc'


## AGING DATA

## BIN AGED LENGTHS INTO CM GROUPS
catch$l_bin<- floor(catch$len/10)
agedat$l_bin<-floor(agedat$len/10)

## NEED TO ADD LENGTHS FOR AGED FISH TO CATCH...

## WEIGHTS FOR WHITE CRAPPIE
top<-table(catch[catch$spp=="wc"& catch$l_bin>=10,]$l_bin)/
    nrow(catch[catch$spp=="wc",])
bot<-table(agedat[agedat$spp=="wc",]$l_bin)/
    nrow(agedat[agedat$spp=="wc",])
pp<-
rw<- data.frame(
    Species="wc",
    l_bin=levels(dat$l_bin),
    rw= as.numeric(top/bot))
top<-table(dat[dat$Spp..Code==1410,]$l_bin)/nrow(dat[dat$Spp..Code==1410,])
bot<-table(agedat[agedat$Species=="bc",]$l_bin)/nrow(agedat[agedat$Species=="bc",])
rw<- data.frame(
    Species="bc",
    l_bin=levels(dat$l_bin),
    rw= as.numeric(top/bot))   


   
rw$rw<- ifelse(is.na(rw$rw),1, rw$rw)

agedat<- merge(agedat, rw, by=c("Species","l_bin"),all.x=TRUE)
   


    
vbgf_fit  <- nls(Length..mm.~ Linf * (1- exp(-k*(Age-t0))),
    agedat,start=list(
        Linf=305, 
        k=0.2, 
        t0=0.0),
    weights= agedat$rw,
    control=list(maxiter=3000))	

#######################################################################
#
#  FIT VBGF FOR EACH SPECIES
#
#######################################################################
mm <- model.matrix(~ 0 + Species, agedat)
m0  <- nls(Length..mm.~ Linf* (1- exp(-k*(Age-t0))),
    agedat,start=list(
        Linf=305, 
        k=0.2, 
        t0=0.0),
    weights= agedat$rw,
    control=list(maxiter=3000))	   
m1  <- nls(Length..mm.~ drop(mm %*% c(LinfBC,LinfWC)) * (1- exp(-k*(Age-t0))),
    agedat,start=list(
        LinfBC=305, 
        LinfWC=305, 
        k=0.2, 
        t0=0.0),
    weights= agedat$rw,
    control=list(maxiter=3000))	   

m2  <- nls(Length..mm.~ Linf * (1- exp(-drop(mm %*% c(kBC,kWC))*(Age-t0))),
    agedat,start=list(
        Linf=305, 
        kBC=0.2, 
        kWC=0.2, 
        t0=0.0),
    weights= agedat$rw,
    control=list(maxiter=3000))	     

m3  <- nls(Length..mm.~ drop(mm %*% c(LinfBC,LinfWC)) * (1- exp(-drop(mm %*% c(kBC,kWC))*(Age-t0))),
    agedat,start=list(
        LinfBC=305, 
        LinfWC=305, 
        kBC=0.2, 
        kWC=0.2, 
        t0=0.0),
    weights= agedat$rw,
    control=list(maxiter=3000))	     
   
AIC(m0);AIC(m1);AIC(m2);AIC(m3)   

#######################################################################
#
#  MAXIMUM AGE
#
#######################################################################

lambda<- aggregate(Age~Species,agedat,max)

    
#######################################################################
# LENGTH-FECUNDITY
#######################################################################
    
fit_fecundity<-lm(log(totalEggs)~log(length),
    egg)   
    
    
#######################################################################
# YPR AND SPR
#######################################################################    
input<-list()    
input$spp_ratio<- c(WC=0.75, BC=0.25) # PROPORTION WHITE AND BLACK CRAPPIE
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
out<-combos(input=input)
out$Y<-NA
out$E<-NA
for(i in 1:nrow(out))
    {
    input$minimum_harvested<- out$min_age_harvested[i]
    input$tr<- out$tr[i]
    input$M<- out$M_above[i]
    input$F_above<- out$F_above[i]
    input$F_below<- out$F_below[i]


    ## SOLVE THE SYSTEM FOR ALL 
    outt <- ode(
        y = c(WC=as.numeric(input$R*input$spp_ratio["WC"]),
            BC=as.numeric(input$R*input$spp_ratio["BC"]), 
            Y=0,EWC=0,EBC=0), 
        times = seq(1,input$maxAge+1,by=0.1), 
        func = biomass_yield, 
        parms = input,
        method="lsoda")
    outt<-as.data.frame(outt)
    # GET YIELD
    out$Y[i]<-outt$Y[nrow(outt)]/1000
    out$E[i]<- outt$EWC[nrow(outt)]+outt$EBC[nrow(outt)]
    }
   
save(out,file="output/ypr.Rdata")
