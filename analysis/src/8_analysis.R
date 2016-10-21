## CHI METHOD
	top<-table(srs$l_bin)/nrow(srs)
	bot<-table(alk$l_bin)/nrow(alk)
	rw<- data.frame(l_bin=levels(srs$l_bin),rw= as.numeric(top/bot))
	rw$rw<- ifelse(is.na(rw$rw),1, rw$rw)
	alk<- merge(alk, rw, by="l_bin",all.x=TRUE)
	fit<- try(
		nls(len_obs~ Linf * (1- exp(-k*(age-t0))),
		alk,start=list(
			Linf=Linf*10, 
			k=k, 
			t0=t0),
		weights= alk$rw,
		control=list(maxiter=3000)),
		silent=T)	










# MAKE COMBINATIONS OF MORTALITY AND 
# MLL TO EVALUATE

## CONVERT MLL INPUT (INCHES) TO MM PRIOR TO ANALYSIS
mll_mm<- as.numeric(unlist(strsplit(input$mll,","))	)*25.4
min_size_harvested<- input$minimum_harvested*25.4

## PARSE OUT CONDITIONAL FISHING MORTALITIES TO EVALUATE
sim_cm<-as.numeric(unlist(strsplit(input$conditional_mortality,",")))	
	
## EXPAND OUT COMBINATIONS TO EVALUATE YIELD
sim<- expand.grid(cm = sim_cm,
	cf = seq(0,0.9,0.05),
	limit=mll_mm,
	min_size_harvested=min_size_harvested,
	maximum_cf_below = seq(0,input$maximum_cf_below,length=4)
	)

## INSTANTANEOUS MORTALITY (NATURAL)
sim$M_below = log(-1/(sim$cm-1)) # BELOW MLL
sim$M_above = log(-1/(sim$cm-1)) # ABOVE MLL

## INSTANTANEOUS ANNUAL MORTALITY (FISHING)
sim$F_below = log(-1/(sim$maximum_cf_below-1)) # BELOW MLL
sim$F_above = log(-1/(sim$cf-1)) # ABOVE MLL

## INSTANTANEOUS ANNUAL MORTALITY (TOTAL)
sim$Z_below = sim$M_below+sim$F_below
sim$Z_above = sim$M_above+sim$F_above

## FINITE ANNUAL MORTALITY
sim$AM_below<- 1-exp(-sim$Z_below)
sim$AM_above<- 1-exp(-sim$Z_above)

## ANNUAL SURVIVAL
sim$S_below<- 1-sim$AM_below
sim$S_above<- 1-sim$AM_above


## EXPLOITATION RATE
sim$u_below<- (sim$F_below*(1-sim$S_below))/sim$Z_below
sim$u_above<- (sim$F_above*(1-sim$S_above))/sim$Z_above

## ??
sim$v_below<- (sim$M_below*(1-sim$S_below))/sim$Z_below
sim$v_above<- (sim$M_above*(1-sim$S_above))/sim$Z_above

## CONVERT MINIMUM HARVESTED FOR YIELD CALCULATE
sim$min_age_harvested<- te_fun(sim$min_size_harvested, input$linf,input$k,input$t0)

## CONVERT MLL TO AGE FOR YIELD CALCULATION
sim$tr<-te_fun(sim$limit, input$linf,input$k,input$t0) 
times<-seq(0,input$maxAge+1,0.1)

yield_sim<- function(x)
	{
	parms<- list(a=x['a'],
		b=x['b'],		
		linf=x['linf'], 
		k=x['k'], 
		t0=x['t0'],		
		M=x['M_above'],		
		F_below=x['xF_below'],
		F_above=x['F_above'],
		minimum_harvested= x['min_age_harvested'],
		tr=x['tr'])
	## SOLVE THE SYSTEM FOR ALL 
	out <- ode(
		y = c(N=input$R, Y=0), 
		times = times, 
		func = biomass_yield, 
		parms = parms,
		method="lsoda")
	out<-as.data.frame(out)
	# GET YIELD
	return(out$Y[nrow(out)]/1000)
	}
	
#sim$Y<- apply(sim,1,yield_sim)



sim$Y<-sapply(1:nrow(sim),function(x)
	{
	i=x
	parms<- list(a=input$a,
		b=input$b,		
		linf=input$linf, 
		k=input$k, 
		t0=input$t0,		
		M=sim$M_above[i],		
		F_below=sim$F_below[i],
		F_above=sim$F_above[i],
		minimum_harvested= sim$min_age_harvested[i],
		tr=sim$tr[i])

	## SOLVE THE SYSTEM FOR ALL 
	out <- ode(
		y = c(N=input$R, Y=0), 
		times = times, 
		func = biomass_yield, 
		parms = parms,
		method="lsoda")
	out<-as.data.frame(out)
	# GET YIELD
	return(out$Y[nrow(out)]/1000)
	})

	
	
	
	
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
	
	
	
	
	
	