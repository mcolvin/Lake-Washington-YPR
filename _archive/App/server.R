
#input<- list()

## INPUTS FROM UI.R
#input$mll<- "10, 11, 12"
#input$conditional_mortality<- "0.1, 0.2, 0.3"
#input$maxAge<- 5
#input$a<- 0.00001
#input$b<-3
#input$linf<- 600
#input$k<- 0.2
#input$t0<-0
#input$R<- 1000

## BELOW MINIMUM LENGTH LIMITS
#input$minimum_harvested<- 9  # MINIMUM SIZE OF FISH HARVESTED BELOW MLL
#input$maximum_cf_below<- 0.2 # MAXIMUM CONDITIONAL FISHING MORTALITY TO EVALUATE BELOW MLL

shinyServer(function(input, output) {

## SIM_DAT
sim_dat<- reactive({
	# MAKE COMBINATIONS OF MORTALITY AND 
	# MLL TO EVALUATE

	## CONVERT MLL INPUT (INCHES) TO MM PRIOR TO ANALYSIS
	mll_mm<- as.numeric(unlist(strsplit(input$mll,",")))*25.4
	min_size_harvested<- input$minimum_harvested*25.4

	## PARSE OUT CONDITIONAL FISHING MORTALITIES TO EVALUATE
	sim_cm<-as.numeric(unlist(strsplit(input$conditional_mortality,",")))	
	
	## EXPAND OUT COMBINATIONS TO EVALUATE YIELD
  if(input$maximum_cf_below==0){
      sim<- expand.grid(cm = sim_cm,
		cf = seq(0,0.9,by=0.1),
		limit=mll_mm,
		min_size_harvested=min_size_harvested,
		maximum_cf_below = 0)
  }
  if(input$maximum_cf_below>0){
  sim<- expand.grid(cm = sim_cm,
  	cf = seq(0,0.9,by=0.05),
		limit=mll_mm,
		min_size_harvested=min_size_harvested,
		maximum_cf_below = seq(0,input$maximum_cf_below,length=4)
		)
}


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
	
	tmp<- list(sim=sim)
	return(tmp)
	})
## END CHUNK 1: SIM_DAT

## FUNCTION TO CALCULATE YPR
## TAKES A BIT OF TIME...
## NEED TO ADD A PROGRESS BAR
out<- reactive({
	sim<- sim_dat()$sim	
	dt<- 0.1
	times<-seq(0,input$maxAge+1,dt)
	Y<-N<-matrix(0,nrow(sim),length(times))
	N[,1]<- input$R   
	for(i in 2:ncol(N))
		{
		indx1<-ifelse(times[i]>=sim$min_age_harvested &times[i]<sim$tr,1,0)
		indx2<-ifelse(times[i]>=sim$tr,1,0)
		harvested<-N[,i-1]*(sim$F_below*indx1+sim$F_above*indx2)
		mortality<- N[,i-1]*sim$M_above
		dN<- (mortality+harvested)*dt			
		Lt<-input$linf * (1 - exp(-input$k* (times[i]-input$t0)))
		## WEIGHT AT AGE
		Wt = input$a*Lt^input$b
		dY<- (Wt*harvested)*dt			
		N[,i]<-N[,i-1]-dN
		Y[,i]<- Y[,i-1]+dY
		}	
	sim$Y<- Y[,ncol(Y)]
	sim<- sim[order(sim$M_above, sim$maximum_cf_below),]

	return(list(sim=sim))
	}) 
	## [2] END
	
## [3] RENDER TABLE OF OUTPUTS
output$sims <- renderTable({(out()$sim)},include.rownames=FALSE) 
## [3] END 

  
  
## [4] YPR GROWTH OVERFISHING FIGURES
output$f01<- renderPlot({
tmp<- out()$sim
#par(mfrow=c(2,1),mar=c(2,4,0,0),oma=c(1,1,1,1))
#plot(N~time,out,type='l',las=1,ylab="Abundance")
plot(Y~F_above,tmp,type='l',las=1,	ylab="Yield (kg)")
})
## [4] END
  
  
  
  
})