
biomass_yield<- function(t,state,parameters)
	{
	WC<-state['WC'] # WHITE CRAPPIE
	BC<-state['BC'] # BLACK CRAPPIE
	Y<-state['Y']	# YIELD FOR BOTH
	EWC<-state['EWC']	# YIELD FOR BOTH
	EBC<-state['EBC']	# YIELD FOR BOTH
	x<- t # age
	
	# SET FISHING MORTALITY GIVEN AGE
    ## WHITE CRAPPIE
	Ft<-0
	if(x<parameters$minimum_harvested)
		{Ft<- 0}
	if(x>= parameters$minimum_harvested &  x<parameters$tr)## BELOW MINIMUM LENGTH LIMIT
		{Ft<- parameters$F_below}
	if(x>=parameters$tr) ## ABOVE  MINIMUM LENGTH LIMIT
		{Ft<- parameters$F_above}
	
    

    
	# CALCULATE LENGTH AND WEIGHT AT AGE
	## LENGTH AT AGE
    ## POOLED VBGF FOR BC AND WC
	Lt<-as.numeric(parameters$linf * (1 - exp(-parameters$k * (x-parameters$t0))))
	
    ## WEIGHT AT AGE
	### WHITE CRAPPIE
    Wt_wc = as.numeric(parameters$a[1]*Lt^parameters$b[1])
    ### BLACK CRAPPIE
    Wt_bc = as.numeric(parameters$a[2]*Lt^parameters$b[2]    )
    
    
    
	## NATURAL MORTALITY
	N_died_wc<- parameters$M*WC
	N_died_bc<- parameters$M*BC

	## FISHING MORTALITY
	N_harvested_wc<- Ft*WC
	N_harvested_bc<- Ft*BC
	
	## CHANGE IN ABUNDANCE
	dWC<- -N_died_wc - N_harvested_wc
	dBC<- -N_died_bc - N_harvested_bc
	
    ## EGGS
    eggs_bc<-(exp(parameters$a_fec)*Lt^parameters$b_fec)*WC*0.5
    eggs_wc<-(exp(parameters$a_fec)*Lt^parameters$b_fec)*BC*0.5
    dEWC<- ifelse(x>2.5,as.numeric(eggs_wc),0)
    dEBC<- ifelse(x>2.5,as.numeric(eggs_bc),0)
    
	## CHANGE IN YIELD
	dY<- (N_harvested_wc*Wt_wc+N_harvested_bc*Wt_bc)	
	
	ret<-list(c(dWC,dBC,dY,dEWC,dEBC),
		Biomass=as.vector(N_harvested_wc*Wt_wc+N_harvested_bc*Wt_bc),
		Lt=as.vector(Lt),
		Wt_wc=as.vector(Wt_wc),
		Wt_bc=as.vector(Wt_bc),
        EWC=eggs_wc,
        EBC=eggs_bc)
	return(ret)
	}


te_fun<- function(limit,linf,k,t0)
	{# FUNCTION TO CONVERT LENGTH TO AGE
	# RETURNS THE AGE OF A FISH GIVEN A SIZE (i.e., inverse of vbgf)
	te<-log(1-((limit/linf)))/-k + t0
	return(te)
	}

    
    
combos<-function(input,...)
    {
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
        maximum_cf_below = seq(0,input$maximum_cf_below,length=3)
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

    return(sim)
    
    
    }