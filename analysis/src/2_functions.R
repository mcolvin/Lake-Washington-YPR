
biomass_yield<- function(t,state,parameters)
	{
	N<-state['N']
	Y<-state['Y']	
	x<- t # age
	
	# SET FISHING MORTALITY GIVEN AGE
	Ft<-0
	if(x<parameters$minimum_harvested)
		{Ft<- 0}
	if(x>= parameters$minimum_harvested &  x<parameters$tr)## BELOW MINIMUM LENGTH LIMIT
		{Ft<- parameters$F_below}
	if(x>=parameters$tr) ## ABOVE  MINIMUM LENGTH LIMIT
		{Ft<- parameters$F_above}
	
	# CALCULATE LENGTH AND WEIGHT AT AGE
	## LENGTH AT AGE
	Lt<-parameters$linf * (1 - exp(-parameters$k * (x-parameters$t0)))
	## WEIGHT AT AGE
	Wt = parameters$a*Lt^parameters$b
	
	## NATURAL MORTALITY
	N_died<- parameters$M*N

	## FISHING MORTALITY
	N_harvested<- Ft*N 
	
	## CHANGE IN ABUNDANCE
	dN<- -N_died - N_harvested
	
	## CHANGE IN YIELD
	dY<- N_harvested*Wt	
	
	ret<-list(c(dN,dY),
		Biomass=as.vector(N_harvested*Wt),
		Lt=as.vector(Lt),
		Wt=as.vector(Wt))
	return(ret)
	}

yieldB_jones<- function(b,k,tmax,t0,F,M,Tr,Recruits,Winf)
	{
	r = Tr-t0# tr-t0
	X = exp(-k*r)
	X1= exp(-k* (tmax-t0))
	P = ((F+M)/k)
	Q = b + 1 # assumes allometric scaling
	out<- ((F*Recruits*exp(F*r)*Winf)/k)* ibeta(X,P,Q)-ibeta(X1,P,Q)
	return(out)
	}


te_fun<- function(limit,linf,k,t0)
	{
	# RETURNS THE AGE OF A FISH GIVEN A SIZE (i.e., inverse of vbgf)
	te<-log(1-((limit/linf)))/-k + t0
	return(te)
	}
