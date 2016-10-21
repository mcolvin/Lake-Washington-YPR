
library(shiny)
library(shinythemes)


te_fun<- function(limit,linf,k,t0)
	{
	# RETURNS THE AGE OF A FISH GIVEN A SIZE (i.e., inverse of vbgf)
	te<-log(1-((limit/linf)))/-k + t0
	return(te)
	}
