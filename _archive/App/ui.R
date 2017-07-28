shinyUI(navbarPage("Yield Per Recruit Model",

tabPanel("Model inputs",
	sidebarLayout(
	
		sidebarPanel(
			h3("How to run the model"),
			p("1. Input $A$ and $B$ for the length-weight relationship")
		),
		
		mainPanel(
			tabsetPanel(
				# TAB PANEL 1
				tabPanel("Inputs",
				fluidRow(
					column(3,
						h3("Harvest inputs"),
						textInput("mll", "Minimum Length Limits (in.)", value="10, 11, 12"),
						textInput("conditional_mortality", "Conditional mortality levels", 
							value="0.1, 0.2, 0.3"),
						numericInput('minimum_harvested', 'Minimum size of fish harvest',
							value=9,min = 0, max = 10000),
						numericInput('maximum_cf_below', 'Max. Cond. Mortality below MLL',
							value=0,min = 0, max = 0.99)
						),
					column(4,
						h3("Weight & growth inputs"),
						numericInput('a', 'a',value=0.00001,min = 0, max = 0.1),
						numericInput('b', 'b',value=3,min = 2, max = 4),
						numericInput('linf', 'Linf',value=600,min = 1, max = 100000),
						numericInput('k', 'k',value=0.3,min = 0.00001, max = 4),
						numericInput('t0', 't0',value=0,min = -10000, max = 10000)
						),
					column(4, 
						h3("Weight & growth inputs"),
						numericInput('maxAge', 'Maximum Age',value=10,min = 0, max = 100),	
						numericInput('RAge', 'Recruit Age',value=10,min = 0, max = 100),						
						numericInput('R', 'Number of recruits',value=1000,min = 1, max = 10000)
						)
					)# end fluid page
				
				
				),
				# END TAB PANEL 1


				# TAB PANEL 2: PLOTS		
				tabPanel("View Yield Per Recruit Graphs",
					plotOutput('f01'),
					h1("2")
					),
				# END TAB PANEL 2: PLOTS		


				# TAB PANEL 3: SIMULATION OUTPUT	
				tabPanel("View simulation data",
					tableOutput('sims'),
					h1("3")
					)
				# END TAB PANEL 3: SIMULATION OUTPUT	
			)# END TABSET PANEL
		) # END MAIN PANEL
    ) # END SIDEBAR PANEL
    )# END SIDEBAR LAYOUT
	) # END TABPANEL
	)# END NAVBAR
	# END END MAIN PANEL


	
	
	
