# https://gist.github.com/dgrapov/5792778
# UI for app
shinyUI(pageWithSidebar(
	# title
	headerPanel("Select Options"),
	
	#input
	sidebarPanel
	(
		selectInput("plottype","Plot Type:", 
			    list("team comparison" = "teamcomparison", boxplot = "boxplot")
			    ),
		#selectInput("league","League:", 
		#	list(uiOutput("leagues")), # depends on dataset ( set by ____ in server.R)
		uiOutput("team1"),      # depends on dataset ( set by ____ in server.R)
		uiOutput("team2"),      # depends on dataset ( set by ____ in server.R)
		# put a conditional here to only display y-axis selection if relevant to plot.type
		uiOutput("xvariable"),  # depends on dataset	( set by output$group in  server.R)
		uiOutput("yvariable"), 	# depends on dataset ( set by output$variable in  server.R)
		checkboxInput("show.points", "show points", TRUE)
	),	

	# output				
	mainPanel(
		h3(textOutput("caption")),
		uiOutput("plot") # depends on input 
	)
))

