# https://gist.github.com/dgrapov/5792778
# UI for app
shinyUI(pageWithSidebar(
	# title
	headerPanel("Select Options"),
	
	#input
	sidebarPanel
	(
		selectInput("dataset","Data:", 
						list(test = "test", iris = "iris", mtcars = "mtcars")
						),
		uiOutput("xvariable"),  		# depends on dataset	( set by output$group in  server.R)
		uiOutput("yvariable"), 	# depends on dataset ( set by output$variable in  server.R)
		selectInput("plot.type","Plot Type:", 
						list(boxplot = "boxplot", histogram = "histogram", density = "density", bar = "bar")
						),
		checkboxInput("show.points", "show points", TRUE)
	),	

	# output				
	mainPanel(
		h3(textOutput("caption")),
		#h3(htmlOutput("caption")),
		uiOutput("plot") # depends on input 
	)
))

