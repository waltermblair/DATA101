# https://gist.github.com/dgrapov/5792778
# shiny server side code for each call
shinyServer(function(input, output, session){
    test <- read.csv("data.csv")
    
	#update variable and group based on dataset
	output$xvariable <- renderUI({ 
		obj<-switch(input$dataset,
           "iris" = iris,
           "mtcars" = mtcars,
           "test" = test)	 
		var.opts<-namel(colnames(obj))
		selectInput("xvariable","X-axis:", var.opts) # update UI 				 
		}) 
		
	output$yvariable <- renderUI({ 
		obj<-switch(input$dataset,
           "iris" = iris,
           "mtcars" = mtcars,
           "test" = test)	 
		var.opts<-namel(colnames(obj))
		selectInput("yvariable","Y-axis:", var.opts) # update UI 				 
		}) 
		
	output$caption<-renderText({
		switch(input$plot.type,
			"boxplot" 	= 	"Boxplot",
			"histogram" =	"Histogram",
			"density" 	=	"Density plot",
			"bar" 		=	"Bar graph")
		})
			
	
	output$plot <- renderUI({
		plotOutput("p")
	})
		
	#plotting function using ggplot2
	output$p <- renderPlot({

	plot.obj<<-list() # not sure why input$X can not be used directly?
	plot.obj$data<<-get(input$dataset) 
	plot.obj$yvariable<<-with(plot.obj$data,get(input$yvariable)) 
	plot.obj$xvariable<<-with(plot.obj$data,get(input$xvariable)) 
	
	#dynamic plotting options
	plot.type<-switch(input$plot.type,
			"boxplot" 	= 	geom_boxplot(),
			"histogram" =	geom_histogram(
			                    #alpha=0.5,
			                    #position="identity", 
			                    stat = "count",
			                    bins = 10),
			"density" 	=	geom_density(alpha=.75),
			"bar" 		=	geom_bar(position="dodge")
		)
		
	require(ggplot2)
	#plotting theme
	.theme<- theme(
				axis.line = element_line(colour = 'gray', size = .75), 
				panel.background = element_blank(),  
				plot.background = element_blank()
				 )	 
	if(input$plot.type=="boxplot")	{		#control for 1D or 2D graphs 
		p<-ggplot(plot.obj$data, 
				aes(
					x 		= plot.obj$xvariable, 
					y 		= plot.obj$yvariable,
					fill 	= as.factor(plot.obj$xvariable)
					)
				) + plot.type
				
				if(input$show.points==TRUE)
				{ 
					p<-p+ geom_point(color='black',alpha=0.5, position = 'jitter')
				}
				
		} else {
		
		p<-ggplot(plot.obj$data, 
				aes(
					x 		= factor(plot.obj$xvariable),
					#fill 	= as.factor(plot.obj$xvariable),
					#yvariable 	= as.factor(plot.obj$yvariable),
					#color 	= as.factor(plot.obj$xvariable)
					)
				) + plot.type
		}
		
	 p<-p+labs(
			fill 	= "",
			x 		= input$xvariable,
			y 		= input$yvariable
			)  +
	.theme
	print(p)
	})	
})


