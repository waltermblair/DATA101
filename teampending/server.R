# https://gist.github.com/dgrapov/5792778
# shiny server side code for each call
shinyServer(function(input, output, session){
    obj <- read.csv("data.csv")
    
	#update variable and group based on dataset
	#output$leagues <- renderUI({ 
	#	
    #	var.opts<-namel(colnames(obj))
	#	selectInput("leagues","League:", var.opts) # update UI 				 
	#	}) 
	
	output$team1 <- renderUI({ 
		var.opts<-sort(obj[!duplicated(obj$home_team_api_id),'home_team_api_id'])
		selectInput("team1","Team 1:", var.opts) # update UI 				 
		}) 
		
	output$team2 <- renderUI({ 
		var.opts<-sort(obj[!duplicated(obj$home_team_api_id),'home_team_api_id'])
		selectInput("team2","Team 2:", var.opts) # update UI 				 
		}) 
			
	output$xvariable <- renderUI({ 
		var.opts<-namel(colnames(obj))
		#if(input$plottype != "teamcomparison"){
		    selectInput("xvariable","X-axis:", var.opts) # update UI 		
		#}		 
		}) 
		
	output$yvariable <- renderUI({ 
		var.opts<-namel(colnames(obj))
		selectInput("yvariable","Y-axis:", var.opts) # update UI 				 
		}) 
		
	output$caption<-renderText({
		switch(input$plottype,
			"boxplot" 	= 	"Boxplot",
			"histogram" =	"Histogram",
			"density" 	=	"Density plot",
			"teamcomparison" 		=	"Team comparison")
		})
			
	
	output$plot <- renderUI({
		plotOutput("p")
	})
		
	#plotting function using ggplot2
	output$p <- renderPlot({

	plot.obj<<-list() # not sure why input$X can not be used directly?
	plot.obj$data<<-obj
	plot.obj$teams<<-plot.obj$data[,'home_team_api_id']
	#if(input$plottype != "teamcomparison"){
	    plot.obj$xvariable<<-with(plot.obj$data,get(input$xvariable))
    #} 
	plot.obj$yvariable<<-with(plot.obj$data,get(input$yvariable)) 
	
	#cutting down to just selected teams
	plot.obj$team1<<-plot.obj$data[plot.obj$data$home_team_api_id == input$team1,]
	plot.obj$team2<<-plot.obj$data[plot.obj$data$home_team_api_id == input$team2,] 
	plot.obj$data_teams<<-rbind(plot.obj$team1, plot.obj$team2)
	
	#dynamic plotting options
	plottype<-switch(input$plottype,
	        "teamcomparison" 		=	geom_bar(position="dodge", stat="identity"),
			"boxplot" 	= 	geom_boxplot(),
			"histogram" =	geom_histogram(
			                    #alpha=0.5,
			                    #position="identity", 
			                    stat = "count",
			                    bins = 10),
			"density" 	=	geom_density(alpha=.75)
			
		)
		
	require(ggplot2)
	#plotting theme
	.theme<- theme(
				axis.line = element_line(colour = 'gray', size = .75), 
				panel.background = element_blank(),  
				plot.background = element_blank()
				 )	 
	
	#control for 1D or 2D graphs 
	
	if(input$plottype=="teamcomparison") {
	
	    p<-ggplot(plot.obj$data_teams, 
				aes(
				    x = as.factor(plot.obj$data_teams[,'home_team_api_id']), 
				    y = plot.obj$data_teams[,input$yvariable],
					group       = home_team_api_id,
					fill       = home_team_api_id,
				)
		) + plottype + labs(x="Team", y=input$yvariable) + 
		    theme(axis.title.x=element_text(size=20), 
		          axis.title.y=element_text(size=20),
		          axis.text.x=element_text(size=15),
		          axis.text.y=element_text(size=15))
	
	}
	
	else if(input$plottype=="boxplot")	{		
		p<-ggplot(plot.obj$data, 
				aes(
					x 		= plot.obj$xvariable, 
					y 		= plot.obj$yvariable,
					fill 	= as.factor(plot.obj$xvariable)
				)
		) + plottype
				
		if(input$show.points==TRUE) { 
			p<-p+ geom_point(color='black',alpha=0.5, position = 'jitter')
		}
				
	} 
	
	else {
		
		p<-ggplot(plot.obj$data, 
				aes(
					x 		= factor(plot.obj$xvariable),
					#group   = plot.obj$teams,
					#fill    = plot.obj$teams,
					#fill 	= as.factor(plot.obj$xvariable),
					#yvariable 	= as.factor(plot.obj$yvariable),
					#color 	= as.factor(plot.obj$xvariable)
				)
		) + plottype
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


