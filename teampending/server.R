# https://gist.github.com/dgrapov/5792778
# shiny server side code for each call
shinyServer(function(input, output, session){
    obj <- read.csv("data_home.csv")
    
	#update variable and group based on dataset
	#output$leagues <- renderUI({ 
	#	
    #	var.opts<-namel(colnames(obj))
	#	selectInput("leagues","League:", var.opts) # update UI 				 
	#	}) 
	
	output$team1 <- renderUI({ 
		var.opts<-obj$team_short_name
		selectInput("team1","Team 1:", c("Select a team", as.character(var.opts))) # update UI 				 
		}) 
		
	output$team2 <- renderUI({ 
		var.opts<-sort(obj[!duplicated(obj$team_short_name),'team_short_name'])
		selectInput("team2","Team 2:", c("Select a team", as.character(var.opts))) # update UI 				 
		}) 
			
	output$xvariable <- renderUI({ 
		var.opts<-namel(colnames(obj))
		selectInput("xvariable","X-axis:", var.opts) # update UI 			 
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
	plot.obj$xvariable<<-with(plot.obj$data,get(input$xvariable))
	plot.obj$yvariable<<-with(plot.obj$data,get(input$yvariable)) 
	
	#dynamic plotting options
	#http://stackoverflow.com/questions/6085238/adding-space-between-bars-in-ggplot2
	plottype<-switch(input$plottype,
	        "teamcomparison" 	=	geom_bar(
	                                    position = position_dodge(width=0.9),
	                                    stat="identity", 
	                                    width=0.8,   
	                                    show.legend=F),
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
	# http://stackoverflow.com/questions/30440335/r-comparing-values-in-a-vector-to-a-single-value-using-the-apply-family
	
	selected_team <- rep(input$team1, nrow(plot.obj$data))
	away_team <- rep(input$team2, nrow(plot.obj$data))
	
	if(input$plottype=="teamcomparison") {
	    p<-ggplot(plot.obj$data_teams, 
				aes(
				    x = as.factor(plot.obj$data[,'variable']), 
				    y = plot.obj$data[,'value'],
					group       = plot.obj$data[,'team_short_name'],
					fill      = plot.obj$data[,'team_short_name'] == selected_team,
					color     = plot.obj$data[,'team_short_name'] == away_team
				)
		) + plottype + labs(x="Team", y=input$yvariable) + 
		    theme(axis.title.x=element_text(size=20), 
		          axis.title.y=element_text(size=20),
		          axis.text.x=element_text(size=15),
		          axis.text.y=element_text(size=15)) + 
		    coord_cartesian(ylim=c(600,950)) +
		    scale_colour_manual(values = c("grey","red", "red"))  +
		    scale_fill_manual(values = c("grey", "green", "red")) 
	
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
					#x 		= factor(plot.obj$xvariable),
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


