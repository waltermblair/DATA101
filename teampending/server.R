# https://gist.github.com/dgrapov/5792778
# shiny server side code for each call
shinyServer(function(input, output, session){

    matches <- read.csv("data_matches.csv")
    teams <- read.csv("data_teams.csv")
    teams_long <- read.csv("data_teams_long.csv")
    
	#update variable and group based on dataset
	output$leagues <- renderUI({ 
		obj<-matches	     
    	var.opts<-sort(obj[!duplicated(obj$league_name),'league_name'])
		selectInput("leagues","League:", var.opts) # update UI 				 
		}) 
	
	output$team1 <- renderUI({ 
	    obj<-teams   
		var.opts<-teams[teams$league_name==input$leagues,'home_team_long_name']
		selectInput("team1","Team 1:", c("Select a team", as.character(var.opts))) # update UI 				 
		}) 
		
	output$team2 <- renderUI({ 
	    obj<-teams   
		var.opts<-teams[teams$league_name==input$leagues,'home_team_long_name']
		selectInput("team2","Team 2:", c("Select a team", as.character(var.opts))) # update UI 				 
		}) 
			
	#output$xvariable <- renderUI({ 
	#    obj<-switch(input$dataset,
    #       "matches" = matches,
    #       "teams" = teams)	  	 
	#	var.opts<-namel(colnames(obj))
	#	selectInput("xvariable","X-axis:", var.opts) # update UI 			 
	#	}) 
		
	output$yvariable <- renderUI({ 
	    obj<-switch(input$plottype,
           "winloss" = matches,
           "teamcomparison" = teams_long)	  	 
		var.opts<-namel(colnames(obj)[8:ncol(obj)])
		selectInput("yvariable","Y-axis:", var.opts) # update UI 				 
		}) 
		
	output$caption<-renderText({
		switch(input$plottype,
			"winloss" 	=	"Home match outcomes versus key ratings across all leagues",
			"teamcomparison" 		=	"Key ratings differences between two teams in a league")
		})
			
	
	output$plot <- renderUI({
		plotOutput("p")
	})
		
	#plotting function using ggplot2
	output$p <- renderPlot({

	plot.obj<<-list() # not sure why input$X can not be used directly?
	plot.obj$data<<-switch(input$plottype,
	        "winloss" = matches,
	        "teamcomparison" = teams_long)
	
	#dynamic plotting options
	#http://stackoverflow.com/questions/6085238/adding-space-between-bars-in-ggplot2
	plottype<-switch(input$plottype,
	        "teamcomparison" 	=	geom_boxplot(
	                                    position = position_dodge(width=0.8),
	                                    width=.5,   
	                                    show.legend=F),
			"winloss" 	= 	geom_boxplot()
			
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
	
	if(input$plottype=="teamcomparison") {
	    selected_team <- rep(input$team1, nrow(plot.obj$data))
	    away_team <- rep(input$team2, nrow(plot.obj$data))
	    home <- plot.obj$data[which(plot.obj$data[,'home_team_long_name'] == selected_team),]
	    away <- plot.obj$data[which(plot.obj$data[,'home_team_long_name'] == away_team),]
	    comparison <- rbind(home, away)
	    p<-ggplot(comparison, 
				aes(
				  x = as.factor(comparison[,'variable']), 
				  y = comparison[,'value'],
				  fill      = comparison[,'home_team_long_name'],
				)
		) + plottype + ylab("Rating") + labs(list(x = "Home", y = "Away")) +
		    theme(axis.title.x=element_text(size=20), 
		          axis.title.y=element_text(size=0),
		          axis.text.x=element_text(size=15),
		          axis.text.y=element_text(size=15)) + 
		    coord_cartesian(ylim=c(600,950)) +
		    scale_colour_manual(values = c("blue","yellow", "orange"))  +
		    scale_fill_manual(values = c("green", "red", "grey")) 
	
	}
	
	else if(input$plottype=="winloss")	{
	    plot.obj$yvariable<<-with(plot.obj$data,get(input$yvariable)) 
	
		p<-ggplot(matches, 
				aes(
					x 		= matches$win_loss, 
					y 		= plot.obj$yvariable,
					fill 	= as.factor(matches$win_loss)
				)
		) + plottype + theme(axis.title.x=element_text(size=0), 
		                     axis.title.y=element_text(size=20),
		                     axis.text.x=element_text(size=20),
		                     axis.text.y=element_text(size=15),
		                     legend.position="none")
				
		#if(input$show.points==TRUE) { 
		#	p<-p+ geom_point(color='black',alpha=0.5, position = 'jitter')
		#}
				
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


