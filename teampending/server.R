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
			
	output$yvariable <- renderUI({ 
	  obj<-matches
		var.opts<-namel(colnames(obj)[18:ncol(obj)])
		selectInput("yvariable","Y-axis:", var.opts) # update UI 				 
		}) 
		
	output$caption<-renderText({
		switch(input$plottype,
			"winloss" 	=	"How rating spread affects game outcome in each league",
			"teamcomparison" 		=	"Comparison of total player ratings between two teams in a league")
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
	                                    show.legend=T),
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
	    leagues <- rep(input$leagues, nrow(plot.obj$data))
	    home <- plot.obj$data[which(plot.obj$data[,'home_team_long_name'] == selected_team),]
	    away <- plot.obj$data[which(plot.obj$data[,'home_team_long_name'] == away_team),]
	    league <- plot.obj$data[which(plot.obj$data[,'home_team_long_name'] == leagues),]
	    comparison <- rbind(home, away, league)
	    p<-ggplot(comparison, 
				aes(
				  x = comparison[,'variable'], 
				  y = comparison[,'value'],
				  fill      = comparison[,'home_team_long_name']
				)
		) + plottype + ylab("Rating") + labs(list(x = "", y = "Rating Value")) +
		    theme(axis.title.x=element_text(size=20), 
		          axis.title.y=element_text(size=20),
		          axis.text.x=element_text(size=20),
		          axis.text.y=element_text(size=20),
		          legend.text = element_text(size=14)) +
	      scale_fill_discrete(name="")    
		    #coord_cartesian(ylim=c(600,950)) +
		    #scale_colour_manual(values = c("blue","yellow", "orange"))  +
		    #scale_fill_manual(values = c("green", "grey", "red"))
	      
	}
	
	else if(input$plottype=="winloss")	{
	  plot.obj$data <- matches[matches$league_name==input$leagues,]
	  yvariable<<-matches[matches$league_name==input$leagues,input$yvariable] 
	  #draws <- plot.obj$data[which(plot.obj$data[,'win_loss'] == 'Draw'),]
	  #draws <- mean(draws$yvariable)
		p<-ggplot(plot.obj$data, 
				aes(
					x 		= matches[matches$league_name==input$leagues,'win_loss'], 
					y 		= yvariable,
					fill 	= as.factor(matches[matches$league_name==input$leagues,'win_loss'])
				)
		) + plottype + labs(x= "", y = input$yvariable) + 
		                theme(axis.title.x=element_text(size=20), 
		                     axis.title.y=element_text(size=20),
		                     axis.text.x=element_text(size=20),
		                     axis.text.y=element_text(size=20),
		                     legend.position="none") + 
		  #scale_fill_manual(values = alpha(c("blue", "red", "green"), c(abs(draws), 0.2, 1)))
			scale_fill_manual(values = c("gray", "red", "green"))	
	} 
	
	.theme
	print(p)
	})	
})


