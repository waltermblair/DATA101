library(RSQLite)
library(lattice)
library(plotly)
library(class)
library(randomForest)
library(reshape2)

# Using https://www.r-bloggers.com/using-sqlite-in-r/
# open connection to database
con <- dbConnect(SQLite(), dbname="soccer.sqlite")

# get a list of all tables
alltables <- dbListTables(con)

# Using http://www.w3schools.com/sql/sql_select.asp
# create dataframes from database tables
League <- dbGetQuery(con, 'select * from League')
Match <- dbGetQuery(con, 'select * from Match') # contains league_id for each match
Team <- dbGetQuery(con, 'select team_api_id,team_fifa_api_id,team_short_name,team_long_name from Team')
colnames(Team)[1] <- 'home_team_api_id'
Player <- dbGetQuery(con, 'select player_api_id,player_name,player_fifa_api_id from Player')
Player_Attributes <- dbGetQuery(con, 'select player_fifa_api_id,player_api_id,date,overall_rating,potential,stamina,agility,aggression,reactions from Player_Attributes')
Team_Attributes <- dbGetQuery(con, 'select team_fifa_api_id,team_api_id,date from Team_Attributes')

# http://www.ats.ucla.edu/stat/r/modules/subsetting.htm
# simplify component dataframes
match <- na.omit(Match)

# replace league id with league name
colnames(League)[1] <- "league_id"
colnames(League)[3] <- "league_name"
League$league_name <- as.character(League$league_name)

# remove unnecessary columns and add league name
match <- match[,c(1,3,7:11,56:66,67:77)]
match <- merge(match, League[,c("league_id", "league_name")], by="league_id")
match <- data.frame(match[,'league_name'], match[,3:ncol(match)-1])

### This statement cuts match down to first n matches
match <- match[,]
cat("now working with first ", nrow(match), " rows\n")

### adding useful columns to Match table
# score: pos is home win, neg is away win
match_score <- match$home_team_goal - match$away_team_goal
df <- data.frame(match, match_score)

# sum all team's players' various attributes from a particular match
home_team_overall_rating <- 0
home_team_potential <- 0
home_team_stamina <- 0
home_team_agility <- 0
home_team_aggression <- 0
home_team_reactions <- 0
for (i in 8:18) {
	home_team_overall_rating = home_team_overall_rating + mean(Player_Attributes[Player_Attributes$player_api_id==match[1,i],'overall_rating'], na.rm = TRUE)
	home_team_potential = home_team_potential + mean(Player_Attributes[Player_Attributes$player_api_id==match[1,i],'potential'], na.rm = TRUE)
	home_team_stamina = home_team_stamina + mean(Player_Attributes[Player_Attributes$player_api_id==match[1,i],'stamina'], na.rm = TRUE)
	home_team_agility = home_team_agility + mean(Player_Attributes[Player_Attributes$player_api_id==match[1,i],'agility'], na.rm = TRUE)
	home_team_aggression = home_team_aggression + mean(Player_Attributes[Player_Attributes$player_api_id==match[1,i],'aggression'], na.rm = TRUE)
	home_team_reactions = home_team_reactions + mean(Player_Attributes[Player_Attributes$player_api_id==match[1,i],'reactions'], na.rm = TRUE)
}

away_team_overall_rating <- 0
away_team_potential <- 0
away_team_stamina <- 0
away_team_agility <- 0
away_team_aggression <- 0
away_team_reactions <- 0
for (j in 19:29) {
	away_team_overall_rating = away_team_overall_rating + mean(Player_Attributes[Player_Attributes$player_api_id==match[1,j],'overall_rating'], na.rm = TRUE)
	away_team_potential = away_team_potential + mean(Player_Attributes[Player_Attributes$player_api_id==match[1,j],'potential'], na.rm = TRUE)
	away_team_stamina = away_team_stamina + mean(Player_Attributes[Player_Attributes$player_api_id==match[1,j],'stamina'], na.rm = TRUE)
	away_team_agility = away_team_agility + mean(Player_Attributes[Player_Attributes$player_api_id==match[1,j],'agility'], na.rm = TRUE)
	away_team_aggression = away_team_aggression + mean(Player_Attributes[Player_Attributes$player_api_id==match[1,j],'aggression'], na.rm = TRUE)
	away_team_reactions = away_team_reactions + mean(Player_Attributes[Player_Attributes$player_api_id==match[1,j],'reactions'], na.rm = TRUE)
}


# sum all teams' players' variables for all matches
home_team_overall_ratings <- vector(mode="numeric", length=0)
home_team_overall_potentials <- vector(mode="numeric", length=0)
home_team_overall_staminas <- vector(mode="numeric", length=0)
home_team_overall_agilities <- vector(mode="numeric", length=0)
home_team_overall_aggressions <- vector(mode="numeric", length=0)
home_team_overall_reactions <- vector(mode="numeric", length=0)

away_team_overall_ratings <- vector(mode="numeric", length=0)
away_team_overall_potentials <- vector(mode="numeric", length=0)
away_team_overall_staminas <- vector(mode="numeric", length=0)
away_team_overall_agilities <- vector(mode="numeric", length=0)
away_team_overall_aggressions <- vector(mode="numeric", length=0)
away_team_overall_reactions <- vector(mode="numeric", length=0)

for(i in 1:nrow(match)) {
	home_team_overall_rating <- 0
	home_team_overall_potential <- 0
	home_team_overall_stamina <- 0
	home_team_overall_agility <- 0
	home_team_overall_aggression <- 0
	home_team_overall_reaction <- 0
	away_team_overall_rating <- 0
	away_team_overall_potential <- 0
	away_team_overall_stamina <- 0
	away_team_overall_agility <- 0
	away_team_overall_aggression <- 0
	away_team_overall_reaction <- 0

	for (j in 8:18) {
    home_team_overall_rating = home_team_overall_rating + mean(Player_Attributes[Player_Attributes$player_api_id==match[i,j],'overall_rating'], na.rm = TRUE)
		home_team_overall_potential = home_team_overall_potential + mean(Player_Attributes[Player_Attributes$player_api_id==match[i,j],'potential'], na.rm = TRUE)
		home_team_overall_stamina = home_team_overall_stamina + mean(Player_Attributes[Player_Attributes$player_api_id==match[i,j],'stamina'], na.rm = TRUE)
		home_team_overall_agility = home_team_overall_agility + mean(Player_Attributes[Player_Attributes$player_api_id==match[i,j],'agility'], na.rm = TRUE)
		home_team_overall_aggression = home_team_overall_aggression + mean(Player_Attributes[Player_Attributes$player_api_id==match[i,j],'aggression'], na.rm = TRUE)
		home_team_overall_reaction = home_team_overall_reaction + mean(Player_Attributes[Player_Attributes$player_api_id==match[i,j],'reactions'], na.rm = TRUE)
		away_team_overall_rating = away_team_overall_rating + mean(Player_Attributes[Player_Attributes$player_api_id==match[i,j+11],'overall_rating'], na.rm = TRUE)
		away_team_overall_potential = away_team_overall_potential + mean(Player_Attributes[Player_Attributes$player_api_id==match[i,j+11],'potential'], na.rm = TRUE)
		away_team_overall_stamina = away_team_overall_stamina + mean(Player_Attributes[Player_Attributes$player_api_id==match[i,j+11],'stamina'], na.rm = TRUE)
		away_team_overall_agility = away_team_overall_agility + mean(Player_Attributes[Player_Attributes$player_api_id==match[i,j+11],'agility'], na.rm = TRUE)
		away_team_overall_aggression = away_team_overall_aggression + mean(Player_Attributes[Player_Attributes$player_api_id==match[i,j+11],'aggression'], na.rm = TRUE)
		away_team_overall_reaction = away_team_overall_reaction + mean(Player_Attributes[Player_Attributes$player_api_id==match[i,j+11],'reactions'], na.rm = TRUE)
	}
	home_team_overall_ratings = c(home_team_overall_ratings, home_team_overall_rating)
	home_team_overall_potentials = c(home_team_overall_potentials, home_team_overall_potential)
	home_team_overall_staminas = c(home_team_overall_staminas, home_team_overall_stamina)
	home_team_overall_agilities = c(home_team_overall_agilities, home_team_overall_agility)
	home_team_overall_aggressions = c(home_team_overall_aggressions, home_team_overall_aggression)
	home_team_overall_reactions = c(away_team_overall_reactions, away_team_overall_reaction)
	away_team_overall_ratings = c(away_team_overall_ratings, away_team_overall_rating)
	away_team_overall_potentials = c(away_team_overall_potentials, away_team_overall_potential)
	away_team_overall_staminas = c(away_team_overall_staminas, away_team_overall_stamina)
	away_team_overall_agilities = c(away_team_overall_agilities, away_team_overall_agility)
	away_team_overall_aggressions = c(away_team_overall_aggressions, away_team_overall_aggression)
	away_team_overall_reactions = c(away_team_overall_reactions, away_team_overall_reaction)
	
}

# add above to df
df <- data.frame(df, home_team_overall_ratings, home_team_overall_potentials, home_team_overall_staminas, home_team_overall_agilities, home_team_overall_aggressions, away_team_overall_ratings, away_team_overall_potentials, away_team_overall_staminas, away_team_overall_agilities, away_team_overall_aggressions)

# remove player_id from df
df <- df[,c(1,3:7,30:ncol(df))]

# split off df_teams (average team info independent of matches) and df_matches (match info)
df_teams <- df
df_matches <- df

## Work on df_matches
# Add column of win/loss/draw to df_matches for convenience
win_loss <- ifelse(df_matches$match_score>0, "Win", ifelse(df_matches$match_score<0, "Loss", "Draw"))
df_matches <- data.frame(df_matches[,c(1:4,7)],win_loss,df_matches[,8:ncol(df_matches)])
colnames(df_matches)[1] <- "league_name"

# Create a column with the spread of these variables between the two teams for each match
df_matches$total_rating_spread <- (df_matches$home_team_overall_ratings - df_matches$away_team_overall_ratings)
df_matches$total_potential_spread <- (df_matches$home_team_overall_potentials - df_matches$away_team_overall_potentials)
df_matches$total_stamina_spread <- (df_matches$home_team_overall_staminas - df_matches$away_team_overall_staminas)
df_matches$total_agility_spread <- (df_matches$home_team_overall_agilities - df_matches$away_team_overall_agilities)
df_matches$total_aggression_spread <- (df_matches$home_team_overall_aggressions - df_matches$away_team_overall_aggressions)

# simplify column names for the sake of shiny
colnames(df_matches)[17] <- "Overall"
colnames(df_matches)[18] <- "Potential"
colnames(df_matches)[19] <- "Stamina"
colnames(df_matches)[20] <- "Agility"
colnames(df_matches)[21] <- "Aggression"

## Work on df_teams
df_teams <- merge(df_teams, Team[,c('home_team_api_id', 'team_short_name', 'team_long_name')], by='home_team_api_id')
df_teams <- data.frame(df_teams[,c(2,1,(ncol(df_teams)-1):ncol(df_teams), 4, 8:(ncol(df_teams)-2))])
colnames(df_teams)[1] <- "league_name"
colnames(df_teams)[3] <- "home_team_short_name"
colnames(df_teams)[4] <- "home_team_long_name"
colnames(Team)[1] <- "away_team_api_id"
df_teams <- merge(df_teams, Team[,c('away_team_api_id', 'team_short_name', 'team_long_name')], by='away_team_api_id')
df_teams <- data.frame(df_teams[,c(2:5,1,(ncol(df_teams)-1):(ncol(df_teams)), 6:(ncol(df_teams)-2))])
colnames(df_teams)[6] <- "away_team_short_name"
colnames(df_teams)[7] <- "away_team_long_name"

# convert character columns into characters
df_teams$league_name <- as.factor(df_teams$league_name)
df_teams$home_team_short_name <- as.character(df_teams$home_team_short_name)
df_teams$home_team_long_name <- as.character(df_teams$home_team_long_name)
df_teams$away_team_short_name <- as.character(df_teams$away_team_short_name)
df_teams$away_team_long_name <- as.character(df_teams$away_team_long_name)

# split up home and away
df_home <- df_teams[,c(1:4, 8:12)]
df_away <- df_teams[,c(1,5:7,13:ncol(df_teams))]

# simplify column names for the sake of shiny
colnames(df_home)[5] <- "Overall"
colnames(df_home)[6] <- "Potential"
colnames(df_home)[7] <- "Stamina"
colnames(df_home)[8] <- "Agility"
colnames(df_home)[9] <- "Aggression"

# long skinny format
# See http://stackoverflow.com/questions/22305023/how-to-get-a-barplot-with-several-variables-side-by-side-grouped-by-a-factor
#df_home<-aggregate(df_home,by=list(df_home$league_name, df_home$home_team_short_name, df_home$home_team_long_name),mean)
#colnames(df_home)[1] <- "league_name"
#colnames(df_home)[2] <- "home_team_short_name"
#colnames(df_home)[3] <- "home_team_long_name"
#df_home <- df_home[,c(1:3, 5, 8:ncol(df_home))]
df_home_long<-melt(df_home,id.vars=c("home_team_long_name", "league_name", "home_team_short_name", "home_team_api_id"))
df_home_league <- df_home_long
df_home_league <- data.frame(df_home_league[,2],df_home_league[,1],df_home_league[,3:ncol(df_home_league)])
colnames(df_home_league)[1] <- "home_team_long_name"
colnames(df_home_league)[2] <- "league_name"
df_home_long <- rbind(df_home_long, df_home_league)

# Output to csv
write.csv(df_matches, file = "data_matches.csv")
write.csv(df_home_long, file = "data_teams_long.csv")
write.csv(df_home, file = "data_teams.csv")

