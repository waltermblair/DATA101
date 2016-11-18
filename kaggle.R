library(RSQLite)

# Using https://www.r-bloggers.com/using-sqlite-in-r/
# open connection to database
con <- dbConnect(SQLite(), dbname="soccer.sqlite")

# get a list of all tables
alltables <- dbListTables(con)

# Using http://www.w3schools.com/sql/sql_select.asp
# create dataframes from database tables
Match <- dbGetQuery(con, 'select * from Match')
Team <- dbGetQuery(con, 'select team_api_id,team_fifa_api_id,team_short_name from Team')
Player <- dbGetQuery(con, 'select player_api_id,player_name,player_fifa_api_id from Player')
Player_Attributes <- dbGetQuery(con, 'select player_fifa_api_id,player_api_id,date,overall_rating from Player_Attributes')

#head(Match)
#head(Player)
#head(Player_Attributes)
#head(Team)


# http://www.ats.ucla.edu/stat/r/modules/subsetting.htm
# simplify component dataframes
# taking teams and players in first five matches
match <- na.omit(Match)
#nrow(match)

# column numbers of home and away players within Match table
home_players <- 56:66
away_players <- 67:77
# other useful column numbers within Match table
home_team <- 8
away_team <- 9
home_goals <- 10
away_goals <- 11

#head(match[,home_players])
#head(match[,away_players])
home_team <- match[,home_team]
away_team <- match[,away_team]
home_goals <- match[,home_goals]
away_goals <- match[,away_goals]

match <- match[,c(1,7:11,56:66,67:77)]

### This statement cuts match down to first 100 matches
match <- match[1:100,]
cat("now working with first ", nrow(match), " rows\n")

# clean version of Match table
# head(match)

### adding useful columns to Match table
# score: pos is home win, neg is away win
match_score <- match$home_team_goal - match$away_team_goal
df <- data.frame(match, match_score)

# how to average a single player's overall_ratings
# mean(Player_Attributes[Player_Attributes$player_api_id==160243,'overall_rating'], na.rm = TRUE)

# how to sum multiple players' average overall_ratings
# sum(mean(Player_Attributes[Player_Attributes$player_api_id==160243,'overall_rating'], na.rm = TRUE), mean(Player_Attributes[Player_Attributes$player_api_id==160243,'overall_rating'], na.rm = TRUE))

# how to grab a single player id from particular match (e.g. match 1)
# match[1,7]

# how to grab all the player ids for home and away team from particular match (e.g. match1)
# home players: match[1,7:17]
# away players: match[1,18:28]

# how to average overall_rating of a single player_id pulled from a particular match
# mean(Player_Attributes[Player_Attributes$player_api_id==match[1,7],'overall_rating'], na.rm = TRUE)

# how to sum all team's players' overall_ratings from a particular match
home_team_overall_rating <- 0
for (i in 7:17) {
	home_team_overall_rating = home_team_overall_rating + mean(Player_Attributes[Player_Attributes$player_api_id==match[1,i],'overall_rating'], na.rm = TRUE)
}

away_team_overall_rating <- 0
for (j in 18:28) {
	away_team_overall_rating = away_team_overall_rating + mean(Player_Attributes[Player_Attributes$player_api_id==match[1,j],'overall_rating'], na.rm = TRUE)
}


# sum all teams' players' overall_ratings for all matches
home_team_overall_ratings <- vector(mode="numeric", length=0)
away_team_overall_ratings <- vector(mode="numeric", length=0)
for(i in 1:nrow(match)) {
	home_team_overall_rating <- 0
	away_team_overall_rating <- 0
	for (j in 7:17) {
        	home_team_overall_rating = home_team_overall_rating + mean(Player_Attributes[Player_Attributes$player_api_id==match[i,j],'overall_rating'], na.rm = TRUE)
		away_team_overall_rating = away_team_overall_rating + mean(Player_Attributes[Player_Attributes$player_api_id==match[i,j+11],'overall_rating'], na.rm = TRUE)
	}
	home_team_overall_ratings = c(home_team_overall_ratings, home_team_overall_rating)
	away_team_overall_ratings = c(away_team_overall_ratings, away_team_overall_rating)
}

# add above to df
df <- data.frame(df, home_team_overall_ratings, away_team_overall_ratings)

# remove player_id from df
df_small <- df[,c(2:4,29:31)]
head(df_small)
