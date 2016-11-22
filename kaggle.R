library(RSQLite)
library(plotly)
library(class)

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
#head(df_small)

# Basic plot of match_score to home_team_overall_ratings
# Using https://plot.ly/r/
x <- df_small$home_team_overall_ratings
y <- df_small$match_score

# Plot of mean home_team_overall_ratings to win versus loss
wins <- df_small[df_small$match_score > 0,]
losses <- df_small[df_small$match_score < 0,]
draws <- df_small[df_small$match_score == 0,]

cat("Number of wins: ",nrow(wins),"\nNumber of losses: ",nrow(losses), "\nNumber of draws: ",nrow(draws), "\n")

x <- wins$home_team_overall_ratings
y <- losses$home_team_overall_ratings
z <- draws$home_team_overall_ratings

barplot(c(mean(x),mean(y),mean(z)),names.arg=c("wins","losses","draws"),main="Not so informative",ylab="Total Team Rating",ylim=c(750,850))

# Create a column with the total_player_rating spread between the two teams
df_small$total_rating_spread <- (df_small$home_team_overall_ratings - df_small$away_team_overall_ratings)

# Plot match_score to total_rating_spread
x <- df_small$total_rating_spread
y <- df_small$match_score
plot(y~x, xlab="Home v. Away Team Rating Spread (Home Adv. > 0)", main="Maybe a positive correlationif we look at more than 100 matches?", ylab="Score Match (Home Win > 0)")

# Plot mean total_rating_spread to win versus loss
x <- df_small[df_small$match_score>0,]$total_rating_spread
y <- df_small[df_small$match_score<0,]$total_rating_spread
z <- df_small[df_small$match_score==0,]$total_rating_spread

barplot(c(mean(x),mean(y),mean(z)),names.arg=c("wins","losses","draws"),main="this looks super promising",ylab="Home v. Away Team Rating Spread (Home Adv. > 0)")

# Add column of win/loss/draw for convenience
win_loss <- ifelse(df_small$match_score>0, "win", ifelse(df_small$match_score<0, "loss", "draw"))
df_small <- data.frame(df_small[,1:4],win_loss,df_small[,5:7])

# Boxplot of total_rating_spread to win_loss
boxplot(total_rating_spread~win_loss,data=df_small, main="Total Rating Spread versus Win/Loss")

# KNN - find closest match neighbor using just total_rating_spread for now
# Using http://stat.ethz.ch/R-manual/R-devel/library/class/html/knn.html
#train <- df_small[11:100,]
#test <- df_small[1:10,]
#cl <- factor(df_small[11:100,'win_loss'])
#knn(train, test, cl, k=3)
