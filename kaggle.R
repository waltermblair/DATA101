library(RSQLite)
library(plotly)
library(class)
library(randomForest)

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
Player_Attributes <- dbGetQuery(con, 'select player_fifa_api_id,player_api_id,date,overall_rating,potential,stamina,agility,aggression,reactions from Player_Attributes')
Team_Attributes <- dbGetQuery(con, 'select team_fifa_api_id,team_api_id,date from Team_Attributes')

# http://www.ats.ucla.edu/stat/r/modules/subsetting.htm
# simplify component dataframes
# taking teams and players in first five matches
match <- na.omit(Match)

match <- match[,c(1,7:11,56:66,67:77)]

### This statement cuts match down to first 1000 matches
match <- match[1:1000,]
cat("now working with first ", nrow(match), " rows\n")

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

# how to sum all team's players' various attributes from a particular match
home_team_overall_rating <- 0
home_team_potential <- 0
home_team_stamina <- 0
home_team_agility <- 0
home_team_aggression <- 0
home_team_reactions <- 0
for (i in 7:17) {
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
for (j in 18:28) {
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

	for (j in 7:17) {
        	home_team_overall_rating = home_team_overall_rating + mean(Player_Attributes[Player_Attributes$player_api_id==match[i,j],'overall_rating'], na.rm = TRUE)
		home_team_overall_potential = home_team_overall_potential + mean(Player_Attributes[Player_Attributes$player_api_id==match[i,j],'potential'], na.rm = TRUE)
		home_team_overall_stamina = home_team_overall_stamina + mean(Player_Attributes[Player_Attributes$player_api_id==match[i,j],'stamina'], na.rm = TRUE)
		home_team_overall_agility = home_team_overall_agility + mean(Player_Attributes[Player_Attributes$player_api_id==match[i,j],'agility'], na.rm = TRUE)
		home_team_overall_aggression = home_team_overall_aggression + mean(Player_Attributes[Player_Attributes$player_api_id==match[i,j],'aggression'], na.rm = TRUE)
		home_team_overall_reaction = home_team_overall_reaction + mean(Player_Attributes[Player_Attributes$player_api_id==match[i,j],'reaction'], na.rm = TRUE)
		away_team_overall_rating = away_team_overall_rating + mean(Player_Attributes[Player_Attributes$player_api_id==match[i,j+11],'overall_rating'], na.rm = TRUE)
		away_team_overall_potential = away_team_overall_potential + mean(Player_Attributes[Player_Attributes$player_api_id==match[i,j+11],'potential'], na.rm = TRUE)
		away_team_overall_stamina = away_team_overall_stamina + mean(Player_Attributes[Player_Attributes$player_api_id==match[i,j+11],'stamina'], na.rm = TRUE)
		away_team_overall_agility = away_team_overall_agility + mean(Player_Attributes[Player_Attributes$player_api_id==match[i,j+11],'agility'], na.rm = TRUE)
		away_team_overall_aggression = away_team_overall_aggression + mean(Player_Attributes[Player_Attributes$player_api_id==match[i,j+11],'aggression'], na.rm = TRUE)
		away_team_overall_reaction = away_team_overall_reaction + mean(Player_Attributes[Player_Attributes$player_api_id==match[i,j+11],'reaction'], na.rm = TRUE)
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
df_small <- df[,c(2:4,29:ncol(df))]

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

# Create a column with the spread of these variables between the two teams
df_small$total_rating_spread <- (df_small$home_team_overall_ratings - df_small$away_team_overall_ratings)
df_small$total_potential_spread <- (df_small$home_team_overall_potentials - df_small$away_team_overall_potentials)
df_small$total_stamina_spread <- (df_small$home_team_overall_staminas - df_small$away_team_overall_staminas)
df_small$total_agility_spread <- (df_small$home_team_overall_agilities - df_small$away_team_overall_agilities)
df_small$total_aggression_spread <- (df_small$home_team_overall_aggressions - df_small$away_team_overall_aggressions)

# Plot match_score to total_rating_spread
x <- df_small$total_rating_spread
y <- df_small$match_score
plot(y~x, xlab="Home v. Away Team Rating Spread (Home Adv. > 0)", main="Maybe a positive correlationif we look at more than 100 matches?", ylab="Score Match (Home Win > 0)")

# Plot mean total_rating_spread to win versus loss
x <- df_small[df_small$match_score>0,]$total_rating_spread
y <- df_small[df_small$match_score<0,]$total_rating_spread
z <- df_small[df_small$match_score==0,]$total_rating_spread

barplot(c(mean(x),mean(y),mean(z)),names.arg=c("wins","losses","draws"),main="this looks super promising",ylab="Home v. Away Team Rating Spread (Home Adv. > 0)",ylim=c(750,850))

# Add column of win/loss/draw for convenience
win_loss <- ifelse(df_small$match_score>0, "win", ifelse(df_small$match_score<0, "loss", "draw"))
df_small <- data.frame(df_small[,1:4],win_loss,df_small[,5:ncol(df_small)])

# Boxplot of total_rating_spread to win_loss
boxplot(total_rating_spread~win_loss,data=df_small, main="Total Rating Spread versus Win/Loss")

# KNN - find closest match neighbor using just total_rating_spread for now
# Using http://stat.ethz.ch/R-manual/R-devel/library/class/html/knn.html
n = nrow(match)

train <- df_small[(n/10+1):n,'total_rating_spread']
test <- df_small[1:(n/10),'total_rating_spread']
cl <- df_small[(n/10+1):n,'win_loss']
actual <- df_small[1:(n/10),'win_loss']
KNN <- knn(data.frame(train), data.frame(test), cl, k=10)
cat("KNN Accuracy (rating spread): ", (length(which(actual == KNN))/length(actual) * 100), "%\n")

# Adding raw team rating to above
train <- df_small[(n/10+1):n,c('home_team_overall_ratings','total_rating_spread')]
test <- df_small[1:(n/10),c('home_team_overall_ratings','total_rating_spread')]
cl <- df_small[(n/10+1):n,'win_loss']
actual <- df_small[1:(n/10),'win_loss']
KNN <- knn(train, test, cl, k=10)
cat("KNN Accuracy (spread + total rating): ", (length(which(actual == KNN))/length(actual) * 100), "%\n")

# Adding other variables as well
train <- df_small[(n/10+1):n,c('total_rating_spread', 'total_potential_spread', 'total_stamina_spread', 'total_agility_spread', 'total_aggression_spread')] 
test <- df_small[1:(n/10),c('total_rating_spread', 'total_potential_spread', 'total_stamina_spread', 'total_agility_spread', 'total_aggression_spread')]
cl <- df_small[(n/10+1):n,'win_loss']
actual <- df_small[1:(n/10),'win_loss']
KNN <- knn(train, test, cl, k=10)
cat("KNN Accuracy (all variables): ", (length(which(actual == KNN))/length(actual) * 100), "%\n")


# Random forest approach on just team rating spread and ID columns
cat("Random forest with overall player ratings plus ID columns:\n ")
remove.inxs = which(colnames(df_small) == 'match_score')
df_tiny = df_small[,-remove.inxs]
fit <- randomForest(win_loss ~ .,
                    data=df_tiny,
                    ntree=500)
print(fit)

# Random forest approach on just team rating spread
cat("Random forest with just spread of overall player ratings:\n ")
df_tiny = df_small[,c(4,5,16)]
remove.inxs = which(colnames(df_tiny) == 'match_score')
                df_tiny = df_tiny[,-remove.inxs]
fit <- randomForest(win_loss ~ .,
                    data=df_tiny,
                    ntree=500)
print(fit)

# Random forests approach on spread of all variables so far
# Take out raw variable columns, we just want the spread columns
cat("Random forest with spread in other variables as well:\n ")
df_tiny = df_small[,c(4,5,16:ncol(df_small))]
remove.inxs = which(colnames(df_tiny) == 'match_score')
                df_tiny = df_tiny[,-remove.inxs]
fit <- randomForest(win_loss ~ .,
                    data=df_tiny,
                    ntree=500)

print(fit)


# Tweak KNN options - k, test/train
# Additional visualizations
# ANCOVA
# Could try matching player rating with the date of the match...tough


