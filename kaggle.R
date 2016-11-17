library(sqldf)
library(dplyr)
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

head(Match)
#head(Player)
#head(Player_Attributes)
head(Team)


# http://www.ats.ucla.edu/stat/r/modules/subsetting.htm
# simplify component dataframes
# taking teams and players in first five matches
match <- na.omit(Match)
nrow(match)

# column numbers of home and away players within Match table
home_players <- 56:66
away_players <- 67:77
# column numbers of home team and away team within Match table
home_team <- 8
away_team <- 9

head(match[,home_players])
head(match[,away_players])
head(match[,'home_team_api'])
head(match[,'away_team_api'])

# how figure out roster of each team?


# Using https://cran.r-project.org/web/packages/dplyr/dplyr.pdf
# assemble our dataframe
