# KNN Homework
# Creating dataframe
# See http://www.dummies.com/programming/r/how-to-create-a-data-frame-from-scratch-in-r/

PassengerId <- c(1:5)
Survived <- c(0,1,1,1,0)
Pclass <- c(3,1,3,1,3)
Sex <- c('male', 'female', 'female', 'female', 'male')
Age <- c(22, 38, 26, 35, 35)
SibSp <- c(1,1,0,1,0)
Fare <- c(7.25, 71.2833, 7.925, 53.1, 8.05)

df <- data.frame(PassengerId, Survived, Pclass, Sex, Age, SibSp, Fare)

# Calculating distances by Age
dist1.2 <- abs(df[1,'Age']-df[2,'Age'])
dist1.3 <- abs(df[1,'Age']-df[3,'Age'])
dist1.4 <- abs(df[1,'Age']-df[4,'Age'])
dist1.5 <- abs(df[1,'Age']-df[5,'Age'])

# Adding distanceByAge to dataframe
distanceByAge <- c(Inf, dist1.2, dist1.3, dist1.4, dist1.5)
df <- data.frame(df, distanceByAge)

# Selecting closest neighbor by age
# See http://stackoverflow.com/questions/3445590/how-to-extract-a-subset-of-a-data-frame-based-on-a-condition-involving-a-field
minDist <- min(distanceByAge)
closestByAge <- df[distanceByAge == minDist,]
print('The closest neighbor by age:')
closestByAge

# Calculating distance by Age and Fare
# Euclidian distance: sqrt(distOne^2 + distTwo^2)
dist1.2 <- sqrt((df[1,'Age']-df[2,'Age'])^2 + (df[1,'Fare']-df[2,'Fare'])^2)
dist1.3 <- sqrt((df[1,'Age']-df[3,'Age'])^2 + (df[1,'Fare']-df[3,'Fare'])^2)
dist1.4 <- sqrt((df[1,'Age']-df[4,'Age'])^2 + (df[1,'Fare']-df[4,'Fare'])^2)
dist1.5 <- sqrt((df[1,'Age']-df[5,'Age'])^2 + (df[1,'Fare']-df[5,'Fare'])^2)

# Adding distanceAgeFare to dataframe
distanceAgeFare <- c(Inf, dist1.2, dist1.3, dist1.4, dist1.5)
df <- data.frame(df, distanceAgeFare)

# Selecting closest neighbor by age and fare
minDist <- min(distanceAgeFare)
closestAgeFare <- df[distanceAgeFare == minDist,]
print('The closest neighbor by age & fare:')
closestAgeFare

# KNN classification using Age & Fare

