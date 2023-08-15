#loading "The Rolling Stones" dataset
dataset <- read.csv('stones_analysis.csv',stringsAsFactors = FALSE,check.names = FALSE)

#examining the structure of dataset
str(dataset)
#examining the summary of dataset
summary(dataset)       

#transforming dataset
source('Utility.R')
dataset <- transormed.dataframe(dataset = dataset)

#checking for NA values
all(complete.cases(dataset))

#examining the summary of transformed dataset
summary(dataset)       

# k- means algorithm works with numeric variables
num.vars <- as.numeric(which(sapply(dataset, is.numeric)))
num.vars

# checking for outliers
apply(X = dataset[,num.vars], # all numeric variables
      MARGIN = 2,
      FUN = function(x) length(boxplot.stats(x)$out))
# Track number has 3, Song duration 10, instrumentalness 54, liveness 14, loudness 1,
# speechiness 25, tempo 6 and valence 2 outliers


# install.packages('DescTools')
library(DescTools)
# let's see outliers for 'Track number'
boxplot(dataset$`Track number`, xlab = 'Track number')
# all	outliers	are	overly	high	values.	So,	we	will	replace	them	with	the	95th	percentile
track_number.w <- Winsorize(dataset$`Track number`, 
                       probs = c(0,0.95)) 
boxplot(track_number.w, xlab='Track number winsorized')

# let's see outliers for 'Song duration'
boxplot(dataset$`Song duration`, xlab =  'Song duration')
# variable 'Song duration' has both overly low and high values
# Low values will be replaced with 5th percentil, whereas high values will be replaced with 95th
song_duration.w <- Winsorize(dataset$`Song duration`, probs = c(0.05, 0.95))
boxplot(song_duration.w, xlab = 'Song duration winsorized')

# let's see outliers for 'instrumentalness'
boxplot(dataset$instrumentalness, xlab =  'instrumentalness')
# variable 'instrumentalness' has only overly high values
instrumentalness.w <- Winsorize(dataset$instrumentalness,probs = c(0,0.815))
# for this variable, overly high values were replaced with 81.5th percentil
boxplot(instrumentalness.w, xlab = 'instrumentalness winsorized')

# let's see outliers for 'liveness'
boxplot(dataset$liveness, xlab =  'liveness')
# variable 'liveness' has overly high values
liveness.w <- Winsorize(dataset$liveness, probs = c(0,0.95))
boxplot(liveness.w, xlab = 'liveness winsorized')

# let's see outliers for 'loudness'
boxplot(dataset$loudness, xlab =  'loudness')
# variable 'loudness' has 1 overly low value that will be replaced with 5th percentile
loudness.w <- Winsorize(dataset$loudness, probs = c(0.05,1))
boxplot(loudness.w, xlab = 'loudness winsorized')

# let's see outliers for 'speechiness'
boxplot(dataset$speechiness, xlab =  'speechintess')
# variable 'speechiness' has overly high values that will be replaced with 95th percentile
speechiness.w <- Winsorize(dataset$speechiness, probs = c(0,0.915))
boxplot(speechiness.w, xlab = 'speechiness winsorized')
# overly high values were replaced with 91.5th percentile instead of 95th,
# because all values couldn't be replaced with 95th percentile

# let's see outliers for 'tempo'
boxplot(dataset$tempo, xlab =  'tempo')
# variable 'tempo' has overly high values that will be replaced with 95th percentile
tempo.w <- Winsorize(dataset$tempo, probs = c(0,0.95))
boxplot(tempo.w, xlab = 'tempo winsorized')

# let's see outliers for 'valence'
boxplot(dataset$valence, xlab =  'valence')
# variable 'valence' has overly low values that will be replaced with 5th percentile
valence.w <- Winsorize(dataset$valence, probs = c(0.05,1))
boxplot(valence.w, xlab = 'valence winsorized')

# updating dataset with winsorized variables
dataset$`Track number` <- track_number.w
dataset$`Song duration` <- song_duration.w
dataset$instrumentalness <- instrumentalness.w
dataset$liveness <- liveness.w
dataset$loudness <- loudness.w
dataset$speechiness <- speechiness.w
dataset$tempo <- tempo.w
dataset$valence <- valence.w

# checking if there are any outliers left
apply(X = dataset[,num.vars], # all numeric variables
      MARGIN = 2,
      FUN = function(x) length(boxplot.stats(x)$out))

# checking summary
summary(dataset[,num.vars])
