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
# rescaling variables, since for example values for variable 'Year Released' go from 1964 to 2016
# whereas for variable 'Song duration' vary from 129.8 to 367.6

dataset.norm <- as.data.frame(apply(dataset[,num.vars], MARGIN = 2, normalize.var))
summary(dataset.norm)

# checking for correlations among variables
dataset.cor <- cor(dataset.norm)
library(corrplot)
corrplot.mixed(dataset.cor)
# variables 'Year Recorded' and 'Year Released' are absolutely correlated
# Also, variables 'energy' and 'loudness' are highly correlated
# we will exclude 'Year Released' and loudness and examine again
dataset.norm$`Year Released` <- NULL
dataset.norm$loudness <- NULL

# finding the best k
eval.metrics <- data.frame()
for (k in 2:8) {
  set.seed(1)
  km <- kmeans(dataset.norm, centers = k, iter.max = 20, nstart = 1000)
  eval.metrics <- rbind(eval.metrics, c(k, km$tot.withinss, km$betweenss/km$totss))
}
colnames(eval.metrics) <- c("clusters", "tot.withinss","ratio")
eval.metrics

library(ggplot2)
ggplot(data = eval.metrics, mapping = aes(x = 2:8, y = tot.withinss)) +geom_line() + geom_point()

dataset.diff <- apply(eval.metrics[,2:3], MARGIN = 2,compute.difference)
dataset.diff
# the second column presents the decrease of tot.withinss when k+1
# the third column presents the increase in ration (betweenss/totss) when k+1

# the best values are for second row of dataset.diff when k = 3
# for k = 3 there is the biggest decrease in totwithinss and the biggest increase in ration

# doing k - means algorithm for k = 3
k <- 3
dataset.kmeans <- kmeans(x = dataset.norm, centers = k, iter.max = 20, nstart = 1000)
dataset.kmeans

stats.kmeans <- summary.stats(dataset.norm, dataset.kmeans$cluster,k)
stats.kmeans
# we have 3 clusters with  106, 74 and 117 observations respectively

# First cluster consists of songs that are the newest, the longest
# The acousticness of these songs is the lowest, meaning they are more likely to have higher proportion of digitally generated
# sounds( such as synthesized instruments, electronic beets and processed vocals) and lower proportion of traditional acoustic
# instruments like guitars, violins...
# The energy of songs in this cluster is the highest, also the tempo and speechiness

dataset$`Cluster participation` <- dataset.kmeans$cluster
songs.1st.cluster <- dataset[dataset$`Cluster participation` == 1,1]
songs.1st.cluster

# Second cluster consists of songs that are the shortest
# The acousticness of these songs is higher, meaning they have larger proportion of traditional acoustic instruments( guitars, violins..)
# The instrumentalness of these songs very high, meaning they are mostly instrumental and they lack significant vocal elements
# The valence in the second cluster is the highest. Valence is an audio feature used for describing emotional mood or tone of songs( value near 0 implies sad/negative mood, value near 1 implies positive/happy mood)
# Sings in this cluster, are the most positive and happy out of all songs from the discography

songs.2nd.cluster <- dataset[dataset$`Cluster participation` == 2,1]
songs.2nd.cluster

# Third cluster consists of the songs that are the oldest and the acousticness of these songs is the highest
# The energy feature is the lowest suggesting that these songs are the most soothing, relaxing and calm
# Also the speechiness, tempo and valence have the lowest values for this cluster, meaning
# that songs are more likely to be instrumental, with lower tempo and conveying negative emotional tone

songs.3rd.cluster <- dataset[dataset$`Cluster participation` == 3,1]
songs.3rd.cluster

#create.comparison.plots(df = dataset.norm, clust = as.factor(dataset.kmeans$cluster))
