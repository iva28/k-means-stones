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

# k- means algoritm works with numeric variables
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
