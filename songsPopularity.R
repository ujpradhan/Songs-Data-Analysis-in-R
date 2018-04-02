#I am revisiting the Songs dataset which I came across in The Analytics Edge course
#We will analyze the different song attributes and observe their relationship with the song's popularity
#with the potential popularity of the song using the power of logistic regression model

#reading the song.csv file
songs <- read.csv("songs.csv", stringsAsFactors = FALSE)

#reading structure of songs
str(songs)

#number of songs in the dataset by year
table(songs$year)

#Top 10 of the Billboard Hot 100 Chart in 2010; bring some old memories? :)
Top2010 <- subset(songs[c("songtitle", "artistname")], songs$year == 2010 & songs$Top10 == 1)

#split data into training and testing set
SongsTrain <- subset(songs, year <= 2009)
SongsTest <- subset(songs, year == 2010)
str(SongsTrain)

#alternative way to avoiding unnecessary variable from the model
nonvars <- c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain <- SongsTrain[ , !(names(SongsTrain) %in% nonvars)]
SongsTest <- SongsTest[ , !(names(SongsTest) %in% nonvars)]

#building the logistic regression model
Model1 <- glm(Top10 ~., data=SongsTrain, family = binomial)

#Result: "loudness" and "energy" are similar variables, but their coefficients are producing opposite impact.
#However, they are both significant.

#checking correlation between "loudness" and "energy"
cor(SongsTrain$loudness, SongsTrain$energy)

#Result "loudness" and "energy" seem highly correlated (r = 0.74), causing multicollinearity

#checking summaries of two models; first without loudness; second without energy
Model2 <- glm(Top10 ~.-loudness, data=SongsTrain, family = binomial)
Model3 <- glm(Top10 ~.-energy, data=SongsTrain, family = binomial)

#checking predictions of Model3 on the training set itself
predictTrain = predict(Model3, type="response")
summary(predictTrain)

#Load ROCR library to plot ROC
library(ROCR)

# Prediction function
ROCRpred <- prediction(predictTrain, SongsTrain$Top10)

# Performance function
ROCRperf <- performance(ROCRpred, "tpr", "fpr")

# Plotting the ROC curve
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

#Result: Based on the ROC curve plot, a threshold of 0.3 looks appropriate
#since at t = 0.3, sensitivity = ~0.45 while false positive rate < 1

#how many songs does Model 3 correctly predict as top 10 hits in 2010
testPredict <- predict(Model3, newdata=SongsTest, type="response")
table(SongsTest$Top10, testPredict >= 0.3)

#Result:
#sensitivity (true positive rate) = 29/(40+29) = 0.49
#specificity (true negative rate) = 288 / (288+26) = 0.92 
#Model3 accurately identifies about half of the songs that make it to Top 10 in the billboard
#Model3 also correctly identifies about 90% of songs that did not make it to the Top 10

