
#install.packages("tidytext")
#install.packages("janeaustenr")
#install.packages("dplyr")
#install.packages("stringr")
#install.packages("tidyverse")
#install.packages("kernlab")

library(tidytext)
library(janeaustenr)
library(dplyr)
library(stringr)
library(tidyverse)
library(textcat)
library(class)
library(kernlab)
library(C50)
library(caret)
library(rpart)
library(randomForest)
library(e1071)

######################################################################################
#################################### Sentiment Analysis ##############################
######################################################################################

######################################### Read consolidated data #############################################################

users_reviews <- read.csv("rev_user_bus.csv", stringsAsFactors = FALSE)

#################################################### Calling sentiment libraries ############################################

nrc <-get_sentiments("nrc")

##################################################### Taking only relevant columns ##########################################

users_reviews <- users_reviews[,c(2,3,6,9)]

##################################################### Putting each word in review in a separate row ###########################

tidy_users_reviews <- users_reviews %>% unnest_tokens(word, text)

################################################ Final sentiment output ###############################################

users_nrcSent <- tidy_users_reviews %>%
  inner_join(nrc) %>%
  count(index = review_id, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

users_nrcSent <- merge(x = users_nrcSent, y = users_reviews, by.y = "review_id", by.x = "index")

business_reviews <- aggregate(users_nrcSent[, c(-1,-13,-14)], list(users_nrcSent$business_id), mean)

colnames(business_reviews)[1] <- "business_id"

write.csv(business_reviews, "business_reviews.csv", row.names = FALSE)
remove('business_reviews', 'users_nrcSent', 'users_nrcSent','tidy_users_reviews','users_reviews', 'nrc')

######################################################################################
#################################### Pre-processing Data #############################
######################################################################################
#Read and Clean File
business_sentiments <- read.csv("business_reviews.csv", stringsAsFactors = FALSE)

# CATEGORIZING CLASSES 
business_sentiments$business_stars[business_sentiments$business_stars < 3] <- "1"
business_sentiments$business_stars[business_sentiments$business_stars >= 3 & business_sentiments$business_stars < 4.5] <- "2"
business_sentiments$business_stars[business_sentiments$business_stars >= 4.5] <- "3"

business_sentiments$business_stars <- factor(business_sentiments$business_stars)


# REMOVING BUSINESS_ID
business_sentiments <- business_sentiments[,-1]

#Normlizing data
business_sentiments[, -12] <- scale(business_sentiments[, -12])

# Exploratory Analysis
barplot(table(business_sentiments$business_stars))

boxplot(business_sentiments[,-12])

set.seed(1234)

################################## Undersampling ##############################################
# SEPARATING THE CLASSES AND SHUFFLING THEIR OBSERVATIONS  
rating1 <- business_sentiments[which(business_sentiments$business_stars==1),]
rating2 <- business_sentiments[which(business_sentiments$business_stars==2),]
rating3 <- business_sentiments[which(business_sentiments$business_stars==3),]

# SIZE OF CLASS WITH LESS OBSERVATIONS
classLengh <- min(length(rating1$business_stars), length(rating2$business_stars), length(rating3$business_stars))

# SELECTING SAME NUMBER OF OBSERVATIONS FROM OTHER CLASSES 
rating1 <- rating1[1:classLengh,]
rating2 <- rating2[1:classLengh,]
rating3 <- rating3[1:classLengh,]

balanced_data <- rbind(rbind(rating1,rating2), rating3)
balanced_data <- balanced_data[sample(nrow(balanced_data)),]

############################### SPLIT DATA IN TRAINING AND TEST #################################

index <- sample(1:nrow(balanced_data), nrow(balanced_data)*0.75, replace = F)

training <- balanced_data[index, ]
testing <- balanced_data[-index, ]

# Cleaning Environment
remove('index', 'balanced_data', 'rating1','rating2','rating3', 'business_sentiments', 'classLengh', 'trainingLengh')

######################################################################################
########################### FINDING THE BEST K  ######################################
############ Evaluate the change in accuracy with increasing values of k #############
######################################################################################

knntestPrediction <- list()
accuracy <- numeric()

for(k in 1:200)
{
  knntestPrediction[[k]] <- knn(training[,-12], testing[,-12], training$business_stars, k, prob=TRUE)
  accuracy[k] <- sum(knntestPrediction[[k]]==testing$business_stars)/length(testing$business_stars)*100
}

plot(accuracy, type="b", col="blue", cex=1, pch=20, 
     xlab="Number of neighbours (k)", ylab="Classification Accuracy (%)", 
     main="Accuracy vs k")


# Add line for max accuracy seen
abline(h=max(accuracy), col="grey", lty=2)
paste("Maximum accuracy is", max(accuracy), "% at k = ", which(accuracy==max(accuracy)))


# Add lines indicating k with best accuracy
abline(v=which(accuracy==max(accuracy)), col="darkorange", lwd=1.5)

# "Maximum accuracy is 59.963768115942 % at k =  95"
# "Maximum accuracy is 60.024154589372 % at k =  33"

remove('knntestPrediction', 'accuracy', 'k')


######################################################################################
#################################### KNN #############################################
######################################################################################

#KNN PREDICTION
knnPrediction <- knn(training[,-12], testing[,-12], training$business_stars, k=33, prob=T)

confusionMatrix(testing$business_stars, knnPrediction, positive = "Yes")
# Accuracy : 0.6021 
confusionMatrix(data = knnPrediction, reference = testing$business_stars, mode = "prec_recall")

remove('knnPrediction')

######################################################################################
########################### K-FOLD CROSS VALIDATION (KFCV) ###########################
######################################################################################

# Initializing variable for list of K predictions results
accuracy <- numeric()

#Joining the training and test data set from knn prediction
dataset <- rbind(testing, training)

#Size of the dataset
sizedf <- length(dataset$business_stars)

#Number of folds or windows of the KFCV 
n_folds <- 20

# Positions of starting fold and ending folds
# Initially starts at position 1 and ends at the size of fold
start_fold <- 1
end_fold <- size_fold <- sizedf/n_folds


for (k in 1:n_folds) {
  # Vector with the positions of elements from the fold/window
  kfold <- c(start_fold:end_fold)
  
  # Sliting the test and training data.
  # The elements from the kfold are separated as a test and the rest as a training set
  testKFCV <- dataset[kfold,]
  trainingKFCV <- dataset[-kfold,]
  
  # KNN 
  ktestPrediction <- knn(trainingKFCV[,-12], testKFCV[,-12], trainingKFCV$business_stars, 95, prob=TRUE)
  accuracy[k] <- sum(ktestPrediction==testKFCV$business_stars)/length(testKFCV$business_stars)*100
  
  # Updating the start fold and end fold variables 
  start_fold <- start_fold + size_fold
  end_fold <- end_fold + size_fold
}

paste("Avg. Accuracy On", n_folds,"WindoWs Is:", mean(accuracy))

# "Avg. Accuracy On 20 WindoWs Is: 59.6827794561934"

remove('dataset', 'accuracy', 'sizedf', 'n_folds', 'start_fold', 'end_fold', 
       'size_fold', 'kfold', 'testKFCV', 'trainingKFCV', 'ktestPrediction', 'k')

######################################################################################
#################################### SVM #############################################
######################################################################################


#we're probably going to build lots of models, so let's make a function to save time
svmPerformance <- function(svm, testing, trueValues) {
  p <- predict(svm, newdata=testing, type = "response")
  accuracy <- 1-mean(p != trueValues)
  return(accuracy)
}

svm.model <- ksvm(business_stars ~ ., data = training, kernel = "anovadot")
svmPerformance(svm.model, testing, testing$business_stars)
# 0.6074879

svmPrediction <- predict(svm.model , newdata=testing, type = "response")

confusionMatrix(testing$business_stars, svmPrediction, positive = "Yes")
# Accuracy : 0.6075
confusionMatrix(data = svmPrediction, reference = testing$business_stars, mode = "prec_recall")

remove('svm.model', 'svmPerformance','svmPrediction')


######################################################################################
###################################### C50 ###########################################
######################################################################################

# Train
cFifty <- C5.0(business_stars~., data = training)
#summary(cFifty)

cFiftyPredict <- predict(cFifty, newdata = testing[, -12])

confusionMatrix(testing$business_stars, cFiftyPredict, positive = "Yes")
# Accuracy : 0.5537
confusionMatrix(data = cFiftyPredict, reference = testing$business_stars, mode = "prec_recall")



remove('cFifty', 'cFiftyPredict')

######################################################################################
#################################### REGRESSION TREE #################################
######################################################################################

regressionTree <- rpart(business_stars~., data = training, method = "class")

# Plot both trees
#plot(regressionTree)
#text(regressionTree)

# PREDICTION
rpartPrediction <- predict(regressionTree, testing, type = "class")

# 2. confusion matrix
confusionMatrix(rpartPrediction, testing$business_stars, positive = "Yes")
# Accuracy : 0.5634



newRpart <- rpart(business_stars~., data = training,
                  control = rpart.control(minsplit = 2, cp = 0))

#fancyRpartPlot(newRpart)

# Huge tree has overfit the data
# evaluate performance of Huge tree
rpartPrediction <- predict(newRpart, testing, type = "class")

confusionMatrix(rpartPrediction, testing$business_stars, positive = "Yes")
#  Accuracy : 0.5187

remove('regressionTree', 'newRpart', 'rpartPrediction')
######################################################################################
#################################### RANDOM FORESTS ##################################
######################################################################################
forest <- randomForest(business_stars~., data = training,
                       importance = TRUE, ntree = 2000)

varImpPlot(forest)
forestPrediction <- predict(forest, testing, type = "class")
confusionMatrix(forestPrediction, testing$business_stars, positive = "Yes")
# Accuracy : 0.6105

remove('forest', 'forestPrediction')

######################################################################################
#################################### Naive Bayes #####################################
######################################################################################

# train NB model
nb <- naiveBayes(business_stars~., data = training)

# predict using trained NB model
nbPredict <- predict(nb, newdata = testing[ , -12])

# look at confusionMatrix
confusionMatrix(testing$business_stars, nbPredict, positive = "Yes")
# Accuracy : 0.4982 

remove('nb', 'nbPredict')
