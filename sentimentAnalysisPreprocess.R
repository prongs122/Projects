# IMDB Sentiment Analysis - Data Preprocessing

# The text documents which will be used for training a naive Bayes classifier and for testing its
# accuracy require some preprocessing in order to get them into a suitable data structure for analysis.
# This is carried out below.

# Read in negative reviews for training and create a training set dataframe containing just negative reviews

setwd("/home/sean/Documents/Machine Learning/IMDB/neg")
path <- getwd()

file_list_neg <- list.files(path = path, pattern = "*.txt")

neg_list <- lapply(file_list_neg , function(f) { read.csv(f , header = F) })
neg_list <- lapply(neg_list , function(f) { paste(unlist(f) , collapse = " ") })

review_neg <- unlist(neg_list)
sentiment_neg <- rep("negative" , length(review_neg))

neg_df <- data.frame(review_neg , sentiment_neg)
neg_df$review_neg <- as.character(neg_df$review_neg)
names(neg_df) <- c("review" , "sentiment")

# Do the same for positive reviews

setwd("/home/sean/Documents/Machine Learning/IMDB/pos")
path <- getwd()

file_list_pos <- list.files(path = path, pattern = "*.txt")

pos_list <- lapply(file_list_pos , function(f) { read.csv(f , header = F) })
pos_list <- lapply(pos_list , function(f) { paste(unlist(f) , collapse = " ")})

review_pos <- unlist(pos_list)
sentiment_pos <- rep("positive" , length(review_pos))

pos_df <- data.frame(review_pos , sentiment_pos)
pos_df$review_pos <- as.character(pos_df$review_pos)
names(pos_df) <- c("review" , "sentiment")

# Create full train set.

full_train_df <- rbind(neg_df , pos_df)

# We need to do the same for the test data but compile both sentiment classes in one dataframe.
# Prepare test data for input to classifier (dataset must contain instances from both classes of sentiment).

# Negative sentiment

setwd("/home/sean/Documents/Machine Learning/smallTest/neg")
path <- getwd()

file_list_test_neg <- list.files(path = path, pattern = "*.txt")

test_neg_list <- lapply(file_list_test_neg , function(f) { read.csv(f , header = F) })
test_neg_list <- lapply(test_neg_list , function(f) { paste(unlist(f) , collapse = " ") })

review_test_neg <- unlist(test_neg_list)
sentiment_test_neg <- rep("negative" , length(review_test_neg))

test_neg_df <- data.frame(review_test_neg , sentiment_test_neg)
test_neg_df$review_test_neg <- as.character(test_neg_df$review_test_neg)
names(test_neg_df) <- c("review" , "sentiment")

# The test data contains many erroneous entries which are discarded here.

errors_test_neg <- c(41, 157, 217, 319, 369, 445, 448, 511, 640, 647, 668, 669, 682, 734, 832, 895, 900, 954) 
test_neg_df <- test_neg_df[-errors_test_neg , ]

# Positive sentiment

setwd("/home/sean/Documents/Machine Learning/smallTest/pos")
path <- getwd()

file_list_test_pos <- list.files(path = path, pattern = "*.txt")

test_pos_list <- lapply(file_list_test_pos , function(f) { read.csv(f , header = F) })
test_pos_list <- lapply(test_pos_list , function(f) { paste(unlist(f) , collapse = " ") })

review_test_pos <- unlist(test_pos_list)
sentiment_test_pos <- rep("positive" , length(review_test_pos))

test_pos_df <- data.frame(review_test_pos , sentiment_test_pos)
test_pos_df$review_test_pos <- as.character(test_pos_df$review_test_pos)
names(test_pos_df) <- c("review" , "sentiment")

# Again, many entries are erroneous and can be discarded

errors_test_pos <- c(87, 132, 218, 276, 513, 521, 526, 535, 538, 610, 657, 788, 851, 950, 975, 1000)

test_pos_df <- test_pos_df[-errors_test_pos , ]

# Create a full test set for the analysis. There are many erroneous entries in the test data (33 in total)
# and we should remove these. This has been done in Libre Office. The cleaned datasets are in the 
# Machine Learning working directory.

full_test_df <- rbind(test_neg_df , test_pos_df)

# Write the prepared dataframes to disk for use in project

setwd("/home/sean/Documents/Machine Learning/ML_project")

write.csv(file = "sentiment_test.csv", full_test_df)
write.csv(file = "full_train_sentiment.csv" , full_train_df)



