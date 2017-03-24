library(tm)
library(wordcloud)
library(RColorBrewer)
library(dplyr)

setwd("~/Documents/Machine Learning/ML_project")

## Stage 1 ##

# Read in the training and test (cleaned) documents

train_df <- read.csv("full_train_sentiment.csv" , stringsAsFactors = F)
train_df$sentiment <- factor(train_df$sentiment)

test_df <- read.csv("sentiment_test.csv" , stringsAsFactors = F)
test_df$sentiment <- factor(test_df$sentiment)

# Create a corpus of training documents in order to carry out text processing

train_corpus <- Corpus(VectorSource(train_df$review))

train_corpus <- tm_map(train_corpus , tolower)
train_corpus <- tm_map(train_corpus , removeNumbers)
train_corpus <- tm_map(train_corpus , removePunctuation)
train_corpus <- tm_map(train_corpus , removeWords , stopwords(kind = "english"))
train_corpus <- tm_map(train_corpus , removeWords , c("one" , "two" , "movie" , "movies", "film" , "films" , "get" , "make"))
train_corpus <- tm_map(train_corpus , stripWhitespace)
train_corpus <- tm_map(train_corpus , stemDocument , language = "english")

# Create a document-term matrix
# Notice how large the dimensions are (2001 rows of 30550 attributes)

train_dtm <- DocumentTermMatrix(train_corpus)
dim(train_dtm)

# Remove sparse terms to reduce this high dimensionality
# Dimensions reduced to 2001 rows of 195 attributes

train_dtm <- removeSparseTerms(train_dtm , 0.80)
dim(train_dtm)

# Which words occur most frequently in the training documents?

freq_word <- colSums(as.matrix(train_dtm))

# Wordcloud of most frequent words (found at least 1000 times)

wordcloud(names(freq_word) , freq_word , min.freq = 1000 , colors = brewer.pal(7 , "Dark2") , scale = c(2 , 1))

# The document term matrix can be coerced to a dataframe and we can attach the sentiment labels

full_freq_df <- data.frame(as.matrix(train_dtm))
full_freq_df <- cbind(full_freq_df , sentiment = train_df$sentiment)

# We need apriori probabilities for each sentiment level

apriori_pos <- nrow(train_df %>%
                      filter(sentiment == "positive")) / nrow(train_df)

apriori_neg <- nrow(train_df %>%
                      filter(sentiment == "negative")) / nrow(train_df)

# Create bag of words for training

vocabulary <- unique(names(freq_word))
n_vocabulary <- length(vocabulary)

# Split the full_freq_df data by sentiment class 
# Each dataframe contains all words of the vocabulary as attributes

freq_df_pos <- full_freq_df %>%
  filter(sentiment == "positive") %>%
  select(1:195) 

freq_df_pos <- rbind(freq_df_pos , count = colSums(freq_df_pos))

freq_df_neg <- full_freq_df %>%
  filter(sentiment == "negative") %>%
  select(1:195) 

freq_df_neg <-  rbind(freq_df_neg , count = colSums(freq_df_neg))

# Calculate the count of all words in all documents of each class (including duplicates) 

total_count_pos <- rowSums(freq_df_pos[1001 , ])
total_count_neg <- rowSums(freq_df_neg[1002 , ])

# Calculate conditional probabilities for each term for both classes of sentiment
# Incorporate Laplace smoothing of +1

condprobs_neg <- sapply(freq_df_neg[1002 , ] , function(x) { (x + 1) / (total_count_neg + n_vocabulary) })
condprobs_pos <- sapply(freq_df_pos[1001 , ] , function(x) { (x + 1) / (total_count_pos + n_vocabulary) })

names(condprobs_neg) <- vocabulary
names(condprobs_pos) <- vocabulary

## Stage 2 ##

# Use the model on unseen test documents and predict the sentiment class

# Preprocess the test set documents in the same manner as the training documents
# Make a copy of the test set first

test_df2 <- test_df

test_corpus <- Corpus(VectorSource(test_df$review))
test_corpus <- tm_map(test_corpus , tolower)
test_corpus <- tm_map(test_corpus , removeNumbers)
test_corpus <- tm_map(test_corpus , removeWords , stopwords(kind = "english"))
test_corpus <- tm_map(test_corpus , removeWords , c("one" , "two" , "movie" , "movies", "film" , "films" , "get" , "make"))
test_corpus <- tm_map(test_corpus , removePunctuation)
test_corpus <- tm_map(test_corpus , stripWhitespace)
test_corpus <- tm_map(test_corpus , stemDocument , language = "english")

# Convert corpus back to dataframe (note that the only variable is now the review)

test_df <- data.frame(review = sapply(test_corpus , as.character) , stringsAsFactors = FALSE)

term_list <- lapply(test_df , function(x) { strsplit(x , split = " ") })
term_list <- term_list$review[1:1966]

# Now, we need to match up the terms with their corresponding aposterior probabilities
# for the target class and calculate the class probabilities for both sentiments

# Create vectors to store probabilities of sentiment being positive or negative

pred_pos <- vector("numeric")

for(i in term_list) { 
  inserts <- sapply(i, function(n) { n <- condprobs_pos[n] } ) 
  pred <- sum(log(inserts) , na.rm = T) + apriori_pos
  pred_pos[length(pred_pos)+1] <- pred
}

pred_neg <- vector("numeric")

for(i in term_list) { 
  inserts <- sapply(i, function(n) { n <- condprobs_neg[n] } ) 
  pred <- sum(unlist(log(inserts)) , na.rm = T) + apriori_neg
  pred_neg[length(pred_neg)+1] <- pred
}

preds_df <- data.frame(pred_neg , pred_pos)
names(preds_df) <- c("negative" , "positive")

# Bind the conditional probabilities to the test dataframe and also the winning classes and
# actual sentiment classes

winners <- colnames(preds_df)[max.col(preds_df)]
results_df <- cbind(test_df , preds_df , predicted = winners , actual = test_df2$sentiment)

# Obtain model accuracy

conf_matrix <- table(winners , test_df2$sentiment)
accuracy <- (conf_matrix[1,1] + conf_matrix[2,2]) / nrow(test_df2)

print(paste("Model accuracy: " , round(accuracy , 3) * 100 , "%"))


