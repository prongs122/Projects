library(tm)
library(wordcloud)
library(RColorBrewer)
library(dplyr)

# Read in large IMDB positive review data

path <- "/home/sean/Documents/Machine Learning/LargeIMDB/pos"
setwd(path)

file_pos <- list.files(path = path , pattern = "*.txt")

pos_list <- lapply(file_pos , function(f) { read.csv(f , header = F) })
pos_list <- lapply(pos_list , function(f) { paste(unlist(f) , collapse = " ") })
sentiment_pos <- rep("positive" , length(pos_list))
review_pos <- unlist(pos_list)

pos_full_df <- data.frame(review = review_pos , sentiment = sentiment_pos)
pos_full_df$review <- as.character(pos_full_df$review)


# Read in large IMDB negative review data

path <- "/home/sean/Documents/Machine Learning/LargeIMDB/neg"
setwd(path)

file_neg <- list.files(path = path , pattern = "*.txt")

neg_list <- lapply(file_neg , function(f) { read.csv(f , header = F) })
neg_list <- lapply(neg_list , function(f) { paste(unlist(f) , collapse = " ") })
sentiment_neg <- rep("negative" , length(neg_list))
review_neg <- unlist(neg_list)

neg_full_df <- data.frame(review = review_neg , sentiment = sentiment_neg)
neg_full_df$review <- as.character(neg_full_df$review)


# Combine all data into one dataframe

IMDB_large <- rbind(neg_full_df , pos_full_df)

setwd("~/Documents/Machine Learning/ML_project")

write.csv(file = "IMDB_full.csv" , IMDB_large)

# Build the multinomial naive Bayes model and predict test document class

IMDB_full <- read.csv("IMDB_full.csv")
IMDB_full$review <- as.character(IMDB_full$review)

# Create a corpus of training documents in order to carry out text processing
# A corpus is a collection of documents

review_corpus <- Corpus(VectorSource(IMDB_full$review))

review_corpus <- tm_map(review_corpus , tolower)
review_corpus <- tm_map(review_corpus , removeNumbers)
review_corpus <- tm_map(review_corpus , removePunctuation)
review_corpus <- tm_map(review_corpus , removeWords , stopwords(kind = "english"))
review_corpus <- tm_map(review_corpus , removeWords , c("one" , "two" , "movie" , "movies", "film" , "films" , "get" , "make"))
review_corpus <- tm_map(review_corpus , stripWhitespace)
review_corpus <- tm_map(review_corpus , stemDocument , language = "english")

# The documents are much cleaner after this text preprocessing
# Inspect the first document in the corpus

inspect(review_corpus[1])

# Create a document-term matrix which gives a word frequency counts for each document
# Notice how large the dimensions are (25000 rows of 84939 attributes)

IMDB_dtm <- DocumentTermMatrix(review_corpus)
dim(IMDB_dtm)

# Remove sparse terms to reduce this high dimensionality 
# Dimensions reduced to 25000 documents now with 137 terms

IMDB_dtm <- removeSparseTerms(IMDB_dtm , 0.90)
dim(IMDB_dtm , 0.90)

# Which words occur most frequently in the training documents?
# The vocabulary can be obtained from the document-term matrix

freq_word <- colSums(as.matrix(IMDB_dtm))

# Wordcloud of most frequent words (found at least 4000 times in the training documents)

wordcloud(names(freq_word) , freq_word , min.freq = 4000 , colors = brewer.pal(7 , "Dark2") , scale = c(1 , 1))

# The document term matrix can be coerced to a dataframe and we can attach the sentiment labels

IMDB_dtm_df <- data.frame(as.matrix(IMDB_dtm) , sentiment = IMDB_full$sentiment)

# We need apriori probabilities for each sentiment level

apriori_pos <- nrow(IMDB_full %>%
                      filter(sentiment == "positive")) / nrow(IMDB_full)

apriori_neg <- nrow(IMDB_full %>%
                      filter(sentiment == "negative")) / nrow(IMDB_full)

# Create bag of words (vocabulary) for training

vocabulary <- unique(names(freq_word))
n_vocabulary <- length(vocabulary)
wordcloud(vocabulary , scale = c(1 , 1))



# Split the data by sentiment class
# Each dataframe contains all words of the vocabulary as attributes

IMDB_pos_dtm <- IMDB_dtm_df %>%
  filter(sentiment == "positive") %>%
  select(1:137) 

IMDB_pos_dtm <- rbind(IMDB_pos_dtm , count = colSums(IMDB_pos_dtm))

IMDB_neg_dtm <- IMDB_dtm_df %>%
  filter(sentiment == "negative") %>%
  select(1:137) 

IMDB_neg_dtm <-  rbind(IMDB_neg_dtm , count = colSums(IMDB_neg_dtm))

# Calculate the count of all words in all documents of each class (including duplicates) 

total_count_pos <- rowSums(IMDB_pos_dtm[12501 , ])
total_count_neg <- rowSums(IMDB_neg_dtm[12501 , ])

# Calculate conditional probabilities for each term for both classes of sentiment
# Incorporate Laplace smoothing of +1

condprobs_neg <- sapply(IMDB_neg_dtm[12501 , ] , function(x) { (x + 1) / (total_count_neg + n_vocabulary) })
condprobs_pos <- sapply(IMDB_pos_dtm[12501 , ] , function(x) { (x + 1) / (total_count_pos + n_vocabulary) })
names(condprobs_neg) <- vocabulary
names(condprobs_pos) <- vocabulary

## Stage 2 ##

# Use the model on unseen test documents and predict the sentiment class

test_df <- read.csv("sentiment_test.csv")
test_df$review <- as.character(test_df$review)

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

# Split the strings in each review to make vectors of individual words

term_list <- lapply(test_df , function(x) { strsplit(x , split = " ") })
term_list <- term_list$review[1:1966]

# Now, we need to match up the terms with their corresponding aposterior probabilities
# for the target class and calculate the class probabilities for both sentiments

# Create vectors to store probabilities of sentiment being positive or negative 
# Implement the multinomial naive Bayes algorithm

pred_pos <- vector("numeric")

for(i in term_list) { 
  inserts <- sapply(i , function(n) { n <- condprobs_pos[n] } ) 
  pred <- sum(log(inserts) , na.rm = T) + log(apriori_pos)
  pred_pos[length(pred_pos)+1] <- pred
}

pred_neg <- vector("numeric")

for(i in term_list) { 
  inserts <- sapply(i , function(n) { n <- condprobs_neg[n] } ) 
  pred <- sum(unlist(log(inserts)) , na.rm = T) + log(apriori_neg)
  pred_neg[length(pred_neg)+1] <- pred
}

preds_df <- data.frame(negative = pred_neg , positive = pred_pos)

# Bind the conditional probabilities to the test dataframe and also the winning classes and
# actual sentiment classes

winners <- colnames(preds_df)[max.col(preds_df)]
results_df <- cbind(test_df , preds_df , predicted = winners , actual = test_df2$sentiment)

# Obtain model accuracy

conf_matrix <- table(winners , results_df$actual)
accuracy <- (conf_matrix[1,1] + conf_matrix[2,2]) / nrow(preds_df)

print(paste("Model accuracy: " , round(accuracy , 3) * 100 , "%"))


