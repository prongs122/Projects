# IMDB Movie Review Sentiment Analysis


# Import modules necessary for task
# Note that the "from __future__ import division" statement allows true division to occur when calculating probabilities

from __future__ import division, print_function
import string
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import nltk
import sklearn


# Create a function for converting the vector of sentiment labels to a Boolean factor with two levels
# True represents a positive review while False represents a negative review


def convert_to_boolean(sentiment_string):
    if sentiment_string == "negative":
        return False
    else:
        return True


## Stage 1 ##

# Read in the training and test documents provided with the assignment submission
# Assign the total number of documents in the training set to train_nb_documents

train_df = pd.read_csv("full_train_sentiment.csv")
train_nb_documents = train_df.shape[0]

# The sentiment labels need to be factorised
# This is done by converting the labels to Boolean levels

train_df["sentiment"] = train_df["sentiment"].apply(lambda x:convert_to_boolean(x))

test_df = pd.read_csv("sentiment_test.csv")
test_nb_documents = test_df.shape[0]

test_df["sentiment"] = test_df["sentiment"].apply(lambda x:convert_to_boolean(x))


## Stage 2 ##

# Some text preprocessing is required in order to prepare the documents for model building

# Convert all characters to lower case
train_df["review"] = train_df["review"].apply(lambda x: x.lower())

# Remove numbers
train_df["review"] = train_df["review"].apply(lambda review_string: ''.join([character for character in review_string if character not in "0123456789"]))

# Remove punctuation
train_df["review"] = train_df["review"].apply(lambda review_string: ''.join([character for character in review_string if character not in string.punctuation]))

# Remove stopwords
from nltk.corpus import stopwords
nltk.download("stopwords")

stopwords_english = set(stopwords.words("english"))
train_df["review"] = train_df["review"].apply(lambda review_string: ' '.join([word for word in review_string.split() if word not in stopwords_english]))

# Remove additional words which carry no real sentiment determining power
filtered_words = set(["one" , "two" , "movie" , "movies", "film" , "films" , "get" , "make"])
train_df["review"] = train_df["review"].apply(lambda review_string: ' '.join([word for word in review_string.split() if word not in filtered_words]))

# Carry out word stemming and add stemmed reviews as an additional column in the training dataframe
from nltk.stem.snowball import SnowballStemmer
stemmer = SnowballStemmer("english")

train_df["stemmed"] = train_df["review"].apply(lambda review_string: ' '.join([stemmer.stem(word) for word in review_string.split()]))

# The documents are much cleaner after this text preprocessing
# Inspect the first document after stemming

print(train_df.ix[0])

# # Create a document-term matrix which gives word frequency counts for each document
# # Notice how large the dimensions are (2001 rows of 30754 attributes)
from sklearn.feature_extraction.text import CountVectorizer
countvec = CountVectorizer()

# Build a pandas dataframe containing documents as rows and words as columns
train_dtm = pd.DataFrame(countvec.fit_transform(train_df["stemmed"]).toarray(), columns = countvec.get_feature_names())

# # Remove sparse terms to reduce this high dimensionality
# # Dimensions reduced to 2001 rows and 196 attributes
countvec2 = CountVectorizer(min_df = 0.2)
train_dtm = pd.DataFrame(countvec2.fit_transform(train_df["stemmed"]).toarray(), columns = countvec2.get_feature_names())
print(train_dtm.shape) 

# What are the word frequencies across all training documents?
# The vocabulary (bag of words) is the 196 terms in the freq_word series
freq_word = train_dtm.sum()
freq_word.sort_values(inplace = True, ascending = False)
print(freq_word)

n_vocabulary = len(freq_word)

# Add sentiment labels to the document-term matrix so we can calculate class apriori probabilities
train_dtm["sentiment"] = train_df["sentiment"]

apriori_pos = train_df["sentiment"].value_counts()[True] / train_nb_documents
apriori_neg = train_df["sentiment"].value_counts()[False] / train_nb_documents

# Split the training data by sentiment class
# Each subset contains all words of the vocabulary as attributes
freq_df_pos = train_dtm.loc[train_dtm["sentiment"] == True]
del(freq_df_pos["sentiment"])

freq_df_neg = train_dtm.loc[train_dtm["sentiment"] == False]
del(freq_df_neg["sentiment"])

# Calculate the total count of all words in all documents of each class (including duplicates)
total_count_pos = freq_df_pos.sum().sum()
total_count_neg = freq_df_neg.sum().sum()

freq_df_pos_count = freq_df_pos.sum()
freq_df_neg_count = freq_df_neg.sum()

# Calculate conditional probabilities for each term for both classes of sentiment
# Incorporate Laplace smoothing of +1

condprobs_neg = freq_df_neg_count.apply(lambda x: (x + 1) / (total_count_neg + n_vocabulary))
condprobs_pos = freq_df_pos_count.apply(lambda x: (x + 1) / (total_count_pos + n_vocabulary))


## Stage 3 ##

# Use the model on unseen test documents and predict the sentiment class

# Preprocess the test set documents in the same manner as the training documents

test_df["review"] = test_df["review"].apply(lambda x: x.lower() )
test_df["review"] = test_df["review"].apply(lambda review_string: ''.join([character for character in review_string if character not in "0123456789"]))
test_df["review"] = test_df["review"].apply(lambda review_string: ''.join([character for character in review_string if character not in string.punctuation]))
test_df["review"] = test_df["review"].apply(lambda review_string: ' '.join([word for word in review_string.split() if word not in stopwords_english]))
test_df["review"] = test_df["review"].apply(lambda review_string: ' '.join([word for word in review_string.split() if word not in filtered_words]))
print(test_df)

# Get conditional probabilities for each document in the test set being positively reviewed
pred_pos = test_df["review"].apply(lambda review_string: np.log(apriori_pos) + sum([np.log(condprobs_pos.get(word,1)) for word in review_string.split()]))

# Then calculate conditional probabilities for each document being negatively reviewed
pred_neg = test_df["review"].apply(lambda review_string: np.log(apriori_neg) + sum([ np.log(condprobs_neg.get(word,1)) for word in review_string.split()]))

# Display the results in a dataframe
results_df = pd.DataFrame()
results_df["P(positive)"] = pred_pos
results_df["P(negative)"] = pred_neg

# Add the predicted classes to the dataframe
# Remember that False represents a negative review while True represents a positive review
results_df["prediction"] = results_df["P(positive)"] > results_df["P(negative)"]

# Then add the actual classes of the test documents to the results dataframe
results_df["actual"] = test_df["sentiment"]

# Finally we calculate a model accuracy
from sklearn.metrics import confusion_matrix
conf_matrix = confusion_matrix(results_df["actual"], results_df["prediction"])
accuracy = (conf_matrix[0,0] + conf_matrix[1,1]) / test_nb_documents

print("Model accuracy:", round(accuracy , 3) * 100 , "%")

