# Two functions have been written in order to implement the naive Bayes algorithm and predict the class for 
# test data. Please note that the argument "target" in the modelNaiveBayes function requires the class outcome
# name to be entered as a string. For the breast cancer dataset used in the assignment, target is set to "class".

# This script contains three functions. The first, modelNaiveBayes, was written to create conditional 
# probability look up tables. The second, predictNaiveBayes, was written in order to make predictions on 
# test data and return a vector of winning class outcomes. The third function, main, carries out the task of
# creating a naive Bayes model, predicting class of test data and visualising the differences between actual
# and predicted classes.

###!!!! Run entire script !!!!###

# The modelNaiveBayes function takes three arguments:
#1 target (a string of the target class) 
#2 data (training dataframe)
#3 laplace (an integer value for Laplace smoothing)


modelNaiveBayes <- function(target, data, laplace) {
  # Set some values for use later and split training data into target class and attribute subsets
  n <- nrow(data)
  y <- data[  , target]
  classes <- levels(y)
  k <- length(classes)
  att.names <- setdiff(colnames(data), target)
  atts <- data[ , att.names]
  
  # Create result objects to be filled
  priors <- setNames(numeric(k), classes)
  probs <- vector("list", k)
  
  # Loop through the possible class outcomes
  for(cl in classes) {
    # Subset by outcome
    atts2 <- atts[y == cl, ]
    # Get apriori probabilities for each class
    priors[cl] <- nrow(atts2) / n
    # Incorporate Laplace smoothing and get attribute proportions where x is each attribute
    probs[[cl]] <- lapply(atts2, function(x) {
      p <- table(x)
      p <- p + laplace
      p <- prop.table(p)
      p <- setNames(as.numeric(p), names(p))
    })
  }
  # Model components
  list(target = target, classes = classes, priors = priors, probs = probs)
}


# The predictNaiveBayes function takes two arguments:
#1 model (a naive Bayes model created with the modelNaiveBayes function)
#2 newdata (test data to make predictions on, can be unlabelled or labelled)

predictNaiveBayes <- function(model, newdata) {
  n <- nrow(newdata)
  # If test data is labelled, we can remove the label
  newdata[ , model$target] = NULL
  class <- model$classes
  # Result object to be filled
  pred <- matrix(NA, nrow = n, ncol = length(class))
  colnames(pred) <- class
  # Loop through new test data
  for(i in 1:nrow(newdata)) {
    # Set attribute label vectors as character vectors to get patterns
    x <- sapply(newdata[i , ], as.character)
    # Look up conditional probabilities for each class
    for(cl in class) {
      tabs <- model$probs[[cl]]
      # Gather conditional probabilities for attribute label and insert
      inserts <- sapply(names(x), function(n) { tabs[[n]][x[n]] } )
      # Implement the naive Bayes algorithm by getting the product of apriori and conditional probabilities
      pred[i, cl] <- prod(inserts, na.rm = T) * model$priors[[cl]]
    }
  }
  print("Conditional probabilities of class outcomes. Highest value identifies predicted class outcome.")
  print(pred)
  # Get winning class and assign to global environment
  winners <<- colnames(pred)[max.col(pred)]
}


# The main function takes no arguments and needs to be run at the end of the script. Three libraries must be
# loaded: e1071, reshape2 and ggplot2. The dataset used in the function has been attached with the assignment 
# and is a pre-processed version of the original dataset.

library(e1071)
library(reshape2)
library(ggplot2)

main <- function() {
  # Read in cleaned dataset provided with assignment submission (or read in alternative and assign to df)
  df <- read.csv("cleaned_breast_df.csv")
  
  # Create naive Bayes model
  model_nb <- modelNaiveBayes("class", df, 1)
  
  # Predict class for i in training data
  predictions <- predictNaiveBayes(model_nb, df)
  
  # We can assess model accuracy by creating a confusion matrix of predicted class versus actual class
  conf_matrix <- table(winners, df[ , model_nb$target])
  print(conf_matrix)
  
  accuracy <- (conf_matrix[1 , 1] + conf_matrix[2 , 2]) / nrow(df)
  accuracy <- round(accuracy * 100, 1)
  print(paste("Model accuracy is:", accuracy, "%"))
  
  # Arrange data for a plot
  
  outcomes <- model_nb$classes
  Predicted <- rowSums(conf_matrix)
  Actual <- colSums(conf_matrix)
  
  df2 <- data.frame(outcomes, Predicted, Actual)
  df3 <- melt(df2)
  
  # Build plot
  
  ploT <<-  ggplot(df3, aes(x = outcomes, y = value, fill = variable)) +
              geom_bar(stat = "identity", colour = "black", alpha = 0.5, position = "dodge") +
              ylab("No. of observations") +
              xlab("Class") +
              ggtitle("Model Predictions v Actual Classes") +
              theme(plot.title = element_text(size = 13, face = "bold")) +
              scale_fill_hue(name = " ")
  
  print(ploT)
            
  # Compare model_nb accuracy with e1071 results
  
  model_e1071 <- naiveBayes(class ~ ., data = df, laplace = 1)
  predictions_e1071 <- predict(model_e1071, df)
  
  conf_matrixe1071 <- table(predictions_e1071, df$class)
  accuracy_e1071 <- (conf_matrixe1071[1 , 1] + conf_matrixe1071[2 , 2]) / nrow(df)
  accuracy_e1071 <- round(accuracy_e1071 * 100, 1)
  print(paste("Model accuracy using e1071 is:", accuracy_e1071, "%"))
  
  # Model validation using train/test split
  
  set.seed(777)
  test.i <- sample(nrow(df), 20)
  test <- df[test.i , ]
  train <- df[-test.i , ]
  
  val.model <- modelNaiveBayes("class", train, 1)
  val.preds <- predictNaiveBayes(val.model, test)
  
  t <- table(winners, test$class)
  acc <- (t[1 , 1] + t[2 , 2]) / nrow(test)
  acc <- round(acc * 100, 1)
  print(paste("Model accuracy using split data:", acc, "%"))
  
}

# Run the main program

main()


