# Use a gradient descent algorithm to minimise the MSE value for a linear regression model
# Linear hypothesis: y = mx + c
# MSE = (sum((y - yhat) ^ 2)) / n
# Note that the cost function is the same as the mean squared error

d1 <- read.csv("DataSet1.csv", header = F)
d2 <- read.csv("DataSet2.csv", header = F)
names(d1) <- c("X", "Y")
names(d2) <- c("X", "Y")

# The gradient descent function which should return optimal coefficients

gradientDescent <- function(x, y, learn_rate, conv_threshold, n, max_iter) {
  plot(x, y, col = "blue", pch = 20)
  m <- runif(1, 0, 1)
  c <- runif(1, 0, 1)
  yhat <- m * x + c
  MSE <- sum((y - yhat) ^ 2) / n
  converged = F
  iterations = 0
  while(converged == F) {
    m_new <- m - learn_rate * ((1 / n) * (sum((yhat - y) * x)))
    c_new <- c - learn_rate * ((1 / n) * (sum(yhat - y)))
    m <- m_new
    c <- c_new
    yhat <- m * x + c
    MSE_new <- sum((y - yhat) ^ 2) / n
    if(MSE - MSE_new <= conv_threshold) {
      converged = T
    }
    iterations = iterations + 1
    if(iterations > max_iter) { 
      abline( c, m) 
      converged = T
      return(paste("Optimal intercept:", c, "Optimal slope:", m))
    }
  }
} 


# R has a built-in function for building a linear regression model: stat_smooth()

library(ggplot2)

ggplot(d2, aes(x = X, y = Y)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x, fill = NA)

co_efficients <- coefficients(lm(d1$Y ~ d1$X))





