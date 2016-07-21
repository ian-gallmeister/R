#Ian Gallmeister
#Statistical Computing + Machine Learning
#10 March 2016
#K-fold Cross Validation

#First Function -------------------------

k_fold1 <- function(formula, method = lm, data = mtcars, predfun = predict, k = 10){
  #k <- 10
  sets <- (1:nrow(data) %% k) + 1
  #sets <- rep(1:10, each = 51/k, length.out = 51) #same result, different order of values
  mspe <- numeric(k) #Mean Square Prediction Error, numeric(k) returns vector of k zeroes
  for(i in 1:k){
    For_Testing <- data[sets == i, ]
    For_Training <- data[!(sets == i),]
    mod <- lm(mpg ~ hp + wt + am, data = For_Training)
    pred_vals <- predict(mod, newdata = For_Testing)
    mspe[k] <- mean((For_Testing[["mpg"]] - pred_vals)^2)
  }
  error_estimate <- sum(mspe)/k
  return(error_estimate)
}

#Second Function ------------------------

k_fold <- function(formula, method, data, predfun, k){
  #k <- 10
  sets <- (1:nrow(data) %% k) + 1
  #sets <- rep(1:10, each = 51/k, length.out = 51) #same result, different order of values
  mspe <- numeric(k) #Mean Square Prediction Error, numeric(k) returns vector of k zeroes
  for(i in 1:k){
    For_Testing <- data[sets == i, ]
    For_Training <- data[!(sets == i),]
    mod <- method(formula, data = For_Training)
    pred_vals <- predfun(mod, newdata = For_Testing)
    mspe[k] <- mean((For_Testing[[as.character(formula[[2]])]] - pred_vals)^2)
  }
  error_estimate <- sum(mspe)/k
  return(error_estimate)
}

#Scoring --------------------------------

scoreActivity::score253(day = 13)
