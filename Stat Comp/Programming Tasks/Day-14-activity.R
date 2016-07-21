#Ian Gallmeister
#Statistical Computing + Machine Learning
#24 March 2016

#Comparing OLS to ridge -----------------

#library(glmnet)
GenCont <- read.csv("http://tiny.cc/dcf/GenCont.csv")

compare_ols_ridge <- function(response, predictors, lambda=1){
  
  training_indices <- sample(1:length(response), 0.5*length(response))
  testing_indices <- setdiff(1:length(response), training_indices)
  
  training_responses <- data.frame(response[training_indices])
  class(training_responses)
  training_predictors <- predictors[training_indices,]
  
  test_responses <- data.frame(response[testing_indices])
  class(test_responses)
  test_predictors <- predictors[testing_indices,]
  
  lin_mod <- lm(training_responses ~ training_predictors)
  ridge_regression <- glmnet(training_predictors, training_responses, alpha = 0, lambda = lambda)
  
  in_sample_lm <- predict(training_responses, lin_mod)
  in_sample_rr <- predict(training_responses, ridge_regression)
  
  out_sample_lm <- predict(testing_responses, lin_mod)
  out_sample_rr <- predict(testing_responses, ridge_regression)
  
}

compare_ols_ridge(response = GenCont[,1], predictors = GenCont[,2:ncol(GenCont)])

#Scoring --------------------------------

scoreActivity::score253(day = 14)