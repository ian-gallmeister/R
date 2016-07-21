#Ian Gallmeister
#Statistical Computing + Machine Learning
#25 February 2016

#Data -----------------------------------
#data(Default, package = "ISLR")

#The Logistic Function ------------------

logistic <- function(x) exp(x)/(1 + exp(x))

#Linear Combinations --------------------

linear_combine <- function(data, coefs){
  result <- 0
  for (nm in names(coefs)) {
    if (nm == "intercept") {
      result <- result + coefs[[nm]]
    } else if (nm %in% names(data)) {
      result <- result + coefs[[nm]] * data[[nm]]
    } else {stop()}
  }
  result
}

#Probabilities --------------------------

LL_logistic <- function (data, coefs, outcome){
  lincom <- linear_combine(data, coefs)
  probs <- logistic(lincom)
  likelihood <- ifelse(outcome, probs, 1 - probs)
  sum(log(likelihood))
}

#LL_logistic(data=Default, 
#            coefs = c(intercept = 1, income = -0.0001), 
#            outcome = Default$default == "Yes")

#Optimize -------------------------------

starting_params <- c(intercept = 1, income = -0.0001)
best_coefs <- optim(starting_params, LL_logistic, data = Default, outcome = Default$default =="Yes", control = list(fnscale = -1))

compare <- glm(default == "Yes" ~ income, data=Default, family = "binomial")
compare

#Scoring --------------------------------

scoreActivity::score253(day = 10)
