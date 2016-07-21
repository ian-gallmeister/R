#Ian Gallmeister
#Statistical Computing + Machine Learning
#16 February 2016

#Task 1 ---------------------------------

vals <- rexp(10, rate = 1/100)

#Task 2 ---------------------------------

test200 <- sum(dexp(vals, rate = 1/200, log=TRUE))

#Task 3 ---------------------------------

LL_exp <- function(rate){
  
  sum(dexp(vals, rate = rate, log=TRUE))
  
}

#Task 4 ---------------------------------

rates <- 1/(50:200)

#Task 5 ---------------------------------

results <- sapply(rates, LL_exp)

#Task 6 ---------------------------------

plot(1/rates, results)

#Task 7 ---------------------------------

exp_results <- optimize(LL_exp, maximum=TRUE, lower=1/200, upper=1/50)

#Scoring --------------------------------

#scoreActivity::score253(day = 7)