#Ian Gallmeister
#Statistical Computing + Machine Learning
#31 March 2016

#The Knots ------------------------------

my_knots <- function(x, k){
  stats::quantile(x, probs = (1:k)/(k+1))
}

#The Basis Set --------------------------

spline_model_matrix <- function(x, knot_locations){
  MM <- cbind(1, x, x^2, x^3)
  for (knot in knot_locations){
    new_vector <- ifelse(x < knot, 0, (x - knot)^3)
    MM <- cbind(MM, new_vector)  
  }
  return(MM)
}

#Finding the best linear combination ----

#spline_coefs <- coef(lm(y ~ MM))
fit_spline <- function(formula, k=2, data=parent.frame()){
  y <- eval(formula[[2]], envir=data)
  x <- eval(formula[[3]], envir=data)
  knot_locations <- my_knots(x, k)
  MM <- spline_model_matrix(x, knot_locations)
  mod <- lm(y ~ MM - 1 )
  res <- list(coef = coef(mod), knots = knot_locations, cov = vcov(mod))
  class(res) <- "my_spline"
  return(res)
}


#The Predict Function -------------------

predict.my_spline <- function(mod, newx = NULL, level = 0.95, intervals=c("none", "confidence", "prediction")){
  intervals <- match.arg(intervals)
  MM <- spline_model_matrix(newx, mod$knots)
  vals <- MM %*% mod$coef
  se <- sqrt(rowSums(MM %*% mod$cov * MM))
  if (intervals == "none") return(vals)
  else return(NULL)
}
#library(ISLR)
#data(Wage)
res <- fit_spline(wage~age, data=Wage)
predict(res)

#Scoring --------------------------------

scoreActivity::score253(day = 16)