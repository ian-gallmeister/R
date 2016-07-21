#Ian Gallmeister
#Statistical Computing + Machine Learning
#18 February 2016

#Task 1.1 ---------------------------------

x <- runif(100, min = 20, max = 70) 
y <- 5 + 3*x + 2*rnorm(100)
My_data <- data.frame( x, y )

#Alt method
#My_data <- data.frame(x = runif(100, max = 70, min = 20))
#My_data$y <- 5 + 3*x + 2*rnorm(100)

#Task 1.2 ---------------------------------

plot(My_data)

#Alt syntax: plot(y ~ x, data=My_data)

#Task 1.3 ---------------------------------

#LL_line <- function(params){
LL_line <- function(params, data){ #alt
  m <- params[1] #slope
  b <- params[2] #intercept
  sigma <- params[3] #std. deviation
  
  resid <- with(data, y - (m*x + b)) #residuals
  likelihood <- dnorm(resid, sd = sigma, mean = 0, log = TRUE)
  llikelihood <- sum(likelihood)
  
  #llikelihood <- sum(log(dnorm(y - (m*x + b), sd = sigma)))
  llikelihood
}

#Task 1.4 ---------------------------------

testA <- LL_line(c(3, 5, 2), My_data)
testB <- LL_line(c(4, 1, 10), My_data)

#Task 1.5 ---------------------------------

starting_params <- c(4, 1, 10)
best <- optim(starting_params, LL_line, control = list(fnscale = -1), data = My_data)

#Task 2 -----------------------------------

#load(url("http://tiny.cc/dcf/Taxi_trips.rda"))

taxi_likelihood <- function(inputs){
  base_fare <- inputs[1]
  per_mile <- inputs[2]
  params <- inputs[3]
  
  #waiting_fare is residuals - assume it can be modelled randomly
  waiting_fare <- with(Taxi_trips, fare_amount - (base_fare + per_mile*trip_distance))
  llikelihood <- sum(log(0.001 + dexp(waiting_fare, rate = params, log = FALSE)))
  #llikelihood <- sum(log(0.00000001 + dexp(Taxi_trips$fare_amount - (per_mile*Taxi_trips$trip_distance + base_fare), rate = params, log = FALSE)))
  
  llikelihood
}

starting_params <- c(3,0.10,0.1)
best <- optim(starting_params, taxi_likelihood, control = list(fnscale = -1))

with(Taxi_trips, plot(trip_distance, fare_amount))
abline(best[[1]][[1]], best[[1]][[2]], col = 'Cyan')

#Scoring --------------------------------

scoreActivity::score253(day = 8)