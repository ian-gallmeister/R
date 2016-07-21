#Day14

our_mean <- function(x){
  #prep for the loop
  sum_sofar <- 0
  #loop
  for (k in 1:length(x)){
    #update state
    sum_sofar <- sum_sofar + x[k]
  }
  #package up result
  sum_sofar/length(x) #calculate mean -- this gets returned since it's just there as a value
}

nth_fibonacci <- function(n){
  n_minus_1th <- 0
  nth <- 1
  holder <- 0
  if (n == 1) {0}
  else if (n == 2) {1}
  else {
    for (k in 3:n) {
      holder <- nth
      nth <- nth + n_minus_1th
      n_minus_1th <- holder
    }
  }
  nth
}

fibonacci <- function(n){
  if (0 > n) return("No negatives!")
  sequence <- numeric(n)
  sequence[1] <- 0
  sequence[c(2,3)] <- 1
  for (k in 3:n) {
    sequence[k] <- sequence[k-1] + sequence[k-2]
  }
  sequence
}


#########################################################
#Leave-one-out Cross Vaidation

#model <- lm(width ~ length*sex, data = KidsFeet)

#State
error <- numeric(nrow(KidsFeet))
for ( i in 1:nrow(KidsFeet)){
  #testing_data <- KidsFeet[i,]
  #training_data <- KidsFeet[-i,]
  model <- lm(width ~ length*sex, data = KidsFeet[-i, ])
  test_output <- predict(model, newdata = KidsFeet[i, ])
  error[i] <- KidsFeet[i,]$width - test_output
}
sum(error^2)

