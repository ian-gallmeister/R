#Ian Gallmeister
#Statistical Computing + Machine Learning
#8 March 2016

#Generate Data --------------------------

n_cases <- 100
red_mean <- c(1,0)
green_mean <- c(0, -1)
blue_mean <- c(-1, 1)

covar_1 <- matrix(c(3, -1.7, -1.7, 1), byrow=TRUE, ncol=2)
covar_2 <- matrix(c(2, 1.5, 1.5, 3), byrow=TRUE, ncol=2)

one <- cbind(rnorm(n_cases), rnorm(n_cases))
two <- cbind(rnorm(n_cases), rnorm(n_cases))
three <- cbind(rnorm(n_cases), rnorm(n_cases))

red <- chol(covar_1)
green <- 2*chol(covar_1)
blue <- 3*chol(covar_2)

red[,1] <- red[,1] + red_mean[1]
red[,2] <- red[,2] + red_mean[2]

green[,1] <- green[,1] + green_mean[1]
green[,2] <- green[,2] + green_mean[2]

blue[,1] <- blue[,1] + blue_mean[1]
blue[,2] <- blue[,2] + blue_mean[2]

Red <- data.frame(x = red[,1], y=red[,2], class = "red", stringsAsFactors = FALSE)
Blue <- data.frame(x = blue[,1], y=blue[,2], class = "blue", stringsAsFactors = FALSE)
Green <- data.frame(x = green[,1], y = green[,2], class = "green", stringsAsFactors = FALSE)

Sim_one <- rbind(Red, Green)
Sim_two <- rbind(Red, Blue)

#LDA and QDA ----------------------------
#LDA
mod_LDA_one <- MASS::lda(class ~ x + y, data=Sim_one)
test_LDA_one <- predict(mod_LDA_one, newdata=Sim_one)

#QDA
mod_QDA_one <- MASS::qda(class ~ x + y, data=Sim_one)
test_QDA_one <- predict(mod_QDA_one, newdata=Sim_one)

#Scoring --------------------------------

scoreActivity::score253(day = 12)