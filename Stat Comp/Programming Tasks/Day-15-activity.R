#Ian Gallmeister
#Statistical Computing + Machine Learning
#29 March 2016

#CODE -----------------------------------

#download.file("http://tiny.cc/dcf/mona.rda", destfile = "mona.rda")
load("mona.rda")
X <- t(mona) - mean(mona[])
X_rand <- matrix(data = rnorm(191*250, mean = 0, sd = 1), nrow = 250)
X_corr <- X_rand%*%chol(var(X))

#Sparse Beta ----------------------------

beta <- numeric(191)
nonzero <- sample(1:191, 16)
for (i in nonzero){ beta[i] <- sample(c(2,5,-3,-4), 1) }

Y_pure <- X%*%beta
Y_real <- X%*%beta + rnorm(length(Y_pure), mean = 1, sd = sqrt(0.1*var(Y_pure)))

#Least Squares --------------------------

#Lasso Estimator ------------------------

#Principal Components -------------------

#Scoring --------------------------------

scoreActivity::score253(day = 15)