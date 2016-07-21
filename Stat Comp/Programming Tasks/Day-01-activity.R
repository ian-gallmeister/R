#Ian Gallmeister
#Statistical Computing + Machine Learning
#21 January 2016

# Task 1 --------------------------------

library(mosaic)

# Task 2 --------------------------------

task2 = paste("Today is", date())

# Task 3 --------------------------------

task3a = names(Galton)
task3b = nrow(Galton)
task3c = mean(Galton[["height"]]) # mean of 4th column (index from 0)

# Task 4 --------------------------------

task4 = matrix( 1:6, ncol = 3, byrow = TRUE )

# Task 5 --------------------------------

task5x = runif(1000, min = 0, max = 1)
task5y = runif(1000, min = 0, max = 1)
task5pi = 4 * sum(sqrt(task5x*task5x + task5y*task5y) <= 1)/1000

# Scoring -------------------------------

require(scoreActivity, quietly = TRUE )
score253(day = 1)