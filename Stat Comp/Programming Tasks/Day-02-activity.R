#Ian Gallmeister
#Statistical Computing + Machine Learning
#26 January 2016

# Task 1 --------------------------------

#download.file("http://www-bcf.usc.edu/~gareth/ISL/Auto.csv", destfile="Auto.csv")
auto_file_name = "C:\\Users\\Ian\\Documents\\R\\Statistical Computing\\Auto.csv"

# Task 2 --------------------------------

Auto = read.csv("Auto.csv", na.strings="?")

# Task 3 --------------------------------

task3 = summary(Auto$horsepower)

# Task 4 --------------------------------

library(ISLR)

# Task 5 --------------------------------

task5top = Auto[1:5, 1:3]
task5bottom = Auto[(nrow(Auto) - 4):nrow(Auto), (ncol(Auto) - 2):ncol(Auto)]
#task5bottom = tail(Auto)[2:6, 1:3]


# Scoring -------------------------------

require(scoreActivity, quietly = TRUE )
score253(day = 2)