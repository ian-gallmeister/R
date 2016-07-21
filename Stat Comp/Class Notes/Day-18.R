#Day 18 -- Trees
#Start with all cases
#Class  x  y
#A      1  2
#B      2  5
#A      3  1
#A      4  3
#B      5  8
#B      6  5
#A      7  4
#B      8  6
#Mean = 4.25
#RSS = 35.5
#Can break this up into two groups
#   Split based on x and calculate mean + RSS
y <- c(2,5,1,3,8,5,4,6)
mean(y)
sum((y - 4.25)^2) #Sum of square redisuals

#Split at 5th position
left <- y[1:5]
right <- y[6:length(y)]
mean(left) # = 3.8
sum((left-mean(left))^2) # = 30.8
mean(right) # = 5
sum((right - mean(right))^2) # = 2

#Combined RSS smallest for 4-4 split
#mean(y[1:4]) = 2.75, rss <- sum((y[1:4] - mean(y[1:4]))^2) = 8.75
#mean(y[5:8]) = 5.75, rss <- sum((y[5:8] - mean(y[5:8]))^2) = 5

#Split Again
left <- y[1:4]
right <- y[5:8]
mean(left[1:2])
sum((left[1:2] - mean(left[1:2]))^2)
mean(left[3:4])
sum((left[3:4] - mean(left[3:4]))^2)

#Decompose all the way, then start removing nodes by those with least improvement to RSS
#(Use RSS because it's extensive.  Improvement is difference between sum of RSS of 
#new groupings subtracted from RSS of previous node with elements of both leaves)

library(tree) #Main guy behind R packages.  Kind of unpleasant.  Wrote tree (one of first R packages)
Cartoon_data <- data.frame(
  x = 1:8,
  y = c(2,5,1,3,8,5,4,6),
  class = c("A", "B", "A", "A", "B", "B", "A", "B")
)

#Perfectly Pure Trees

pure <- tree.control(8, mincut = 0, minsize = 1, mindev = 0)
rtree_pure <- tree(y~x, data=Cartoon_data, control = pure)
plot(rtree_pure)
text(rtree_pure) #Adds labels to trees
#> rtree_pure
#node), split, n, deviance, yval
#* denotes terminal node
#1) root 8 35.50 4.25  # root, items in node, RSS, mean
#  2) x < 4.5 4  8.75 2.75  
#    4) x < 2.5 2  4.50 3.50  
#      8) x < 1.5 1  0.00 2.00 *
#      9) x > 1.5 1  0.00 5.00 *
#    5) x > 2.5 2  2.00 2.00  
#      10) x < 3.5 1  0.00 1.00 *
#      11) x > 3.5 1  0.00 3.00 *
#  3) x > 4.5 4  8.75 5.75  
#    6) x < 5.5 1  0.00 8.00 *
#    7) x > 5.5 3  2.00 5.00  
#      14) x < 7.5 2  0.50 4.50  
#        28) x < 6.5 1  0.00 5.00 *
#        29) x > 6.5 1  0.00 4.00 *
#      15) x > 7.5 1  0.00 6.00 *


#8 cases, probability something in one class or the other
#RSS?  --  Deviance RSS = (1/n)e^{-r^2/(2\sigma^2)}
#Maximize log likelihood or minimize negative of log likelihood.  Same thing.
ctree_pure <- tree(class ~ x, data=Cartoon_data, control=pure) #Not Working.  Must be something in Rmd
plot(ctree_pure)
text(ctree_pure)

pure_for_cps <- tree.control(nrow(CPS85), mincut = 0, minsize = 1, mindev = 0)
Sector_classifier <- tree(sector ! wage + sex + educ + exper, data = mosaicData:CPS85, control = pure_for_cps)
#More code that isn't there yet ...