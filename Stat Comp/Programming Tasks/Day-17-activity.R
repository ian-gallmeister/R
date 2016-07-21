#Ian Gallmeister
#Statistical Computing + Machine Learning
#12 April 2016

#library(tree)
Cartoon_data <- data.frame(
  x = 1:8,
  y = c(2,5,1,3,8,5,4,6),
  class = c("A","B","A","A","B","B","A","B")
)

#Perfectly Pure Trees
pure <- tree.control(8, mincut = 0, minsize = 1, mindev = 0)

  #Regression Tree
  rtree_pure <- tree(y~x, data=Cartoon_data, control = pure)
  plot(rtree_pure)
  text(rtree_pure) #Adds labels to trees

  #Classification Tree
  ctree_pure <- tree(class~x, data = Cartoon_data, control = pure)
  plot(ctree_pure)
  text(ctree_pure)

#Evaluating the Tree
predict(rtree_pure)
predict(ctree_pure)
predict(rtree_pure, newdata = data.frame(x = 3))
predict(ctree_pure, newdata = data.frame(x = 7))

#Deviance of Each Node
print(rtree_pure)
print(ctree_pure)


#Deviance of Each Tree
#dev_rtree_pure <- sum(rtree_pure[1]$frame$dev)
#dev_ctree_pure <- sum(ctree_pure[1]$frame$dev)
dev_rtree <- sum(ifelse(rtree_pure[1]$frame$var == "<leaf>", rtree_pure[1]$frame$dev, 0))
dev_ctree <- sum(ifelse(ctree_pure[1]$frame$var == "<leaf>", ctree_pure[1]$frame$dev, 0))

#Pruning the Tree
rtree_5 <- prune.tree(rtree_pure, best = 5)
ctree_2 <- prune.misclass(ctree_pure, best = 2)
nterminal <- 2:8
regmod <- predict(rtree_5, newdata = data.frame(x = nterminal))
#1 2 3 4 5 6 7 
#5 2 2 8 5 5 5 

#tree_deviance <- ???

#Building a Real Classifier
pure_for_cps <- tree.control(nrow(mosaicData::CPS85), mincut = 0, minsize = 1, mindev = 0)
Sector_classifier <- tree(sector ~ wage + sex + educ + exper, 
                          data = mosaicData::CPS85, control = pure_for_cps)

new_sector_classifier <- prune.tree(Sector_classifier, best = 20)
dev_sectclas <- sum(ifelse(new_sector_classifier[1]$frame$var == "<leaf>", 
                           new_sector_classifier[1]$frame$dev, 0))
#Deviance of terminal nodes = 1426.738

#Scoring --------------------------------

scoreActivity::score253(day = 17)
