#Unsupervised Learning

library(mosaicData)
head(Galton)
data(Galton)
library(lattice)
library(ggplot2)
library(DataComputing) # for the function %>%
densityplot( ~ height, data = Galton)
#names(output)
#output$center
#head(output$cluster)
#table(output$cluster)

output <- kmeans(Galton$height, c(63, 72), n = 2)
Galton$cluster2 <- output$cluster
Galton %>% ggplot(aes(x = height, color = cluster2)) + geom_density()
Galton %>% ggplot(aes(x = height, color = as.factor(cluster2))) + geom_freqpoly()


output <- kmeans(Galton$height, c(63, 60, 72), n = 3)
Galton$cluster3 <- output$cluster
Galton %>% ggplot(aes(x = height, color = as.factor(cluster3))) + geom_freqpoly()


with(Galton, table(sex, cluster2))

#KMeans gives nonoverlapping groups
#To get overlapping ones, split into groups, 