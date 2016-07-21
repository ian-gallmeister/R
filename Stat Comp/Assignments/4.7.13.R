data(Boston)
pairs(Boston)
#lstat, age, nox, ptratio, tax

crime01 <- ifelse(Boston$crim > median(Boston$crim), 1, 0)
Boston2 <- data.frame(Boston, crime01)
train <- sample(nrow(Boston), 0.5*nrow(Boston))
test <- setdiff(1:nrow(Boston), train)
trainBoston <- Boston2[train,]
testBoston <- Boston2[test,]

library(MASS) #LDA
library(class) #KNN

#Logistic
log <- glm(crime01 ~ lstat + age + nox + ptratio + tax, data=trainBoston, family=binomial)
summary(log)

log2 <- glm(crime01 ~ nox + rm, data=trainBoston, family=binomial)
summary(log2)
chances <- predict(log2, type="response")
predictions <- rep(0, nrow(trainBoston))
predictions[chances>0.5] <- 1
table(predictions, testBoston$crime01)
mean(predictions == testBoston$crime01)
#51% accuracy

#LDA
lad <- lda(crime01 ~ nox + rm, data = trainBoston)
lad
chances <- predict(lad, testBoston)
table(chances$class, testBoston$crime01)
mean(chances$class == testBoston$crime01)
#85% accuracy

#KNN
trainBoston2 <- cbind(trainBoston$nox, trainBoston$rm)
testBoston2 <- cbind(testBoston$nox, testBoston$rm)
set.seed(1)

knnprediction3 <- knn(trainBoston2, testBoston2, testBoston$crime01, k=3)
table(knnprediction3)
mean(knnprediction3 == testBoston$crime01)
#57% accuracy

knnprediction5 <- knn(trainBoston2, testBoston2, testBoston$crime01, k=5)
table(knnprediction5)
mean(knnprediction5 == testBoston$crime01)
#54% accuracy

knnprediction10 <- knn(trainBoston2, testBoston2, testBoston$crime01, k=10)
table(knnprediction10)
mean(knnprediction10 == testBoston$crime01)
#57% accuracy

knnprediction30 <- knn(trainBoston2, testBoston2, testBoston$crime01, k=30)
table(knnprediction30)
mean(knnprediction30 == testBoston$crime01)
#56% accuracy