#(a)
data(Auto, package="ISLR")
mpg01 <- ifelse(Auto$mpg > median(Auto$mpg), 1, 0)
Auto2 <- data.frame(Auto, mpg01)

#(b)
plot(mpg01, Auto$horsepower)
plot(mpg01, Auto$displacement)
plot(mpg01, Auto$weight)
plot(mpg01, Auto$acceleration)
plot(mpg01, Auto$year)
plot(mpg01, Auto$cylinders)

#(c)
nrow(Auto)
test <- sample(392, 196)
testAuto <- Auto2[test,]
trainAuto <- Auto2[setdiff(1:392, test),]

#(d)
library(MASS)
lad <- lda(trainAuto$mpg01 ~ trainAuto$horsepower + trainAuto$weight)
lad
ldaprediction <- predict(lda, testAuto)
predictionclass <- ldaprediction$class
table(predictionclass)
mean(predictionclass == testAuto$mpg01)
#So it's accurate 49.5% of the time

#(e)
qad <- qda(trainAuto$mpg01 ~ trainAuto$horsepower + trainAuto$weight)
qad
qadclass <- predict(qad, testAuto)$class
table(qadclass)
mean(qadclass == testAuto$mpg01)
#So this is accurate 50% of the time

#(f)
logistic <- glm(trainAuto$mpg01 ~ trainAuto$horsepower + trainAuto$weight, family=binomial)
summary(logistic)
probability <- predict(logistic, testAuto, type="response")
prediction <- rep(1, 196)
prediction[probability<0.5] <- 0
mean(prediction == testAuto$mpg01)
#So this is accurate 48.4% of the time

#(g)
library(class)
trainAuto2 <- cbind(trainAuto$horsepower, trainAuto$weight)
testAuto2 <- cbind(testAuto$horsepower, testAuto$weight)
set.seed(1)
knnprediction <- knn(trainAuto2, testAuto2, trainAuto$mpg01, k=1)
table(knnprediction)
mean(knnprediction == testAuto$mpg01)
#Accurate 88% of the time
knnprediction5 <- knn(trainAuto2, testAuto2, trainAuto$mpg01, k=5)
table(knnprediction5)
mean(knnprediction5 == testAuto$mpg01)
#Accurate 87% of the time.  Roughly comparable.
knnprediction10 <- knn(trainAuto2, testAuto2, trainAuto$mpg01, k=10)
table(knnprediction10)
mean(knnprediction10 == testAuto$mpg01)
#Also accurat eabout 87% of the time.  Seems to be a theme.