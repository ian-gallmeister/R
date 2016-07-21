#Trees
#------GOOD-------
#Fits around missing data well
#Fast
#Can innovate with trees
#Measure of importance -- Start with all cases. Null model has deviance.  Split has two deviances
#------BAD-------
#Not smooth
#Interpretability

Houses <- read.csv("http://tiny.cc/mosaic/SaratogaHouses.csv")
View(Houses)
mod1 <- lm(Price ~., data = Houses)
summary(mod1)
anova(mod1)
mod2 <- lm(Price ~ Fireplace + Living.Area + Baths, data = Houses)
summary(mod2)
library(rpart)
library(rpart.plot)
mod3 <- rpart(Price ~., data = Houses)
mod3
prp(mod3, type = 3)
mod4 <- rpart(Price ~., data = Houses, cp = 0.001)
prp(mod4)

mod5 <- rpart(Price ~ Fireplace + Living.Area, data = Houses, cp = 0.01)
prp(mod5, type = 4)
#Ways to __________
#Support Vector Machines
#Bagging -- lowers variance, is smoother
#Boosting -- make small model, add on another small model to ends with remaining data
#Neural Network
#Ridge Regression
#Lasso
#Random Forests + Bagging very closely related - come in same package
library(randomForest)
rf1 <- randomForest(Price ~., data = Houses, mtry = 6)
#can tell it how many trees to grow etc...
#Cross validation can let you set parameters if no idea what to do
importance(rf1)

rf2 <- randomForest( Price ~ ., data = Houses, mtry = 3 )
importance(rf2)

predict(rf2, newdata = data.frame(Living.Area = 2000, Baths = 2, Bedrooms = 3, Acres = 0.5, Age = 20, 
                                  Fireplace = factor("Y", levels = c("Y", "N")) ), ntrees = 200)
