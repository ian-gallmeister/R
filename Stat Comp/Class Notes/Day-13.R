require(ISLR)
require(MASS, quietly = TRUE)
##
## Attaching package: ’MASS’
## The following object is masked from ’package:dplyr’:
##
## select
mod1 <- lda(default ~ balance + income, data = Default)
#mod1
#Call:
#  lda(default ~ balance + income, data = Default)
#
#Prior probabilities of groups:
#  No    Yes 
#0.9667 0.0333 
#
#Group means:
#  balance   income
#No   803.9438 33566.17
#Yes 1747.8217 32089.15
#
#Coefficients of linear discriminants:
#  LD1
#balance 2.230835e-03
#income  7.793355e-06

# plot(mod1)
pts <- with(Default,
            expand.grid(
              balance = seq(min(balance), max(balance), length = 100),
              income = seq(min(income), max(income), length = 100)
            ))
out <- predict(mod1, newdata = pts)$class

#predict()
#   Generic Function
#     function (object, ...) 
#     UseMethod("predict")
#     <bytecode: 0x000000000a0784a0>
#     <environment: namespace:stats>
#UseMethod looks at type of object, then calls particular version of predict() based on object type
#class(mod1) == "lda", so predict(mod1) calls predict.lda
#methods(predict) shows methods.  Ones with * are "invisible" --> can't get basic info from just putting f'n name into console
#Both packages and random users can add versions to predict()
#Can call predict.glm() or predict.poly() etc...
#Can also call ?predict.glm etc... for more specific information.

#To get at invisible functions, use three colons.
pts <- cbind(pts, prediction = out)
#prediction = out is te prediction outcome

plot(balance ~ income, data = Default, col = default)
#defaults are red, nondefaults in black

plot(balance ~ income, data = pts, col = prediction, pch = 20)
#area of space - more 

#########################################################################################
#Now fit the model with prior = c(.5, .5)
mod3 <- lda(default ~ balance + income, data = Default, prior=c(0.5, 0.5))
# plot(mod1)
pts <- with(Default,
            expand.grid(
              balance = seq(min(balance), max(balance), length = 100),
              income = seq(min(income), max(income), length = 100)
            ))
out <- predict(mod2, newdata = pts)$class
pts <- cbind(pts, prediction = out)
plot(balance ~ income, data = Default, col = default)
plot(balance ~ income, data = pts, col = prediction, pch = 20)

################################################################################################
#Show as well for qda()
mod3 <- qda(default ~ balance + income, data = Default, prior=c(0.5, 0.5))
# plot(mod1)
pts <- with(Default,
            expand.grid(
              balance = seq(min(balance), max(balance), length = 100),
              income = seq(min(income), max(income), length = 100)
            ))
out <- predict(mod3, newdata = pts)$class
pts <- cbind(pts, prediction = out)
plot(balance ~ income, data = Default, col = default)
plot(balance ~ income, data = pts, col = prediction, pch = 20)
