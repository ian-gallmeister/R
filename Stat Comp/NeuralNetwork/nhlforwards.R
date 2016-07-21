#Data Analysis - NHL Forwards

library(ggplot2)
library(ISLR)
library(MASS)

forwards <- read.csv("nhlforwards.csv", row.names = 1)
#Originally found with sample(nrow(forwards)/2, nrow(forwards)), now is this to preserve datasets used in python
training_indices <- c(8,391,40,231,354,109,82,448,385,320,392,53,544,534,156,525,275,248,218,79,
                     3,93,540,414,480,85,49,409,400,476,174,445,348,208,47,387,418,508,469,180,
                     69,102,285,438,257,15,282,338,362,149,135,460,475,20,505,117,377,115,457,496,
                     223,408,398,491,52,195,543,343,169,443,265,183,520,326,532,12,292,242,429,512,
                     307,492,419,299,263,204,101,266,519,346,495,196,300,140,163,1,529,99,119 ,6,
                     184,407,160,364,59,426,33,287,181,473,284,330,126,342,80,507,224,293,252,134,
                     546,361,389,35,337,28,471,523,335,237,420,96,356,141,234,23,538,219,7,372,
                     401,332,230,4,442,9,518,202,545,349,399,255,114,294,410,390,81,422,167,44,
                     194,31,209,484,201,325,468,199,50,290,291,187,55,147,322,254,11,244,104,164,
                     91,455,437,150,323,86,308,173,166,280,352,48,110,365,5,216,279,453,63,145,
                     464,286,258,490,256,298,380,243,105,513,185,430,211,436,21,26,176,276,462,37,
                     186,10,373,220,316,162,146,461,406,463,132,261,386,74,353,88,378,144,143,435,
                     268,60,121,411,171,269,122,296,379,470,295,217,137,421,434,70,130,29,536,309,
                     76,289,66,120,225,477,94,229,344,245,172,510,450,251,314)
testing_indices <- -training_indices
train_forwards <- forwards[training_indices,]
test_forwards <- forwards[testing_indices,]

#graphs
#> names(forwards)
#[1] "Player" "Years"  "Teams"  "GP"     "Goal"   "Shots"  "Sh."    "TOI"    "Cap"    "Star"  
ggplot(forwards, aes(x = Years, y = Star, alpha = 0.1)) + geom_point(position = position_jitter(width = 0, height = 0.3), aes(colour=ifelse(forwards$Star > 0.5, 'darkorchid', 'gold4')))
ggplot(forwards, aes(x = Cap, y = Star, alpha = 0.1)) + geom_point(position = position_jitter(width  = 0, height = 0.3), aes(colour=ifelse(forwards$Star > 0.5, 'darkorchid', 'gold4')))
ggplot(forwards, aes(x = GP, y = Star, alpha = 0.1)) + geom_point(position = position_jitter(width = 0, height = 0.3), aes(colour=ifelse(forwards$Star > 0.5, 'darkorchid', 'gold4')))
ggplot(forwards, aes(x = Goal, y = Star, alpha = 0.1)) + geom_point(position = position_jitter(width = 0, height = 0.3), aes(colour=ifelse(forwards$Star > 0.5, 'darkorchid', 'gold4')))
ggplot(forwards, aes(x = Shots, y = Star, alpha = 0.1)) + geom_point(position = position_jitter(width = 0, height = 0.3), aes(colour=ifelse(forwards$Star > 0.5, 'darkorchid', 'gold4')))
#Remove SSS high Sh% people from consideration (246, 279, 478, 257, 276, 360)
ggplot(forwards[-c(246, 279, 478, 257, 276, 360), ], aes(x = Sh., y = Star, alpha = 0.1)) + geom_point(position = position_jitter(width = 0, height = 0.3))#, aes(colour=ifelse(forwards$Star > 0.5, 'darkorchid', 'gold4')))
ggplot(forwards, aes(x = TOI, y = Star, alpha = 0.1)) + geom_point(position = position_jitter(width = 0, height = 0.3), aes(colour=ifelse(forwards$Star > 0.5, 'darkorchid', 'gold4')))

#Neural Network Coefficients
#-0.45588379115313554 -- Years
#1.566633202802108 -- Teams
#-29.167007454346205 -- GP
#17.879819852899384 -- Goals
#1.984327078842147 -- Shots
#1.6916332104492906 -- Sh.
#21.287589391692475 -- TOI
#10.507345214014052 -- Cap


#####################
#LOGISTIC REGRESSION#
#####################
#Years, Cap, Goal, Shots, TOI -- GP*Sh., GP*TOI

#All Variables
logistic_all <- glm( Star ~., data = train_forwards, family=binomial )
summary(logistic_all)
alllogistic_test <- predict(logistic_all, newdata = test_forwards)
probabilties <- predict(logistic_all, type="response")
#table(alllogistic_test, Star)
predictions <- rep(0, 276)
predictions[probabilties > 0.5] <- 1

with(test_forwards, table(predictions, Star))
#> with(test_forwards, table(predictions, Star))
              #Star
#predictions   0   1
          #0 191  42
          #1  36   7

#Some Variables
logistic_select <- glm( Star ~ GP*Sh. + Years + Cap + Shots + TOI, 
                        data = train_forwards, family = binomial )
summary(logistic_select)
somelogistic_test <- predict(logistic_select, newdata = test_forwards)

predictions2 <- rep(0, 276)
predictions2[probabilties > (1 - 0.176)] <- 1
with(test_forwards, table(predictions2, Star))
              #Star
#predictions2  0   1
          #0 203  44
          #1  24  5

#Three Best Neural Network Variables
logistic_nnbest <- glm( Star ~ Goal + TOI + Cap, data = train_forwards, family = binomial )
summary(logistic_nnbest)
iunno <- predict(logistic_nnbest, newdata = test_forwards)
probabilities3 <- rep(0, 276)
probabilities3[iunno > 0.176] <- 1
with(test_forwards, table(probabilities3, Star))
#Star
#probabilities3   0   1
#0 220  29
#1   7  20

##############################
#LINEAR DISCRIMINANT ANALYSIS#
##############################

#All Variables
lda_all <- lda( Star ~., data = train_forwards )
lda_all
#Call:
#  lda(Star ~ ., data = train_forwards)
#
#Prior probabilities of groups:
#  0         1 
#0.7963636 0.2036364 
#
#Group means:
#  Years    Teams       GP      Goal     Shots        Sh.      TOI     Cap
#0 5.429224 2.219178 276.6027  48.19178  475.7397 0.09421273 4009.434 1592033
#1 7.446429 1.875000 487.4286 163.82143 1356.0357 0.11827517 9051.643 4841737
#
#Coefficients of linear discriminants:
#  LD1
#Years -3.378464e-02
#Teams  1.989107e-02
#GP    -4.411038e-03
#Goal   1.471520e-02
#Shots  2.890550e-05
#Sh.   -3.229450e-03
#TOI    1.110712e-04
#Cap    3.006619e-07

#plot(lda_all)
predictions_all <- predict( lda_all, newdata = test_forwards )
#>names(predictions_all)
#[1] "class"    "posterior" "x"
all_class <- predictions_all$class
table(all_class, test_forwards$Star)
 #all_class   0   1
         #0 220  21
         #1   7  28

sum(predictions_all$posterior[,1]>=(1 - .176)) #[1] 219 - predicted normal
sum(predictions_all$posterior[,1]<(1 - .176))  #[1] 57 - predicted star (49 actually)

#Some Variables
lda_some <- lda( Star ~ GP*Sh. + Years + Cap + Shots + TOI, data = train_forwards )
lda_some
#Call:
#  lda(Star ~ GP * Sh. + Years + Cap + Shots + TOI, data = train_forwards)
#
#Prior probabilities of groups:
#  0         1 
#0.7963636 0.2036364 
#
#Group means:
#  GP        Sh.    Years     Cap     Shots      TOI   GP:Sh.
#0 276.6027 0.09421273 5.429224 1592033  475.7397 4009.434 27.37154
#1 487.4286 0.11827517 7.446429 4841737 1356.0357 9051.643 58.63605
#
#Coefficients of linear discriminants:
#  LD1
#GP     -6.922539e-03
#Sh.     3.615062e-01
#Years  -1.583413e-02
#Cap     3.062910e-07
#Shots   1.604887e-03
#TOI     2.031976e-04
#GP:Sh.  8.406666e-03

#plot(lda_some)
predictions_some <- predict( lda_some, newdata = test_forwards )

some_class <- predictions_some$class
table(some_class, test_forwards$Star)
 #some_class   0   1
          #0 219  20
          #1   8  29

sum(predictions_some$posterior[,1]>=(1 - .176)) #[1] 217 - predicted normal
sum(predictions_some$posterior[,1]<(1 - .176))  #[1] 59 - predicted star (49 acutally)

#3 Best Neural network Variables
lda_nnb <- lda( Star ~ Goal + TOI + Cap, data = train_forwards )
lda_nnb
#Call:
#  lda(Star ~ Goal + TOI + Cap, data = train_forwards)
#
#Prior probabilities of groups:
#  0         1 
#0.7963636 0.2036364 
#
#Group means:
#  Goal      TOI     Cap
#0  48.19178 4009.434 1592033
#1 163.82143 9051.643 4841737
#
#Coefficients of linear discriminants:
#  LD1
#Goal  1.990713e-02
#TOI  -2.368100e-04
#Cap   3.337289e-07

#plot(lda_some)
predictions_nnb <- predict( lda_nnb, newdata = test_forwards )

nnb_class <- predictions_nnb$class
table(nnb_class, test_forwards$Star)
#nnb_class   0   1
        #0 218  26
        #1   9  23


sum(predictions_nnb$posterior[,1]>=(1 - .176)) #[1] 219 - predicted normal -- Same as some
sum(predictions_nnb$posterior[,1]<(1 - .176))  #[1] 57 - predicted star -- Same as some

#################################
#QUADRATIC DISCRIMINANT ANALYSIS#
#################################
#All Variables
qda_all <- qda( Star ~ ., data = train_forwards )
qda_all
#Call:
#  qda(Star ~ ., data = train_forwards)
#
#Prior probabilities of groups:
#  0         1 
#0.7963636 0.2036364 
#
#Group means:
#  Years    Teams       GP      Goal     Shots        Sh.      TOI     Cap
#0 5.429224 2.219178 276.6027  48.19178  475.7397 0.09421273 4009.434 1592033
#1 7.446429 1.875000 487.4286 163.82143 1356.0357 0.11827517 9051.643 4841737

#plot(lda_some)
qpredictions_all <- predict( qda_all, newdata = test_forwards )

qall_class <- qpredictions_all$class
table(qall_class, test_forwards$Star)
#qall_class   0   1
         #0 199  13
         #1  28  36

sum(qpredictions_all$posterior[,1]>=(1 - .176)) #[1] 195 - predicted normal
sum(qpredictions_all$posterior[,1]<(1 - .176))  #[1] 81 - predicted star

#Some Variables
qda_some <- qda( Star ~ GP*Sh. + Years + Cap + Shots + TOI, data = train_forwards )
qda_some
#Call:
#  qda(Star ~ GP * Sh. + Years + Cap + Shots + TOI, data = train_forwards)
#
#Prior probabilities of groups:
#  0         1 
#0.7963636 0.2036364 
#
#Group means:
#  GP        Sh.    Years     Cap     Shots      TOI   GP:Sh.
#0 276.6027 0.09421273 5.429224 1592033  475.7397 4009.434 27.37154
#1 487.4286 0.11827517 7.446429 4841737 1356.0357 9051.643 58.63605

#plot(lda_some)
qpredictions_some <- predict( qda_some, newdata = test_forwards )

qsome_class <- qpredictions_some$class
table(qsome_class, test_forwards$Star)
#qsome_class   0   1
          #0 198  13
          #1  29  36

sum(qpredictions_some$posterior[,1]>=(1 - .176)) #[1] 191 - predicted normal
sum(qpredictions_some$posterior[,1]<(1 - .176))  #[1] 85 - predicted star

#3 Best Neural Network
qda_nnb <- qda( Star ~ Goal + TOI + Cap, data = train_forwards )
qda_nnb
#Call:
#  qda(Star ~ Goal + TOI + Cap, data = train_forwards)
#
#Prior probabilities of groups:
#  0         1 
#0.7963636 0.2036364 
#
#Group means:
#  Goal      TOI     Cap
#0  48.19178 4009.434 1592033
#1 163.82143 9051.643 4841737

#plot(lda_some)
qpredictions_nnb <- predict( qda_nnb, newdata = test_forwards )

qnnb_class <- qpredictions_nnb$class
table(qnnb_class, test_forwards$Star)
#qnnb_class   0   1
         #0 210  19
         #1  17  30

sum(qpredictions_nnb$posterior[,1]>=(1 - .176)) #[1] 214 - predicted normal
sum(qpredictions_nnb$posterior[,1]<(1 - .176))  #[1] 62 - predicted star

######################
#CLASSIFICATION TREES# -- Not going to be particularly effective
######################