#Ian Gallmeister
#Statistical Computing + Machine Learning
#26 January 2016

#Task 1 ---------------------------------

library("ISLR")
data(College, package = "ISLR")

#Task 2 ---------------------------------

Yield = college[["Enroll"]]/college[["Accept"]]
College = data.frame(college, Yield)

#Task 3 ---------------------------------

all_indices = c(1:nrow(College))
train_indices = sample(all_indices, 200)
test_indices = setdiff(all_indices, train_indices)
Train_data = College[train_indices,]
Test_data = College[test_indices,]

#Task 4 --------------------------------

Yield_mod1 = lm(Yield ~ Top10perc + Outstate + Expend, Train_data)

#Task 5 --------------------------------

Y_train = Train_data$Yield
fhat_train = predict(Yield_mod1, newdata = Train_data)
MSE_train = sum((Y_train - fhat_train)^2)/length(train_indices)

#Task 6 --------------------------------

Y_test = Test_data$Yield
fhat_test = predict(Yield_mod1, newdata = Test_data)
MSE_test = sum((Y_train - fhat_train)^2)/length(test_indices)

#Scoring -------------------------------

scoreActivity::score253(day = 3)