#Ian Gallmeister
#23 February 2016
#Day 9

#Conditionals and Student Loans
data(Default, package = "ISLR")

#if (Default$student == "Yes"){
#  0
#} else {
#  100
#}
#Above is for a single datapoint.  This needs to apply to all students at once.
#In Python, would add a for loop. -> 
#for (k in 1:nrow(Default))
#if (Default[k,]$student == "Yes"){ result[k] = 0} else {result[k] = 100}

#In R, use a different function
#Students pay nothing, nonstudents pay.
you_pay <- ifelse( Default$student == "Yes", 0, 100 )

#But you pay 10% of your income
you_pay <- ifelse( Default$student == "Yes", 0, 0.1*Default$balance )

#If the loan balance is less than 10% of your income, pay 10% of the balance.  Otherwise pay 5% of your income
you_pay <- ifelse( Default$student == "Yes", 0, 
                   ifelse( Default$balance < 10*Default$income, 
                           0.1*Default$balance, 0.05*Default$income ) )

bigger_one <- with(Default, ifelse( balance > income, balance, income ))

#Use if here, because just have a single number
if (sum(Default$balance) > 10000 && mean(Default$balance > 5000)) {stop("Too much!")} else {100}