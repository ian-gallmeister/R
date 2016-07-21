#Ian Gallmeister
#Statistical Computing + Machine Learning
#4 February 2016

#Task 1 ---------------------------------

plot(1, xlim=c(0,100), ylim=c(0,100), type = "n", asp=1, xlab = "Independent", ylab="Dependent")

#Task 2 ---------------------------------

x1 = c(20, 40, 40, 20)
y1 = c(20, 20, 40, 40)
lines(x1, y1)

x2 = c(x1, 20)
x2_closed = c(x1, 20)
y2_closed = c(y1, 20)
lines(x2_closed, y2_closed)

#Task 3 ---------------------------------

angles = seq(0, 2*pi, length=1000)
x3 = 20*cos(angles) + 50
y3 = 60 + 20*sin(angles)
lines(x3, y3)

#Task 4 ---------------------------------

x4 = 15*abs(sin(angles))*cos(angles) + 7
y4 = 15*abs(sin(angles))*sin(angles) + 15
polygon(x4, y4, col="cyan")

#Task 5 ---------------------------------

x5 = 50 + 30*cos(angles)
y5 = 60 + 30*sin(angles)
polygon(x5, y5, col="red")

x6 = 50 + 20*cos(angles)
y6 = 60 + 20*sin(angles)
polygon(x6, y6, col="white")

x7 = 50 + 10*cos(angles)
y7 = 60 + 10*sin(angles)
polygon(x7, y7, col="red")

#Scoring --------------------------------

scoreActivity::score253(day = 4)

