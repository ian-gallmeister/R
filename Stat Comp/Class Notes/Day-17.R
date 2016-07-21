#Flexibility (-> decreased bias) vs Variance
#Parameter to move in 1 direction or the other
#\lambda in Ridge Regression/Lasso  --> Cross Validation to find ideal lambda
#Polynomials (booo!) -- Cubic splines: 1, x, x^2, x^3, (x - NOTi)^3 <- for all i
#Natural splines + beta splines --> parameter is number of knots
#Smoothing splines --> parameter is lambda (lambda --> constrained optimization)
#                  --> punish non-smoothness, reward smoothness
#                  --> smoothness in sense of 2nd derivative
#                  --> \int |f^{(z)}(x)| dx
#                  --> polynomials very not smooth
#                  --> \sum\left( y_i - f(x_i) \right) + \lambda \int |f^{(z)}(x)|dx
#                  --> Need function continuous in their first derivative
#White noise, Browinan Motion <- Wiener Process
#Noisy function -> length scale comparable to the noise
#Large run so noise cancelled out so derivative needs space for noisy function
#Polynomials don't have this b/c no noise in polynomials

##################################################################
##################################################################
#Find the first derivative
first <- function(f, x = 1, h = 0.001){ #First Derivative
  function(x) {(1/(2*h))*(f(x + h) - f(x - h))}
}
##################################################################
##################################################################


x <- seq(0, 10, length = 1000)
fprime <- first(sin)
#> fprime
#function(x) {(1/(2*h))*(f(x + h) - f(x - h))}
#<environment: 0x0000000014544cc8>
y <- fprime(x)
plot(x, y, type = "l")

#Well, this is a little messed up --> numbers discrete past 16 digits, so differences go a little wonky.
fprime <- first(sin, h = 0.0000000000000001)
plot(x, fprime(x))
.Machine$double.eps #--> smallest value for computer

#This h is the smallest one you want to use
fprime <- first(sin, h = sqrt(.Machine$double.eps))
plot(x, fprime(x), type = "l")

##################################################################
##################################################################
#Find the 2nd derivative
second <- function(f, h = 0.001){
  fprime <- first(f, h=h)
  first(fprime)
}
##################################################################
##################################################################

fdoubleprime <- second(sin)
fdoubleprime
plot(x, fdoubleprime(x), type = "l")

##################################################################
##################################################################
penalty <- function(f, a = 0, b = 1, h = 0.001){
  f2 <- function(x) {abs(second(f, h = h)(x)^2)}
  integrate(f2, a, b)
}

#Natural Spline -- linear outside x's with data