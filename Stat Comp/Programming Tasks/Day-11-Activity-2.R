#Day 11 Activity

Sigma <- matrix(c(1, -.7, -.7, 1), nrow = 2, ncol = 2, byrow = TRUE)
InvSigma <- solve(Sigma)
#A x = b
#solve(A, b)
#If no b, assumes an identity matrix

#Cholesky Decomposition of Sigma
# Transpose of A * A is Sigma --> t(A) %*% A == Sigma
A <- chol(Sigma)

x <- rnorm(10000)
y <- rnorm(10000)
#Plot to see largely circularly distributed random data
plot(x, y, pch=".", asp =  1)

#Take random uncorrelated data and multiply by the covariance matrix to correlate it
new <- cbind(x, y) %*% A
head(new)

#Plot to see elliptically distributed correlated data generated from random data
plot(new[,1], new[,2], pch = ".", asp = 1)

#Find the correlation - hint: looks very much like Sigma
#Matrix of form:
# X to X, X to Y
# Y to X, Y to Y
cor(new)

#Covariance matrix is:
#Matrix of form:
# sigma_x^2,       sigma_x*sigma_y
# sigma_x*sigma_y, sigma_y^2
cov(new)
