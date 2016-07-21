#Ian Gallmeister
#Statistical Computing + Machine Learning
#25 February 2016

#Create Matrix --------------------------

A <- matrix (c(1.0, -0.7, -0.7, 1.0), nrow = 2, ncol = 2, byrow = TRUE)

A_inv <- solve(A, matrix(c(1,1,1,1), nrow = 2))

test_inverse <- A %*% A_inv

#Matrix Compositions --------------------

A_chol <- chol(A)

A == t(A_chol) %*% A_chol

#Orthogonal Vectors and Matrices --------

x1 <- rnorm(10)
x2 <- rnorm(10)

x1 <- cbind(x1)
x2 <- cbind(x2)

t(x1) %*% x2

X <- cbind(x1, x2)

X_cov <- (t(X) %*% X)/nrow(X) #Check symmetrical
t(X) %*% X == t(t(X) %*% X)

w1 <- cbind(rnorm(10000))
w2 <- cbind(rnorm(10000))

W <- cbind(w1, w2)
W_cov <- (t(W) %*% W)/nrow(W)

#Generating Correlated Random Vectors ---

A_inv_chol = chol(A_inv)
Y <- X %*% A_chol
Y_cov <- (t(Y) %*% Y)/nrow(Y)

Y <- W %*% A_chol
Y_cov <- (t(Y) %*% Y)/nrow(Y)

plot(Y[,1], Y[,2], col = rgb(0, 0, 0, 0.05))

#Scoring --------------------------------

scoreActivity::score253(day = 11)