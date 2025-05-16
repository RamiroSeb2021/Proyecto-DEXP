xx <- matrix(c(4,7,14,7,15,30,14,30,60), ncol = 3, byrow = T)
solve(matrix(c(4,7,0,
               7,15,0,
               0,0,0), ncol = 3, byrow = T))
A1 <- solve(matrix(c(4,7,7,15), ncol=2, byrow = T))

lambda <- c(1,0,0)

A2 <- matrix(c(60/44, 0 , -14/44, 0,0,0, -14/44, 0, 4/44), ncol = 3, byrow = T)

X <- matrix(c(1,1,2,1,2,4,1,1,2,1,3,6), ncol = 3, byrow = T)

lambda%*%A2%*%t(X)
8/11

