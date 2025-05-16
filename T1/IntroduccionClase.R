set.seed(12345)

x1 <- rnorm(100, 0.2, 0.2)
x2 <- rnorm(100, 0.5, 0.2)

# Prueba de diferencia de medias clásica

t.test(x1, x2, var.equal = T)

sec_col = rep(c(1,0), c(100,100))

ter_col = rev(sec_col)

X <- cbind(1, sec_col, ter_col)

y <- c(x1, x2)

# beta_est <- solve(t(X) %*% X) %*% t(X) %*% y

# No tienen solución

t(X) %*% y # Calculo de sumas

## modelo 

fit <- lm(y ~ sec_col)
summary(fit)

# Pepsi = A
# cocacola = B
# big cola = C
# D1 = D
