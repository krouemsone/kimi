y <- c(1, 2, 3)
x <- c(0, 1, 2)
X <- cbind(1, x)

# 初始值（按笔记要求）
mu  <- y
eta <- log(mu)
z   <- eta
W   <- diag(mu)

# 第一次迭代 (IWLS)
beta1 <- solve(t(X) %*% W %*% X) %*% (t(X) %*% W %*% z)
eta1  <- X %*% beta1
mu1   <- exp(eta1)
z1    <- eta1 + (y - mu1) / mu1
W1    <- diag(as.numeric(mu1))

# 第二次迭代
beta2 <- solve(t(X) %*% W1 %*% X) %*% (t(X) %*% W1 %*% z1)

# 用 glm 对比验证
fit <- glm(y ~ x, family = poisson)
coef(fit); summary(fit)
