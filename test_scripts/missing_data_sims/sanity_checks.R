options(java.parameters = c("-Xmx4000m"))
library(YARF)
set_YARF_num_cores(7)
seed = 1984

n = 600
p = 1
sigma = 0.5
beta_0 = 3
beta_1 = 4
X = data.frame(matrix(runif(n * p), nrow = n))
colnames(X)[1] = "x"
y = beta_0 + beta_1*X[,1] + rnorm(n, 0, sigma)
plot(X[,1], y)
abline(a = beta_0, b = beta_1, col = "gray", lwd = 3)
p = ncol(X)



yarf_mod = YARF(X, y, num_trees = 500, seed = seed)
YARF_update_with_oob_results(yarf_mod)

xs = seq(0, 1, by = 0.001)
Xstar = data.frame(xs)
colnames(Xstar)[1] = "x"
points(xs, predict(yarf_mod, Xstar), col = "blue", type = "l")

predict(yarf_mod, 0.5)
