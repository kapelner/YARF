options(java.parameters = c("-Xmx4000m"))
library(YARF)

n = 3000
p = 30
sigma = 0.5
beta_0 = 3
beta_1 = 4
p = ncol(X)
X = data.frame(matrix(runif(n * p), nrow = n))
y = beta_0 + beta_1*X[,1] + rnorm(n, 0, sigma)
plot(X[,1], y)
abline(a = beta_0, b = beta_1, col = "gray", lwd = 3)

# ##create a bare minimum working examples
library(MASS); data(Boston)
seed = 1984
X = Boston[, 1 : 13]; y = Boston[, 14]

# X[,1] = rnorm(nrow(X), 0, sigma)
# X[,4:13] = NULL
# p = ncol(X)
# y = beta_0 + beta_1*X[,1] + rnorm(nrow(X), 0, sigma)
plot(X[,1], y)

seed = 1984

num_trees = 500
set_YARF_num_cores(1)
yarf_mod_cores_1 = YARF(X, y, num_trees = num_trees, seed = seed)
set_YARF_num_cores(7)
yarf_mod_cores_all = YARF(X, y, num_trees = num_trees, seed = seed)

YARF_update_with_oob_results(yarf_mod_cores_1)
YARF_update_with_oob_results(yarf_mod_cores_all)


illustrate_trees(yarf_mod_cores_1, open_file = TRUE, trees = 1, max_depth = 4)
illustrate_trees(yarf_mod_cores_all, open_file = TRUE, trees = 1, max_depth = 4)


