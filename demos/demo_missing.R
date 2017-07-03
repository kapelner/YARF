options(java.parameters = c("-Xmx4000m"))
library(YARF)
set_YARF_num_cores(7)

seed = 1776
set.seed(seed)
n = 300
x1 = runif(n, 0, 4)
x2 = runif(n, 0, 4)
eps = rnorm(n, 0, 0.2)
y = x1 + 2 * x2 + eps

yarf_mod = YARF(data.frame(x1, x2), y, seed = seed)

xstar = data.frame(x1 = 1, x2 = 1)
xstar = rbind(xstar, c(1, NA))
xstar = rbind(xstar, c(NA, NA))
predict(yarf_mod, xstar)
#############################





library(MASS); data(Boston)
#YARF/RF on BHD
X = Boston[, 1 : 13]; y = Boston[, 14]
yarf_mod = YARF(X, y, num_trees = 1000)

xstar = X[50, , drop = FALSE]
predict(yarf_mod, xstar)
xstar
summary(X$rm)
xstar$rm = NA
illustrate_trees(yarf_mod, trees = c(1), open_file = TRUE, max_depth = 4)
predict(yarf_mod, xstar)


## missingness models for X next time...