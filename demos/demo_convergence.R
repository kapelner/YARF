options(java.parameters = c("-Xmx4000m"))
library(YARF)
library(MASS); data(Boston)

X = Boston[, 1 : 13]; y = Boston[, 14]

set_YARF_num_cores(7)
yarf_mod = YARF(X, y, fit_until_convergence = TRUE, tolerance = 0.0001)

YARF_convergence(yarf_mod)

yarf_mod
YARF_update_with_oob_results(yarf_mod)

oob_cost_by_iteration = .jcall(yarf_mod$java_YARF, "[D", "OOBCostsByIteration")

moe = s / sqrt(length(diffs))
c(xbar - moe, xbar + moe)
plot(1 : length(diffs), diffs, type = "l")
YARF_stop(yarf_mod)

yarf_mod = YARF(X, y)
yarf_mod
