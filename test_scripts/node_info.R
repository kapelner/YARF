options(java.parameters = c("-Xmx4000m"))
library(YARF)
num_cores = 2
set_YARF_num_cores(num_cores)
library(MASS)
data(Boston)

X = Boston[, 1 : 13]
y = Boston[, 14]

yarf_mod = YARF(X, y, num_trees = 10, wait = TRUE)

# nodes = prediction_nodes(yarf_mod)
# raw = compute_raw_proximity_info(yarf_mod)

raw = compute_raw_proximity_info(yarf_mod, X[1:5, ], X[6 : 10, ])
tree_average_proximity_info(raw)
