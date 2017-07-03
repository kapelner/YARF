options(java.parameters = c("-Xmx4000m"))
library(YARF)
set_YARF_num_cores(1)

library(PTE)
data(continuous_example)
X = data.frame(continuous_example$X)
y = continuous_example$y
tx = data.frame(tx = X$treatment)
X$treatment = NULL

yarf_mod_vanilla = YARF(X, y, num_trees = 500)
yarf_mod_vanilla

yarf_mod_personalized = YARF(X, y, num_trees = 500, nodesize = 100, Xother = tx,
                             node_assign_script = read_file("js/null_assignment.js"),
                             cost_single_node_calc_script = read_file("js/personalized_medicine_cost.js"))
#still working on this...


