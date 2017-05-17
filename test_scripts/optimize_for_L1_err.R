
options(java.parameters = c("-Xmx4000m")); library(YARF); library(MASS); data(Boston)
X = Boston[, 1 : 13]; y = Boston[, 14]

#vanilla RF
yarf_mod_vanilla = YARF(X, y, num_trees = 500)
YARF_update_with_oob_results(yarf_mod_vanilla)

#load the node assign as the median value
node_assign_script = read_file("scr02.js")
shared_scripts = read_file("scr04.js")

yarf_mod = YARF(X, y, num_trees = 500, 
                node_assign_script = node_assign_script,
                shared_scripts = shared_scripts)
YARF_update_with_oob_results(yarf_mod)

#note same results


#predict(yarf_mod, X)

#now let's go even crazier... let's make the cost function sum of absolute errors (instead of squared errors)
cost_single_node_calc_script = read_file("scr03.js")


yarf_mod = YARF(X, y, num_trees = 500,
                cost_single_node_calc_script = cost_single_node_calc_script,
                shared_scripts = shared_scripts)
yarf_mod
YARF_update_with_oob_results(yarf_mod)


#now let's go even crazier... let's aggregate across the tree predictions by median
aggregation_script = " 
function aggregateYhatsIntoOneYhat(y_hat_trees, yarf){ //yarf unused
  return median(Java.from(y_hat_trees));
}"


yarf_mod = YARF(X, y, num_trees = 500, 
                node_assign_script = node_assign_script,
                cost_single_node_calc_script = cost_single_node_calc_script,
                shared_scripts = shared_scripts, 
                aggregation_script = aggregation_script)
yarf_mod
YARF_update_with_oob_results(yarf_mod)

###INTERESTING!!!!
yarf_mod = YARF(X, y, num_trees = 500,
                shared_scripts = shared_scripts, 
                aggregation_script = aggregation_script)
yarf_mod
YARF_update_with_oob_results(yarf_mod)