options(java.parameters = c("-Xmx4000m")) #this means 4GB of RAM for YARF
library(YARF)


#(0) demonstrate equality with Liaw's package
library(MASS); data(Boston)
seed = 1984
X = Boston[, 1 : 13]; y = Boston[, 14]

num_trees = 500
set_YARF_num_cores(1)
yarf_mod_cores_1 = YARF(X, y, num_trees = num_trees)
YARF_update_with_oob_results(yarf_mod_cores_1)

library(randomForest)
randomForest(X, y)

data(iris)
yarf_mod_cores_1 = YARF(iris[, 1 : 4], iris[, 5], num_trees = num_trees)
YARF_update_with_oob_results(yarf_mod_cores_1)

library(randomForest)
randomForest(iris[, 1 : 4], iris[, 5])
# we have an elaborate benchmark file that demonstrates YARF's equal performance with Liaw's package
# see benchmarking folder

#(1) demonstrate parallelism and reproducibility with the seed argument
set_YARF_num_cores(4)
yarf_mod_cores_all = YARF(X, y, num_trees = num_trees, seed = seed)

YARF_update_with_oob_results(yarf_mod_cores_1)
YARF_update_with_oob_results(yarf_mod_cores_all)


#(2) demonstrate illustrations

illustrate_trees(yarf_mod_cores_1, trees = c(159), open_file = TRUE, max_depth = 4)

print_at_split_node_script = "function printAtSplitNode(node){
		return '' + node.cost() / node.nodeSize(); //avg cost
    }";
print_at_leaf_script = "function printAtLeaf(node){
		return '' + node.cost() / node.nodeSize(); //avg cost
		}";

illustrate_trees(yarf_mod_cores_1, trees = c(1), max_depth = 4,
		length_in_px_per_half_split = 40,
		print_at_leaf_script = print_at_leaf_script,
		print_at_split_node_script = print_at_split_node_script,
		open_file = TRUE)


#(2) demonstrate asynchronicity / convergence
#### see file test_asynchronicity.R

#(3) demonstrate auto-convergence
#### see file test_convergence.R

#(3) demonstrate missing data
#### NOT DONE YET...

#(4) demonstrate quantile YARF
#### see file test_quantile.R

#(5) demonstrate node information
library(MASS); data(Boston)
X = Boston[, 1 : 13]; y = Boston[, 14]
set_YARF_num_cores(3)
yarf_mod = YARF(X, y, num_trees = 500)
nodes = prediction_nodes(yarf_mod)
length(nodes)
length(nodes[[1]])
nodes[[1]][[169]]
raw = compute_raw_proximity_info(yarf_mod, X[1, ], X[6 : 10, ])
proximity_info = tree_average_proximity_info(raw)
length(proximity_info[[1]])

proximity_info[[1]][[5]]


illustrate_trees(yarf_mod, trees = c(159), open_file = TRUE, max_depth = 5)


