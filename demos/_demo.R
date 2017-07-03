options(java.parameters = c("-Xmx4000m")) #this means 4GB of RAM for YARF
library(YARF)
library(randomForest)

#(0) demonstrate equality with Liaw's package
library(MASS); data(iris); data(Boston)
seed = 1984
num_trees = 500
set_YARF_num_cores(1)

#YARF/RF on Iris
randomForest(iris[, 1 : 4], iris[, 5])
YARF(iris[, 1 : 4], iris[, 5], num_trees = num_trees)

#YARF/RF on BHD
X = Boston[, 1 : 13]; y = Boston[, 14]
randomForest(X, y)
yarf_mod_cores_1 = YARF(X, y, num_trees = num_trees, seed = seed)
yarf_mod_cores_1


# we have an elaborate benchmark file that demonstrates YARF's equal performance with Liaw's package
# see benchmarking folder

#(1) demonstrate parallelism and reproducibility with the seed argument
set_YARF_num_cores(7)
yarf_mod_cores_all = YARF(X, y, num_trees = num_trees, seed = seed)
yarf_mod_cores_all


#(2) demonstrate illustrations

illustrate_trees(yarf_mod_cores_1, trees = c(159), open_file = TRUE, max_depth = 4)

print_at_split_node_script = "function printAtSplitNode(node){
		return '' + Math.round(node.cost() / node.nodeSize() * 100) / 100; //avg cost
    }";
print_at_leaf_script = "function printAtLeaf(node){
		return '' + Math.round(node.cost() / node.nodeSize() * 100) / 100; //avg cost
		}";

illustrate_trees(yarf_mod_cores_1, trees = c(159), max_depth = 4,
		length_in_px_per_half_split = 40,
		print_at_leaf_script = print_at_leaf_script,
		print_at_split_node_script = print_at_split_node_script,
		open_file = TRUE)



























#(2) demonstrate asynchronicity / convergence

#(3) demonstrate auto-convergence

#(4) demonstrate missing data

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


