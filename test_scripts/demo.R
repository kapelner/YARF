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
# we have an elaborate benchmark file that demonstrates YARF's equal performance with Liaw's package

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


#(2) demonstrate asynchronicity
options(java.parameters = c("-Xmx4000m")) #this means 4GB of RAM for YARF
library(YARF)
set_YARF_num_cores(3)
n = 8000
p = 10
X = matrix(rnorm(n * p), nrow = n)
beta = as.matrix(rnorm(p))
y_expe = as.numeric(X %*% beta)
y = y_expe + rnorm(n)
X = data.frame(X)

#build a model asynchronously by setting wait to FALSE
yarf_mod = YARF(X, y, num_trees = 5000, wait = FALSE)
#the model initializes and upon the beginning of tree construction,
#returns you to the R prompt while it works as a background process
yarf_mod
#shows that it is in the process of being built
#you can also check to make sure the regression is correct
head(yarf_mod$model_matrix_training_data) # among other things...

#at any time, you can see the performance by computing the oob
#on the subset of trees that have been built. Here, you can see
#if there are major problems and cut your losses
YARF_update_with_oob_results(yarf_mod)
#If you would like to wait until the model is built, you
#can set up a process to notify you about the progress which
#will return to the prompt when the model is completed
#and you can also see a illustration of the convergence
YARF_progress_reports(yarf_mod, time_delay_in_seconds = 4, plot_oob_error = TRUE, trail = 30)
#you can "stop" this function at any time without stopping the
#model construction if you would like to return to the console

#if you really want to halt construction and free the CPU, you can
#use the stop function
YARF_stop(yarf_mod)
#at this point you can still query the model but if you want
#to remove it and return the memory,
rm(yarf_mod)
gc()

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


