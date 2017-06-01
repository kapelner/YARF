options(java.parameters = c("-Xmx4000m"))
library(YARF)
num_cores = 2
set_YARF_num_cores(num_cores)
library(MASS)
data(Boston)

X = Boston[, 1 : 13]
y = Boston[, 14]

yarf_mod = YARF(X, y, num_trees = 10, wait = TRUE)

raw = compute_raw_proximity_info(yarf_mod, X[1:5, ], X[6 : 10, ])
tree_average_proximity_info(raw)

## New proximity_info function ##
shared_scripts = 'function sample_avg(arr){
    var sum = 0.0;
    for (i = 0; i < arr.length; i++){
        sum += arr[i];
    }
    return sum / arr.length;
}'

prox_script = paste("
    function proxNodeCost(node){
       var ys = Java.from(node.node_ys());
       var y_avg = sample_avg(ys);
       var sum_err = 0.0;
       for (i = 0; i < ys.length; i++){
        sum_err += Math.pow(Math.abs(ys[i] - y_avg), ", 2, ");
       }
       return sum_err;
    }
  ", sep = "")

shared_initial_substring = function(s1, s2){
	for (i in 1 : nchar(s1)){
		if (identical(substr(s1, 1, i), substr(s2, 1, i))){
			next
		} else {
			i = i - 1
			break
		}		
	}
	substr(s1, 1, i)
}

# return info that can be used to build proximity values later
yarf_mod = YARF(X, y, num_trees = 100, mtry=1, shared_scripts=shared_scripts)
out = proximity_info(yarf_mod, X, X, prox_single_node_calc_script=prox_script)
shared_initial_substring(out$X1$path[1,1], out$X2$path[1,1])

## JUNKME ##
set.seed(123)
source('../../ProbabilityEstimation/Code/models.R')
train = oned_model(5e2, 10)
test = oned_model(1e2, 10)

X = as.data.frame(train$X)
y = as.factor(train$y)

boot_ind = lapply(1:100, function(x) 1:500)

yarf_mod = YARF(X, y, num_trees = 100, mtry=1, shared_scripts=shared_scripts, nodesize=1,
    bootstrap_indices=boot_ind, calculate_oob_error = FALSE)

out = proximity_info(yarf_mod, X, X, prox_single_node_calc_script=prox_script)

out$X1$path[3,5]
out$X1$vals[[3]][[5]]
library(dplyr)
matt = data.frame(path=as.character(out$X1$path[,1]), sign=as.character(y))
matt %>% arrange(path)

get_tree_num_nodes_leaves_max_depths(yarf_mod)
apply(out$X1$path, 2, function(x) max(nchar(x)))
apply(out$X1$path, 2, function(x) length(unique(x)))
