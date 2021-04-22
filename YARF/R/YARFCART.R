#' Fit a CART model
#' 
#' A convenience method to build a CART model via YARF. There are many customizations available.
#' 
#' @inheritParams YARF
#' @inherit YARF return 
#' @author Adam Kapelner
#' @export
YARFCART = function(
		#data arguments
		X = NULL, y = NULL, Xy = NULL, Xother = NULL,
		allow_missingness_in_y = FALSE,
		#customizable bootstrap
		bootstrap_indices = NULL, #if you want to specify data indices for the trees
		other_indices = NULL, #other indices you pass to the tree which will NOT be included in the OOB
		mtry = NULL,
		nodesize = NULL,
		#all custom scripts/functions
		mtry_script = NULL,
		split_vals_script = NULL,
		make_node_to_leaf_script = NULL,
		cost_single_node_calc_script = NULL,
		cost_both_children_calc_script = NULL,
		node_assign_script = NULL,
		after_node_birth_function_script = NULL,
		shared_scripts = NULL, 
		#everything that has to do with possible missing values (MIA stuff)
		use_missing_data = TRUE,
		replace_missing_data_with_x_j_bar = FALSE,
		#other arguments for Java
		serialize = FALSE,
		seed = NULL,
		calculate_oob_error = TRUE,
		oob_cost_calculation_script = NULL,
		verbose = TRUE,
		debug_log = FALSE
){
    if (is.null(bootstrap_indices)) {
	    bootstrap_indices = list()
	    bootstrap_indices[[1]] = 1 : nrow(X)
    }
	YARF(
			#data arguments
			X = X, y = y, Xother = Xother,
			allow_missingness_in_y = allow_missingness_in_y,
			#pick the trees		
			num_trees = 1, #here's where the action happens
			#customizable bootstrap
			bootstrap_indices = bootstrap_indices, #if you want to specify data indices for the trees
			other_indices = other_indices, #other indices you pass to the tree which will NOT be included in the OOB
			mtry = "all",
			nodesize = nodesize,
			#all custom scripts/functions
			mtry_script = mtry_script,
			split_vals_script = split_vals_script,
			make_node_to_leaf_script = make_node_to_leaf_script,
			cost_single_node_calc_script = cost_single_node_calc_script,
			node_assign_script = node_assign_script,
			after_node_birth_function_script = after_node_birth_function_script,
			shared_scripts = shared_scripts, 
			#everything that has to do with possible missing values (MIA stuff)
			use_missing_data = use_missing_data,
			replace_missing_data_with_x_j_bar = replace_missing_data_with_x_j_bar,
			#other arguments for Java
			serialize = serialize,
			seed = seed,
			wait = TRUE,
			calculate_oob_error = calculate_oob_error,
			oob_cost_calculation_script = oob_cost_calculation_script,
			verbose = verbose,
			debug_log = debug_log
		)
}

#' Create a single tree stump
#' 
#' A convenience method to build a stump of a tree model via YARF. There are many customizations available.
#' 
#' @inheritParams YARF
#' @inherit YARF return 
#' @author Adam Kapelner
#' @export
YARFCARTSTUMP = function(
		#data arguments
		X = NULL, y = NULL, Xy = NULL, Xother = NULL,
		allow_missingness_in_y = FALSE,
		#customizable bootstrap
		bootstrap_indices = NULL, #if you want to specify data indices for the trees
		other_indices = NULL, #other indices you pass to the tree which will NOT be included in the OOB
		mtry = NULL,
		#all custom scripts/functions
		mtry_script = NULL,
		split_vals_script = NULL,
		make_node_to_leaf_script = NULL,
		cost_single_node_calc_script = NULL,
		cost_both_children_calc_script = NULL,
		node_assign_script = NULL,
		after_node_birth_function_script = NULL,
		shared_scripts = NULL, 
		#everything that has to do with possible missing values (MIA stuff)
		use_missing_data = TRUE,
		replace_missing_data_with_x_j_bar = FALSE,
		#other arguments for Java
		serialize = FALSE,
		seed = NULL,
		calculate_oob_error = TRUE,
		oob_cost_calculation_script = NULL,
		verbose = TRUE,
		debug_log = FALSE
){
	if (is.null(bootstrap_indices)) {
		bootstrap_indices = list()
		bootstrap_indices[[1]] = 1 : nrow(X)
	}
	YARF(
		#data arguments
		X = X, y = y, Xother = Xother,
		allow_missingness_in_y = allow_missingness_in_y,
		#pick the trees		
		num_trees = 1, #here's where the action happens
		#customizable bootstrap
		bootstrap_indices = bootstrap_indices, #if you want to specify data indices for the trees
		other_indices = other_indices, #other indices you pass to the tree which will NOT be included in the OOB
		mtry = "all",
		nodesize = nrow(X) + 1, #here's where the action happens
		#all custom scripts/functions
		mtry_script = mtry_script,
		split_vals_script = split_vals_script,
		make_node_to_leaf_script = make_node_to_leaf_script,
		cost_single_node_calc_script = cost_single_node_calc_script,
		node_assign_script = node_assign_script,
		after_node_birth_function_script = after_node_birth_function_script,
		shared_scripts = shared_scripts, 
		#everything that has to do with possible missing values (MIA stuff)
		use_missing_data = use_missing_data,
		replace_missing_data_with_x_j_bar = replace_missing_data_with_x_j_bar,
		#other arguments for Java
		serialize = serialize,
		seed = seed,
		wait = TRUE,
		calculate_oob_error = calculate_oob_error,
		oob_cost_calculation_script = oob_cost_calculation_script,
		verbose = verbose,
		debug_log = debug_log
	)
}