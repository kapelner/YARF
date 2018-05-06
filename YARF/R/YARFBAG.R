#' A convenience method to build a Bagged Tree model via YARF. 
#' 
#' You can conceptualize this as a random forest without the feature that de-correlates the trees, namely
#' trying random subsets of variables at each split. Here, all splits are considered. You can also think about
#' this as model averaging many CART trees built from bootstrap samples (or specific samples you choose).
#' 
#' @param X 								The data frame of training data
#' @param y 								The vector of training responses
#' @param Xy 								The data frame of training data where the last column is responses
#' @param Xother 							Other data that is used in the training but the RF doesn't split on it
#' @param allow_missingness_in_y			If \code{TRUE}, missingness in the response variable, \code{y}, is allowed. If this is not
#' 											handled in the custom functions, YARF will crash. Default is \code{FALSE}.
#' @param num_trees 						The number of trees in the RF. Default is \code{NULL} which sets the value to \code{500} 
#' 											if \code{fit_until_convergence} is set to \code{FALSE} and 10,000 if \code{fit_until_convergence} 
#' 											is set to \code{TRUE}.
#' 											
#' 											If the model is fit asynchronously via the \code{wait} parameter
#' 											being set to \code{TRUE}, this number represents a vague contract between the user and the 
#' 											software as the model fitting can be (a) halted by the user or the model can be (b) set 
#' 											to converge automatically (by setting the \code{fit_until_convergence} parameter
#' 											to \code{TRUE}). In the latter case, this parameter functions as the maximum number of trees
#' 											that can be created and thus, it is recommended that this number is very large, hence the default
#' 											of 10,000.
#' 
#' @param bootstrap_indices 				A list with keys 1, 2, ..., num_trees where each value is the indices of the training data
#' 											you wish to use for each tree. The default is \code{NULL} indicating the default algorithm 
#' 											of sampling {1,...,n} with replacement (i.e. the non-parametric bootsrap default). Needless
#' 											to say indices specified here will not be part of the out-of-bag collection of indices.	You may
#' 											specify more than enough (i.e. more than \code{num_trees}) elements in this list. Only the first
#' 											\code{num_trees} will be used. No warning message will be displayed if you include too many.
#' @param other_indices						An optional list with keys 1,2,..., num_trees where each value is indices of the training data
#' 											you wish to use in some custom way for each tree using a custom function. If not custom function
#' 											is specified which makes use of this, it will be ignored. However, indices specified here will not be part of 
#' 											the out-of-bag collection of indices. If this is not your wish, the elements in the vectors
#' 											specified in this list's values should be a subset of those in the values of \code{bootstrap_indices}.
#' 											The default is \code{NULL} indicating you do not wish to specify any "other" data records. 
#' @param split_vals_script					A custom javascript function which selects the split values to be greedily searched in feature j.
#' 											The default is \code{NULL} which employs the midpoints of all sorted values.
#' 
#' 											  function tryVals(node, j)\{ //node is of type YARF.YARFNode and j is the feature number in {0,...,p-1}
#' 
#' 											    ...
#' 
#' 											    return double_array //a vector of split vals to greedily assess
#' 
#' 											  \}
#' 
#' @param nodesize							The minimum number of observations in a node. YARF will stop splitting at this point.
#' 											If \code{NULL} the out-of-the-box default of 5 for regression and 1 for classification 
#' 											will be used.
#' @param make_node_to_leaf_script			A custom javascript function to be used to calculate nodesize (see below).
#' 											The default is \code{NULL} where nodesize will be calculated as a static constant (see the 
#' 											\code{nodesize} argument).
#' 
#' 											  function makeNodeIntoLeaf(node)\{ //node is of type YARF.YARFNode
#' 
#' 											    ...
#' 
#' 											    return boolean //where true makes this node into a leaf
#' 
#' 											  \}
#' 			
#' @param cost_single_node_calc_script		A custom cost calculation for a potential node (when considering a split) in Javascript (see below). 
#' 											The default is \code{NULL} which means the out-of-the-box default of sum of squared error relative
#' 											to the sample average (if regression) and sum of entropy (if classification). You may find it 
#' 											convenient to also made a node assignment here. If so, make sure you specify the node assignment
#' 											function as a blank function (not \code{NULL}).
#' 
#' 											  function nodeCost(node)\{ //node is of type YARF.YARFNode
#' 
#' 											    ...
#' 
#' 											    return double //where a higher number indicates a higher cost
#' 
#' 											  \}
#' 
#' @param cost_both_children_calc_script	A custom cost calculation in Javascript for an entire split considering both the putative left and right children
#' 											nodes (see below). The default is \code{NULL} which means the out-of-the-box default for Random Forests which is
#' 											sum of left and right nodes' costs for regression and average of left and right nodes' cost (relative
#' 											to the number of observations in each node).
#' 
#' 											  function totalChildrenCost(leftNode, rightNode)\{ //both nodes are of type YARF.YARFNode
#' 
#' 											    ...
#' 
#' 											    return double //where a higher number indicates a higher cost
#' 
#' 											  \}
#' 
#' 
#' @param node_assign_script				A custom node assignment function in Javascript (see below). This function is run after RF greedily finds the 
#' 											"lowest cost" split. The default is \code{NULL} corresponding to the sample average of the node responses 
#' 											in regression or the modal class during classification. 
#' 
#' 											  function assignYhatToNode(node)\{ //node is of type YARF.YARFNode
#' 
#' 											    ...
#' 
#' 											    return double //assigned as this node's predicted value ("y_hat")
#' 
#' 											  \}
#' 
#' @param after_node_birth_function_script	A custom function in Javascript which is executed after a node is given birth to. The default is 
#' 											\code{NULL} which implies nothing special is done, the Random Forest default. This is particularly
#' 											useful to record extra information in the node (e.g. by writing a hash to the \code{other_info} 
#' 											field in the node).
#' 
#' 											  function nodeAfterNodeBirth(node)\{ //node is of type YARF.YARFNode
#' 
#' 											    ...
#' 
#' 											  \}
#' 
#' @param shared_scripts					Custom Javascript code that are always in scope when running all your custom methods. 
#' 											The default is \code{NULL} for no shared scripts. 
#' @param use_missing_data					Use the "missing-incorporated-in-attributes" strategy to fit data with missingness. The 
#' 											default is \code{TRUE}.	
#' @param use_missing_data_dummies_as_vars	For each predictor in the dataset that is missing, create a new variable \code{M_<name>} that
#' 											is one for observations that are missing and 0 if not. The default is \code{TRUE}.	
#' @param serialize 						Should the YARF model be saved? The default is \code{FALSE} as this is costly in processing 
#' 											time and memory. This can only be set to \code{TRUE} if \code{wait = TRUE}. If \code{TRUE},
#' 											we will automatically serialize after other operations that add data (such as the OOB evaluation).
#' @param seed								Set a random seed for reproducibility. 
#' @param calculate_oob_error				Should we also calculate the OOB error? Default is \code{TRUE}. Automatically is turned off if
#' 											\code{wait} becomes \code{FALSE}.
#' @param oob_cost_calculation_script		This parameter will determine the out-of-bag cost of the RF forest model. Default is \code{NULL}. If
#' 											it is \code{NULL}, then the cost will be 1-R^2 for regression models and misclassification error for classification 
#' 											models.
#' 
#' 											If non-null, an optional custom Javascript function which calculates the cost of a prediction given the true
#' 											value of the prediction (see below). If is likely similar to \code{cost_single_node_calc_script}. It
#' 											is recommended to share code between them by writing a function included in the \code{shared_scripts} argument  
#' 											which can be referenced when calculating OOB results.
#' 
#' 											  function oobCost(y_hat, y)\{ //y_hat is the predicted value and y is the true value (both are of type double)
#' 
#' 											    ...
#' 
#' 											    return double //where a larger number indicates a higher cost to the error between y and y_hat.
#' 
#' 											  \}
#' 
#' 											If this script is specified (i.e. the argument is non-\code{NULL}), this will also affect the output of the 
#' 											\code{\link{YARF_update_with_oob_results}} function. This script can be reset after the model is built if necessary.
#' @param tolerance							This parameter is ignored unless \code{fit_until_convergence} is set to \code{TRUE}. If so, this controls the tolerance
#' 											used when assessing the convergence of the RF model. Default is \code{0.1}. If \code{oob_cost_calculation_script} is 
#' 											specified, this should also be specified as 0.1 may not be appropriate for a custom oob cost function.
#' 
#' @param wait								Should we hang R to wait for the YARF model to complete? The default is \code{TRUE}.
#' @param verbose 							Should we print out messages verbosely during construction? Default is \code{FALSE}.
#' @param debug_log							Should we print out messages from Java? Default is \code{FALSE}.
#' 
#' @return									A list that (a) reiterates all ob the above arguments passed in and (b) \code{pred_type} is the 
#' 											guess as to modeling type: regression or classification (c) \code{java_YARF} is the Java object
#' 											(d) \code{y_levels} is the unique values of the response variable (e) \code{model_matrix_training_data}
#' 											is the data matrix with factor variables converted to dummies that is fed into YARF as the X variables
#' 											(f) \code{training_data_features} is the names of the features (g) \code{predictors_which_are_factors}
#' 											is a list of the variables that are factors and (h) \code{n} and \code{p} which are the dimensions of
#' 											the training data model matrix and (i) various other convenient data and internal data of use to 
#' 											other functions in this package.
#' 
#' @author Adam Kapelner
#' @export
YARFBAG = function(
		#data arguments
		X = NULL, y = NULL, Xy = NULL, Xother = NULL,
		allow_missingness_in_y = FALSE,
		#pick the trees
		num_trees = NULL,
		#customizable bootstrap
		bootstrap_indices = NULL, #if you want to specify data indices for the trees
		other_indices = NULL, #other indices you pass to the tree which will NOT be included in the OOB
		nodesize = NULL,
		#all custom scripts/functions
		mtry_script = NULL,
		split_vals_script = NULL,
		make_node_to_leaf_script = NULL,
		cost_single_node_calc_script = NULL,
		node_assign_script = NULL,
		after_node_birth_function_script = NULL,
		shared_scripts = NULL, 
		#everything that has to do with possible missing values (MIA stuff)
		use_missing_data = TRUE,
		use_missing_data_dummies_as_vars = TRUE,
		replace_missing_data_with_x_j_bar = FALSE,
		#other arguments for Java
		serialize = FALSE,
		seed = NULL,
		wait = TRUE,
		calculate_oob_error = TRUE,
		fit_until_convergence = FALSE,
		oob_cost_calculation_script = NULL,
		tolerance = 0.01,
		verbose = TRUE,
		debug_log = FALSE
){
	YARF(
		#data arguments
		X = X, y = y, Xother = Xother,
		allow_missingness_in_y = allow_missingness_in_y,
		#pick the trees		
		num_trees = num_trees,
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
		use_missing_data_dummies_as_vars = use_missing_data_dummies_as_vars,
		replace_missing_data_with_x_j_bar = replace_missing_data_with_x_j_bar,
		#other arguments for Java
		serialize = serialize,
		seed = seed,
		wait = wait,
		calculate_oob_error = calculate_oob_error,
		fit_until_convergence = fit_until_convergence,
		oob_cost_calculation_script = oob_cost_calculation_script,
		tolerance = tolerance,
		verbose = verbose,
		debug_log = debug_log
	)
}