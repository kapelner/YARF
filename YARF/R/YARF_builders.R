#' Builds a YARF Model. There are many customizations available.
#' 
#' @param X 								The data frame of training data
#' @param y 								The vector of training responses which is either numeric (for regression 
#' 											or factor (for classification).
#' @param Xy 								The data frame of training data where the last column is responses
#' @param Xother 							Other data that is used in the training but the RF doesn't split on it
#' @param allow_missingness_in_y			If \code{TRUE}, missingness in the response variable, \code{y}, is allowed. If the missings are not
#' 											handled (somehow) in the custom functions, YARF will crash!! Default is \code{FALSE}.
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
#' 											\code{num_trees} will be used. No warning message will be displayed if you include too many. If 
#' 											this parameter is specified, there is no need to specify \code{n_max_per_tree}.
#' @param n_max_per_tree					An upper limit on the number of observations used to build each tree. If this parameter is specified,
#' 											there is no need to specify \code{bootstrap_indices}. Default is \code{NULL} for the entire dataset, \code{n}.
#' @param other_indices						An optional list with keys 1,2,..., num_trees where each value is indices of the training data
#' 											you wish to use in some custom way for each tree using a custom function. If not custom function
#' 											is specified which makes use of this, it will be ignored. However, indices specified here will not be part of 
#' 											the out-of-bag collection of indices. If this is not your wish, the elements in the vectors
#' 											specified in this list's values should be a subset of those in the values of \code{bootstrap_indices}.
#' 											The default is \code{NULL} indicating you do not wish to specify any "other" data records. 
#' @param mtry 								The number of variables tried at every split. The default is \code{NULL} which indicates
#' 											the out-of-box RF default which is floor(p / 3) for regression and floor(sqrt(p)) for
#' 											classification. If you want to use all possible features, set this parameter to "all".
#' 											If you want a custom function, leave this NULL and see next parameter. 
#' @param mtry_script						A custom javascript function which selects the variables to be greedily searched (see below)
#' 											The default is \code{NULL} which employs the \code{mtry} argument. If you specify your function
#' 											please randomize the order of the returned attributes to arbitrate ties.
#' 
#' 											  function tryVars(node)\{ //node is of type YARF.YARFNode
#' 
#' 											    ...
#' 
#' 											    return int_array //a subset of {0,...,p-1}, indices indicating the variables to perform the exhaustive search on
#' 
#' 											  \}
#' 
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
#' @param serialize 						Should the YARF model be saved? The default is \code{FALSE} as this is costly in processing
#' 											time and memory. This can only be set to \code{TRUE} if \code{wait = TRUE}. If \code{TRUE},
#' 											we will automatically serialize after other operations that add data (such as the OOB evaluation).
#' @param seed								Set a random seed for reproducibility. 
#' @param wait								Should we hang R to wait for the YARF model to complete? The default is \code{TRUE}.
#' @param calculate_oob_error				Should we also calculate the OOB error? Default is \code{TRUE}. Automatically is turned off if
#' 											\code{wait} becomes \code{FALSE}.
#' @param fit_until_convergence				Default is \code{FALSE}. If \code{TRUE}, then the \code{wait} parameter is set to \code{FALSE}
#' 											regardless of the user-specified value. Then, the Random Forest model is fit until "convergence"
#' 											as defined below:  
#' 
#' 											After each tree is completed, the OOB cost is computed. If the cost
#' 											decreases the algorithm does not engage. However, upon the first 
#' 											increase in cost, the model checks for convergence by calculating the average change in cost
#' 											and the standard deviation of change in cost. If a 95\% confidence interval of the average cost is
#' 											within the window created by plus or minus \code{tolerance} (i.e. from zero), then the algorithm has "converged".
#' 											Upon convergence, the model is stopped and a message is printed to the console. Note that specification 
#' 											of the \code{oob_cost_calculation_script} and a proper \code{tolerance} level is essential.
#' 
#' 											It is strongly recommended to set the \code{num_trees} parameter large, otherwise the model can halt before
#' 											it has converged since \code{num_trees} represents an upper limit. If left unspecified, \code{num_trees}
#' 											is set to 10,000 when \code{fit_until_convergence} set to \code{TRUE}.
#' 
#' 											If the user wishes to view the convergence in real time, we recommend the \code{\link{YARF_progress}} 
#' 											function. However, this function will lock the console.
#' 
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
#' 
#' @param tolerance							This parameter is ignored unless \code{fit_until_convergence} is set to \code{TRUE}. If so, this controls the tolerance
#' 											used when assessing the convergence of the RF model. Default is \code{0.1}. If \code{oob_cost_calculation_script} is 
#' 											specified, this should also be specified as 0.1 may not be appropriate for a custom oob cost function.
#' 
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
YARF = function(
		#data arguments
		X = NULL, y = NULL, Xy = NULL, Xother = NULL,
		allow_missingness_in_y = FALSE,
		#pick the trees		
		num_trees = NULL,
		#customizable bootstrap
		bootstrap_indices = NULL, #if you want to specify data indices for the trees
		n_max_per_tree = NULL,
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
		wait = TRUE,
		calculate_oob_error = TRUE,
		fit_until_convergence = FALSE,
		oob_cost_calculation_script = NULL,
		tolerance = 0.01,
		verbose = TRUE,
		debug_log = FALSE
	){
		
	assertDataFrame(X, null.ok = TRUE)
	assertChoice(class(y), c("numeric", "integer", "factor"), null.ok = TRUE)
	assertDataFrame(Xy, null.ok = TRUE)
	assertDataFrame(Xother, null.ok = TRUE)
	assertLogical(allow_missingness_in_y)
	assertCount(num_trees, positive = TRUE, null.ok = TRUE)
	assertList(bootstrap_indices, null.ok = TRUE)
	assertCount(n_max_per_tree, positive = TRUE, null.ok = TRUE)
	#mtry assert handled later
	assertCount(nodesize, positive = TRUE, null.ok = TRUE)
	assertCharacter(mtry_script, null.ok = TRUE)
	assertStringContains(mtry_script, "function tryVars(node){")
	assertCharacter(split_vals_script, null.ok = TRUE)
	assertStringContains(split_vals_script, "function tryVars(node, j){")
	assertCharacter(make_node_to_leaf_script, null.ok = TRUE)
	assertStringContains(make_node_to_leaf_script, "function makeNodeIntoLeaf(node){")
	assertCharacter(cost_single_node_calc_script, null.ok = TRUE)
	assertStringContains(cost_single_node_calc_script, "function nodeCost(node){")
	assertCharacter(cost_both_children_calc_script, null.ok = TRUE)
	assertStringContains(cost_both_children_calc_script, "function totalChildrenCost(leftNode, rightNode){")
	assertCharacter(node_assign_script, null.ok = TRUE)
	assertStringContains(node_assign_script, "function assignYhatToNode(node){")
	assertCharacter(after_node_birth_function_script, null.ok = TRUE)
	assertStringContains(after_node_birth_function_script, "function nodeAfterNodeBirth(node){")
	assertCharacter(shared_scripts, null.ok = TRUE)
	assertLogical(use_missing_data)
	assertLogical(replace_missing_data_with_x_j_bar)
	assertLogical(serialize)
	assertNumeric(seed, null.ok = TRUE)
	assertLogical(wait)
	assertLogical(calculate_oob_error)
	assertLogical(fit_until_convergence)
	assertCharacter(oob_cost_calculation_script, null.ok = TRUE)
	assertStringContains(oob_cost_calculation_script, "function oobCost(y_hat, y){")
	assertNumeric(tolerance, lower = .Machine$double.xmin)
	assertLogical(verbose)
	assertLogical(debug_log)
	
	if (fit_until_convergence){
		wait = FALSE
	}
	
	if (serialize && !wait){
		stop("'serialize' can only by TRUE if 'wait' is TRUE (you cannot save a model that is not yet fully constructed). If you wish to use asynchronous building with \"wait\" you can always run the \"YARF_serialize\" function even during construction.")
	}
	
	if (is.null(num_trees)){
		if (fit_until_convergence){
			num_trees = get("MAX_TREES", YARF_globals)
		} else {
			num_trees = 500
		}
	}
	
	if ((as.integer(num_trees) != num_trees) || num_trees < 1){
		stop("The 'num_trees' argument is not a positive integer.")
	}
	
	if (!is.null(mtry_script)){
		if (class(mtry_script) != "character"){
			stop("'mtry_script' must be a character string of Javascript code")
		}
	}
	
	if (!is.null(make_node_to_leaf_script)){
		if (class(make_node_to_leaf_script) != "character"){
			stop("'make_node_to_leaf_script' must be a character string of Javascript code")
		}
	}
	
	if (!is.null(cost_single_node_calc_script)){
		if (class(cost_single_node_calc_script) != "character"){
			stop("'cost_single_node_calc_script' must be a character string of Javascript code")
		}
	}
		
	if (!is.null(cost_both_children_calc_script)){
		if (class(cost_both_children_calc_script) != "character"){
			stop("'cost_both_children_calc_script' must be a character string of Javascript code")
		}
	}
	
	if (!is.null(node_assign_script)){
		if (class(node_assign_script) != "character"){
			stop("'node_assign_script' must be a character string of Javascript code")
		}
	}
	
	if (!is.null(after_node_birth_function_script)){
		if (class(after_node_birth_function_script) != "character"){
			stop("'after_node_birth_function_script' must be a character string of Javascript code")
		}
	}
	
	if (!is.null(shared_scripts)){
		if (class(shared_scripts) != "character"){
			stop("'shared_scripts' must be a character string of Javascript code")
		}
	}	
	
	if (!is.null(oob_cost_calculation_script)){
		if (class(oob_cost_calculation_script) != "character"){
			stop("'oob_cost_calculation_script' must be a character string of Javascript code")
		}
	}
	
	if (verbose){
		if (fit_until_convergence){
			cat("YARF initializing with as many trees as it needs to converge (but max 10,000)...\n")	
		} else {
			cat("YARF initializing with a fixed", num_trees, "trees...\n")	
		}
		
	}	
	t0 = Sys.time()
	
	if ((is.null(X) && is.null(Xy)) || is.null(y) && is.null(Xy)){
		stop("You need to give YARF a training set either by specifying X and y or by specifying a matrix Xy which contains the response named \"y.\"\n")
	} else if (!is.null(X) && !is.null(y) && !is.null(Xy)){
		stop("You cannot specify both [X,y] and Xy simultaneously.")		
	} else if (is.null(X) && is.null(y)){ #they specified Xy, so now just pull out X,y
		#first ensure it's a dataframe
		if (!("data.frame" %in% class(Xy))){
			stop(paste("The training data Xy must be a data frame."), call. = FALSE)	
		}
		Xy = data.frame(Xy)
		y = Xy[, ncol(Xy)]
		for (cov in 1 : (ncol(Xy) - 1)){
			if (colnames(Xy)[cov] == ""){
				colnames(Xy)[cov] = paste("V", cov, sep = "")
			}
		}
		X = as.data.frame(Xy[, 1 : (ncol(Xy) - 1)])
		colnames(X) = colnames(Xy)[1 : (ncol(Xy) - 1)]
	}
	
	#make sure it's a data frame
	if (!("data.frame" %in% class(X))){
		stop(paste("The training data X must be a data frame."), call. = FALSE)	
	}
	
	#cast it just to make sure (doesn't work with tbl_df's e.g.) but ensure it keeps the same column names. Casting using data.frame 
	#changes "(Intercept)" to "X.Intercept." for some terrrrrrible reason.
	original_col_names = colnames(X)
	X = data.frame(X)
	colnames(X) = original_col_names
	
	#make sure it's a well-formed data frame
	if (ncol(X) == 0){
		stop("Your data matrix must have at least one attribute.")
	}
	if (nrow(X) == 0){
		stop("Your data matrix must have at least one observation.")
	}
	if (length(y) != nrow(X)){
		stop("The number of responses must be equal to the number of observations in the training data.")
	}	
	
	if (!allow_missingness_in_y && (sum(is.na(y)) > 0 || sum(is.null(y)) > 0 || sum(is.nan(y)) > 0)){
		stop("You cannot have any missing data in your response vector unless \"allow_missingness_in_y\" is set to TRUE.")
	}
	
	#convenient to keep around
	n = nrow(X)
	if (!is.null(other_indices)){
		assertSubset(other_indices, 1 : n)
	}
	if (!is.null(nodesize)){
		assertTRUE(nodesize < n)
	}
	
	
	#now take a look at the "other" data
	if (!is.null(Xother)){
		if (class(Xother) != "data.frame"){
			stop(paste("The other data, Xother, must be a data frame."), call. = FALSE)	
		}
		if (nrow(X) != n){
			stop("The other data, Xother, must have the same number of rows as X.")
		}
		if (verbose && nrow(na.omit(Xother)) < nrow(Xother)){
			cat("Warning: There is missing data in 'Xother'.\n")
		}
	}
	
	#now take a look at the bootstrap indices data
	if (is.null(bootstrap_indices)){ #the user wants the standard non-parametric bootstrap sampling with replacement
		bootstrap_indices = list()
		one_to_n = seq(1, n)
		
		if (is.null(n_max_per_tree)){
			num_to_sample = n	
		} else {
			num_to_sample = n_max_per_tree
		}
		
		if (!is.null(seed)){
			#save current RNG state
			current_RNG_state = .Random.seed
			#TEMPORARILY set the seed in R so that the boostrap indices will be the same
			set.seed(seed)
		}
		
		for (t in 1 : num_trees){
#			bootstrap_indices[[t]] = sort(sample(one_to_n, replace = TRUE)) #easier to debug
			bootstrap_indices[[t]] = sample(one_to_n, num_to_sample, replace = TRUE)
		}
		if (!is.null(seed)){
			#pretend like it never happened: return the RNG state to what is just was
			.Random.seed = current_RNG_state
		}
		
		
	} else {
		#ensure the indicies is the correct format
		if (!(class(bootstrap_indices) %in% c("list"))){
			stop("The bootstrap_indicies must be a list")
		}
		for (t in 1 : num_trees){
			indices = bootstrap_indices[[t]]
			if (!isTRUE(all.equal(indices, as.integer(indices))) || any(indices < 1) || any(indices > n)){
				stop("The bootstrap_indicies values must all be vectors whose elements are all in {1,...,n}")
			}
		}
		if (!is.null(n_max_per_tree)){
			warning("Argument \"n_max_per_tree\" ignored since \"bootstrap_indices\" was specified.")
		}
	}

	if (!is.null(other_indices)){
		#ensure the indicies is the correct format
		if (!(class(other_indices) %in% c("list"))){
			stop("The other_indicies must be a list")
		}
		for (t in 1 : num_trees){
			indices = other_indices[[t]]
			if (!isTRUE(all.equal(indices, as.integer(indices))) || any(indices < 1) || any(indices > n)){
				stop("The other_indicies values must all be vectors whose elements are all in {1,...,n}")
			}
		}
	}

	#we are about to construct a YARF Java object. First, let R garbage collect
	#to clean up previous YARF objects that are no longer in use. This is important
	#because R's garbage collection system does not "see" the size of Java objects. Thus,
	#you are at risk of running out of memory without this invocation. 
	gc() #Delete at your own risk!	
	
	#init the java object
	java_YARF = .jnew("YARF.YARF")
	
	if (!is.null(seed)){
		#set the seed in Java
		.jcall(java_YARF, "V", "setSeed", as.integer(seed))
	}

	#now take care of classification or regression
	y_levels = levels(y)
	num_y_levels = length(y_levels)
	if (class(y) == "numeric" || class(y) == "integer"){ #if y is numeric, then it's a regression problem
		pred_type = "regression"
		if (class(y) == "integer"){
			cat("Warning: The response y is integer, YARF will default to regression.\n")
		}
	} else if (class(y) == "factor"){ #if y is a factor and binary
		if (num_y_levels > 5){
			cat("Warning: You are doing classification with more than 5 classes. Cast y to numeric if you wish to do regression.\n")
		}		
		pred_type = "classification"
	} else { #otherwise throw an error
		stop("Your response must be either numeric or a factor.\n")
	}
	
	#java expects doubles
	y = as.numeric(y)
	
	#if no column names, make up names
	if (is.null(colnames(X))){
		colnames(X) = paste("V", seq(from = 1, to = ncol(X), by = 1), sep = "")
	}
	
	#now we should regenerate the factors for the factor columns
	predictors_which_are_factors = names(which(sapply(X, is.factor)))
	for (predictor in predictors_which_are_factors){
		X[, predictor] = factor(X[, predictor])
	}
	if (verbose && length(predictors_which_are_factors) > 0){
		cat("YARF factors created...\n")
	}
	
	#if we're not using missing data, go on and get rid of it
	if (!use_missing_data && !replace_missing_data_with_x_j_bar){
		rows_before = nrow(X)
		X = na.omit(X)
		rows_after = nrow(X)
		if (rows_before - rows_after > 0){
			stop("You have ", rows_before - rows_after, " observations with missing data. \nYou must either omit your missing data using \"na.omit()\" or turn on the\n\"use_missing_data\" or \"replace_missing_data_with_x_j_bar\" feature in order to use YARF.\n")
		}
	} else if (replace_missing_data_with_x_j_bar){
		X = imputeMatrixByXbarjContinuousOrModalForBinary(X, X)
		if (verbose){
			cat("Imputed missing data using attribute averages.\n")
		}
	}
	
	model_matrix_training_data = pre_process_data(X)$data

	p = ncol(model_matrix_training_data) # we subtract one because we tacked on the response as the last column
	


	
	# # now take care of missing data - add each column as a missingness dummy
    # predictor_columns_with_missingness = as.numeric(which(colSums(is.na(data)) > 0))
	#
    # # only do something if there are predictors with missingness
    # if (length(predictor_columns_with_missingness) > 0){
	# 	M = list()
	# 	for (col in predictor_columns_with_missingness) {
	# 		M[[as.character(col)]] = as.numeric(is.missing(model_matrix_training_data[,col]))
	# 	}
    # }

#	factor_lengths = pre_process_obj$factor_lengths
	if (verbose){
		cat("YARF after data preprocessed...", ncol(model_matrix_training_data), "total features...\n")
	}

	#now set whether we want the program to log to a file
	if (debug_log){
		cat("warning: printing out the log file will slow down the runtime significantly.\n")
		.jcall(java_YARF, "V", "writeStdOutToLogFile")
	}


	
	#build YARF to spec with what the user wants
	.jcall(java_YARF, "V", "setNumTrees", as.integer(num_trees))
	.jcall(java_YARF, "V", "setVerbose", verbose)
	.jcall(java_YARF, "V", "setPredType", pred_type)
	
	#now load data and/or scripts
	if (!is.null(mtry)){
		if (mtry == "all"){
			mtry = p
		}
		assertCount(mtry, positive = TRUE)
		assertTRUE(mtry <= p)
		.jcall(java_YARF, "V", "setMTry", as.integer(mtry))
	}
	if (!is.null(mtry_script)) {
		.jcall(java_YARF, "V", "setMtry_function_str", mtry_script)
	}
	
	
	if (!is.null(nodesize)){
		.jcall(java_YARF, "V", "setNodesize", as.integer(nodesize))
	} else { #Breiman defaults...
		.jcall(java_YARF, "V", "setNodesize", as.integer(ifelse(pred_type == "regression", 5, 1)))
	}
	if (!is.null(make_node_to_leaf_script)) {
		.jcall(java_YARF, "V", "setMake_node_into_leaf_function_str", make_node_to_leaf_script)
	}
	
	if (!is.null(split_vals_script)){
		.jcall(java_YARF, "V", "setSplit_values_function_str", split_vals_script)
	}
	
	if (!is.null(cost_single_node_calc_script)){
		.jcall(java_YARF, "V", "setCost_single_node_calc_function_str", cost_single_node_calc_script)
	}
	
	if (!is.null(node_assign_script)){
		.jcall(java_YARF, "V", "setNode_assignment_function_str", node_assign_script)
	}
	
	if (!is.null(cost_both_children_calc_script)){
		.jcall(java_YARF, "V", "setCost_both_children_calc_function_str", cost_both_children_calc_script)
	}
	
	if (!is.null(after_node_birth_function_script)){
		.jcall(java_YARF, "V", "setAfter_node_birth_function_str", after_node_birth_function_script)
	}
	
	if (!is.null(shared_scripts)){
		.jcall(java_YARF, "V", "setShared_scripts_str", shared_scripts)
	}
	
	if (!is.null(oob_cost_calculation_script)){
		.jcall(java_YARF, "V", "setOob_cost_calculation_str", oob_cost_calculation_script)
	}
	
	#now load the training data into YARF
	for (i in 1 : n){
		row_as_char = as.character(model_matrix_training_data[i, ]) #unfortunately I couldn't get the data setting in Java to work without casting. At least it's only a one time O(np) cost!
		row_as_char = replace(row_as_char, is.na(row_as_char), "NA") #this seems to be necessary for some R-rJava-linux distro-Java combinations
		.jcall(java_YARF, "V", "addTrainingDataRow", row_as_char)
	}
	.jcall(java_YARF, "V", "addTrainingDataResponse", y)

	# for (col in predictor_columns_with_missingness) {
	# 	.jcall(java_YARF, "V", "addMissingnessDummy", as.integer(col), M[[as.character(col)]])
	# }

	.jcall(java_YARF, "V", "finalizeTrainingData")
	.jcall(java_YARF, "V", "setTrainingDataNames", colnames(model_matrix_training_data))
	
	if (!is.null(Xother)){
		.jcall(java_YARF, "V", "setOtherDataNames", .jarray(as.character(colnames(Xother))))
		#now load the "other" data into YARF
		for (i in 1 : n){
			row_as_char = as.character(Xother[i, ])
			row_as_char = replace(row_as_char, is.na(row_as_char), "NA") #this seems to be necessary for some R-rJava-linux distro-Java combinations
			.jcall(java_YARF, "V", "addOtherDataRow", .jarray(row_as_char))
		}
	}
	
	#now load the bootstrap indices into YARF
	for (t in 1 : num_trees){
		.jcall(java_YARF, "V", "addBootstrapIndices", as.integer(bootstrap_indices[[t]] - 1), as.integer(t - 1)) ##Java is minus 1 from R's indexing on all fronts
	}
	#and the "other" indices if specified
	if (!is.null(other_indices)){
		for (t in 1 : num_trees){
			.jcall(java_YARF, "V", "addOtherIndices", as.integer(other_indices[[t]] - 1), as.integer(t - 1)) ##Java is minus 1 from R's indexing on all fronts
		}
	}

	
	
	#do we want to do the YARF model building asynchronously?
	.jcall(java_YARF, "V", "setWait", wait)
	if (fit_until_convergence){
		.jcall(java_YARF, "V", "stopAtConvergence")
		.jcall(java_YARF, "V", "setTolerance", as.numeric(tolerance))
	}
	
	#build the YARF model and let the user know what type of model this is
	if (verbose){
		cat("Beginning YARF", pred_type, "model construction...")
	}
	#load the number of cores the user set (default is declared in zzz.R)
	num_cores = get("YARF_NUM_CORES", YARF_globals)
	.jcall(java_YARF, "V", "Build", as.integer(num_cores)) #Finally get it built
	if (verbose){
		cat("done.\n")
	}
	
	yarf_mod = list(
		X = X,
		y = y,
		Xother = Xother,
		allow_missingness_in_y = allow_missingness_in_y,		
		num_trees = num_trees,
		bootstrap_indices = bootstrap_indices,
		other_indices = other_indices,
		mtry = mtry,
		nodesize = nodesize,
		mtry_script = mtry_script,
		make_node_to_leaf_script = make_node_to_leaf_script,
		cost_single_node_calc_script = cost_single_node_calc_script,
		node_assign_script = node_assign_script,
		after_node_birth_function_script = after_node_birth_function_script,
		shared_scripts = shared_scripts, 
		use_missing_data = use_missing_data,
		replace_missing_data_with_x_j_bar = replace_missing_data_with_x_j_bar,
		serialize = serialize,
		seed = seed,
		wait = wait,
		verbose = verbose,
		debug_log = debug_log,
		pred_type = pred_type,
		t0 = t0,
		java_YARF = java_YARF,
		y_levels = y_levels,	
		num_y_levels = num_y_levels,
		n = n,
		p = p,
		model_matrix_training_data = model_matrix_training_data,
		training_data_features = colnames(model_matrix_training_data),
		predictors_which_are_factors = predictors_which_are_factors,
		validation_test_indices = sample(1 : n),
		fit_until_convergence = fit_until_convergence,
		oob_cost_calculation_script = oob_cost_calculation_script,
		tolerance = tolerance
	)
	
	#Let's serialize the object if the user wishes
	if (serialize){
		YARF_serialize(yarf_mod)
	}
	
	#use R's S3 object orientation for convenience
	class(yarf_mod) = "YARF"
	
	if (calculate_oob_error && wait){
		if (verbose){
			cat("Calculating OOB error...")
		}		
		yarf_mod = YARF_update_with_oob_results(yarf_mod, oob_cost_calculation_script = oob_cost_calculation_script)
		if (verbose){
			cat("done.\n")
		}
	}
	yarf_mod
}


#' Sets the number of cores for YARF to use for all operations
#' which are multithreaded (e.g. model construction, prediction, etc)
#' 
#' @param num_cores 			The number of cores to use
#' 
#' @author Adam Kapelner
#' @export
set_YARF_num_cores = function(num_cores){
	assertCount(num_cores, positive = TRUE)
	
	assign("YARF_NUM_CORES", num_cores, YARF_globals)
	if (num_cores == 1){
		cat("YARF is now making use of one core. Are you sure?\n")
	} else {
		cat("YARF can now make use of", num_cores, "cores.\n")
	}
	
}

#' Serializes the model so the user can use \code{save} and \code{save.image}
#' to write it to a file that can be then loaded into another and/or future R session.
#' 
#' @param yarf_mod 								The yarf model object
#' 
#' @author Adam Kapelner
#' @export
YARF_serialize = function(yarf_mod){
	assertClass(yarf_mod, "YARF")
	
	cat("serializing so that the YARF model could potentially be saved and transported to future R sessions...")
	.jcache(yarf_mod$java_YARF)
	cat("done\n")	
}


