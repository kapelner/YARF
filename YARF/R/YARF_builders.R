#' Builds a YARF Model. There are many custom functions
#' 
#' @param X 								The data frame of training data
#' @param y 								The training responses
#' @param Xy 								The data frame of training data where the last column is responses
#' @param Xother 							Other data that is used in the training but the RF doesn't split on it
#' @param allow_missingness_in_y			If \code{TRUE}, missingness in the response variable, \code{y}, is allowed. If this is not
#' 											handled in the custom functions, YARF will crash. Default is \code{FALSE}.
#' @param num_trees 						The # of trees in the RF. Default is \code{500}.
#' @param boostrap_indices 					An n x num_trees matrix of indices where each column is the bootstrap indices of the training data.
#' 											The default is \code{NULL} indicating the default algorithm of sampling {1,...,n} with replacement.	
#' @param mtry 								The number of variables tried at every split. The default is \code{NULL} which indicates
#' 											the out-of-box RF default which is floor(p / 3) for regression and for classification,
#' 											floor(sqrt(p)). If you want a custom function, leave this NULL and see next parameter. 
#' @param mtry_fun							If you wish to create a custom number of mtry, pass in javascript code here as a string.
#' @param nodesize							The minimum number of observations in a node. YARF will stop splitting at this point.
#' 											If \code{NULL} the out-of-the-box default of 5 for regression and 1 for classification 
#' 											will be used.
#' @param nodesize_fun						A custom javascript function to be used to calculate nodesize. The default is \code{NULL} where
#' 											nodesize will be calculated as a static constant (see the \code{nodesize} argument).				
#' @param cost_single_node_calc_fun			A custom cost calculation for a potential node (when considering a split) in Javascript. 
#' 											The default is \code{NULL} which means the out-of-the-box default of sum of squared error relative
#' 											to the sample average (if regression) and sum of entropy (if classification). You may find it 
#' 											convenient to also made a node assignment here. If so, make sure you specify the node assignment
#' 											function as a blank function (not \code{NULL}).
#' @param cost_both_children_calc_fun		A custom cost calculation for an entire split considering both the putative left and right children
#' 											nodes. The default is \code{NULL} which means the out-of-the-box default for Random Forests which is
#' 											sum of left and right nodes' costs for regression and average of left and right nodes' cost (relative
#' 											to the number of observations in each node).
#' 
#' 											which defaults to sum of squared error for regression or sum of entropy for classification.							 
#' @param node_assign_fun					A custom node assignment function in Javascript. This function is run after RF greedily finds the 
#' 											"lowest cost" split. The default is \code{NULL} corresponding to the sample average of the node responses 
#' 											in regression or the modal class during classification. 
#' @param aggregation_fun					A custom javascript function which aggregates the predictions in the trees for one observations 
#' 											into one scalar prediction. The default is \code{NULL} corresponding to the sample average for
#' 											regression and the modal category for classification.
#' @param shared_functions					Custom Javascript functions that are always around. The default is \code{NULL} for no shared functions. 
#' @param use_missing_data					Use the "missing-in-attributes" to fit data with missingness. 	
#' @param covariates_to_permute 			Indices of features to randomly permute when creating a YARF. The default is \code{NULL}
#' 											indicating no features are permuted. This is an argument used mostly by other YARF functions.
#' @param mem_cache_for_speed 
#' @param serialize 						Should the YARF model be saved? The default is \code{FALSE} as this is costly in processing 
#' 											time and memory. This can only be set to \code{TRUE} if \code{wait = TRUE}. If \code{TRUE},
#' 											we will automatically serialize after other operations that add data (such as the OOB evaluation).
#' @param seed								Set a random seed for reproducibility. 
#' @param wait								Should we hang R to wait for the YARF model to complete? The default is \code{TRUE}.
#' @param verbose 							Should we print out messages verbosely during construction? Default is \code{FALSE}.
#' @param debug_log							Should we print out messages from Java? Default is \code{FALSE}.
#' 
#' @return									A list of all arguments passed in plus... 
#' 
#' @author Adam Kapelner
#' @export
YARF = function(
		#data arguments
		X = NULL, y = NULL, Xy = NULL, Xother = NULL,
		allow_missingness_in_y = FALSE,
		#pick the trees		
		num_trees = 500,
		#customizable bootstrap
		boostrap_indices = NULL, #if you want to write your own bootstrapper for the trees, send a n x T matrix of indices here
		#mtry or a custom function
		mtry = NULL,
		mtry_fun = NULL,
		#nodesize or a custom function
		nodesize = NULL,
		nodesize_fun = NULL,
		#a custom fuction for the cost calculation
		cost_calc_fun = NULL,
		#a custom function for the cost calculation
		node_assign_fun = NULL,
		#a custom function for aggregating results of trees
		aggregation_fun = NULL,
		#any helper code which will be accessible to code above
		shared_funs = NULL, 
		#everything that has to do with possible missing values (MIA stuff)
		use_missing_data = FALSE,
		replace_missing_data_with_x_j_bar = TRUE,
		#other arguments
		mem_cache_for_speed = TRUE,
		covariates_to_permute = NULL, #PRIVATE
		serialize = FALSE,
		seed = NULL,
		wait = TRUE,
		verbose = TRUE,
		debug_log = FALSE){
	
	if (serialize && !wait){
		stop("'serialize' can only by TRUE if 'wait' is TRUE (you cannot save a model that is not yet fully constructed).")
	}
	
	if ((as.integer(num_trees) != num_trees) || num_trees < 1){
		stop("The 'num_trees' argument is not a positive integer.")
	}
	
	if (!is.null(mtry_fun)){
		if (class(mtry_fun) != "character"){
			stop("'mtry_function' must be a character string of Javascript code")
		}
	}
	
	if (!is.null(nodesize_fun)){
		if (class(nodesize_fun) != "character"){
			stop("'nodesize' must be a character string of Javascript code")
		}
	}
	
	if (!is.null(cost_calc_fun)){
		if (class(cost_calc_fun) != "character"){
			stop("'cost_calc_fun' must be a character string of Javascript code")
		}
	}
	
	if (!is.null(node_assign_fun)){
		if (class(node_assign_fun) != "character"){
			stop("'node_assign_fun' must be a character string of Javascript code")
		}
	}
	
	if (!is.null(aggregation_fun)){
		if (class(aggregation_fun) != "character"){
			stop("'aggregation_fun' must be a character string of Javascript code")
		}
	}
	
	
	
	if (!is.null(shared_funs)){
		if (class(shared_funs) != "character"){
			stop("'shared_funs' must be a character string of Javascript code")
		}
	}
	
	
	
	if (verbose){
		cat("YARF initializing with", num_trees, "trees...\n")	
	}	
	t0 = Sys.time()
	
	if ((is.null(X) && is.null(Xy)) || is.null(y) && is.null(Xy)){
		stop("You need to give YARF a training set either by specifying X and y or by specifying a matrix Xy which contains the response named \"y.\"\n")
	} else if (!is.null(X) && !is.null(y) && !is.null(Xy)){
		stop("You cannot specify both X,y and Xy simultaneously.")		
	} else if (is.null(X) && is.null(y)){ #they specified Xy, so now just pull out X,y
		#first ensure it's a dataframe
		if (class(Xy) != "data.frame"){
			stop(paste("The training data Xy must be a data frame."), call. = FALSE)	
		}
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
	if (class(X) != "data.frame"){
		stop(paste("The training data X must be a data frame."), call. = FALSE)	
	}
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
	if (is.null(boostrap_indices)){ #the user wants the standard non-parametric bootstrap sampling with replacement
		bootstrap_indices = matrix(NA, n, num_trees)
		one_to_n = seq(1, n)
		for (t in 1 : num_trees){
			bootstrap_indices[, t] = sample(one_to_n)
		}
	} else {
		#ensure the indicies is the correct format
		if (class(bootstrap_indices) %notin% c("data.frame", "matrix")){
			stop("The bootstrap_indicies must be a data.frame or matrix")
		}
		if (!all.equal(dim(bootstrap_indices), c(n, num_trees))){
			stop("The bootstrap_indicies must be n x num_trees")
		}
		if (!(sum(apply(bootstrap_indices, 1, as.integer) == apply(bootstrap_indices, 1, function(x){x})) != (n * num_trees))){
			stop("The bootstrap_indicies must be integers")
		}
		if (sum(bootstrap_indices > n) + sum(bootstrap_indices < 1) > 0){
			stop("The bootstrap_indicies elements must all be in {1,...,n}")
		}
	}
	if (verbose){
		cat("YARF data input checked...\n")
	}	
	#we are about to construct a YARF object. First, let R garbage collect
	#to clean up previous YARF objects that are no longer in use. This is important
	#because R's garbage collection system does not "see" the size of Java objects. Thus,
	#you are at risk of running out of memory without this invocation. 
	gc() #Delete at your own risk!	
	
	#init the java object
	java_YARF = .jnew("YARF.YARF")

	#now take care of classification or regression
	y_levels = levels(y)
	num_y_levels = length(y_levels)
	if (class(y) == "numeric" || class(y) == "integer"){ #if y is numeric, then it's a regression problem
		pred_type = "regression"
		if (class(y) == "integer"){
			cat("Warning: The response y is integer, YARF will default to regression.\n")
		}
	} else if (class(y) == "factor"){ #if y is a factor and binary
		if (num_y_levels > 8){
			cat("Warning: You are doing classificaiton with more than 8 classes. Cast y to numeric if you wish to do regression.")
		}		
		pred_type = "classification"
	} else { #otherwise throw an error
		stop("Your response must be either numeric or a factor.\n")
	}
	
	#java expects doubles
	y = as.numeric(y)

	if (verbose){
		cat("YARF java init...\n")
	}
	
	#if no column names, make up names
	if (is.null(colnames(X))){
		colnames(X) = paste("V", seq(from = 1, to = ncol(X), by = 1), sep = "")
	}
	
	#now we should regenerate the factors for the factor columns
	predictors_which_are_factors = names(which(sapply(X, is.factor)))
	for (predictor in predictors_which_are_factors){
		X[, predictor] = factor(X[, predictor])
	}
	if (verbose){
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
	if (verbose){
		cat("YARF before preprocess...\n")
	}
	
	pre_process_obj = pre_process_training_data(X)
	model_matrix_training_data = pre_process_obj$data
	p = ncol(model_matrix_training_data) # we subtract one because we tacked on the response as the last column
#	factor_lengths = pre_process_obj$factor_lengths
	if (verbose){
		cat("YARF after preprocess...", ncol(model_matrix_training_data), "total features...\n")
	}

#	#this is a private parameter ONLY called by cov_importance_test
#	if (!is.null(covariates_to_permute)){
#		#first check if these covariates are even in the matrix to begin with
#		for (cov in covariates_to_permute){
#			if (!(cov %in% colnames(model_matrix_training_data)) && class(cov) == "character"){
#				stop("Covariate \"", cov, "\" not found in design matrix.")
#			}
#		}
#		permuted_order = sample(1 : nrow(model_matrix_training_data), nrow(model_matrix_training_data))
#		model_matrix_training_data[, covariates_to_permute] = model_matrix_training_data[permuted_order, covariates_to_permute]
#	}
	
	#now set whether we want the program to log to a file
	if (debug_log & verbose){
		cat("warning: printing out the log file will slow down the runtime significantly.\n")
		.jcall(java_YARF, "V", "writeStdOutToLogFile")
	}

	#if the user hasn't set a number of cores, set it here
	if (!exists("YARF_NUM_CORES", envir = YARF_globals)){
		assign("YARF_NUM_CORES", YARF_NUM_CORES_DEFAULT, YARF_globals)
	}
	#load the number of cores the user set
	num_cores = get("YARF_NUM_CORES", YARF_globals)
	
	#build YARF to spec with what the user wants
	.jcall(java_YARF, "V", "setNumCores", as.integer(num_cores)) #this must be set FIRST!!!
	.jcall(java_YARF, "V", "setNumTrees", as.integer(num_trees))
	.jcall(java_YARF, "V", "setVerbose", verbose)
	.jcall(java_YARF, "V", "setMemCacheForSpeed", mem_cache_for_speed)
	.jcall(java_YARF, "V", "setPredType", pred_type)
	
	#now load data and/or scripts
	if (!is.null(mtry)){
		.jcall(java_YARF, "V", "setMTry", as.integer(mtry))
	} else if (!is.null(mtry_fun)) {
		.jcall(java_YARF, "V", "setMTryFunction", mtry_fun)
	}
	if (!is.null(nodesize)){
		.jcall(java_YARF, "V", "setNodesize", as.integer(nodesize))
	} else if (!is.null(nodesize_fun)) {
		.jcall(java_YARF, "V", "setNodesizeFunction", nodesize_fun)
	}
	
	if (!is.null(cost_calc_fun)){
		.jcall(java_YARF, "V", "setCostCalcFunction", cost_calc_fun)
	}
	
	if (!is.null(node_assign_fun)){
		.jcall(java_YARF, "V", "setNodeAssignmentFunction", node_assign_fun)
	}
	
	if (!is.null(aggregation_fun)){
		.jcall(java_YARF, "V", "setAggregationFunction", aggregation_fun)
	}
	
	if (!is.null(shared_funs)){
		.jcall(java_YARF, "V", "setSharedFunctions", shared_funs)
	}
	
	
	
	if (!is.null(seed)){
		#set the seed in R
		set.seed(seed)
		#set the seed in Java
		.jcall(java_YARF, "V", "setSeed", as.integer(seed))
	}
	
	#now load the training data into YARF
	for (i in 1 : n){
		row_as_char = as.character(model_matrix_training_data[i, ])
		row_as_char = replace(row_as_char, is.na(row_as_char), "NA") #this seems to be necessary for some R-rJava-linux distro-Java combinations
		.jcall(java_YARF, "V", "addTrainingDataRow", row_as_char)
	}
	.jcall(java_YARF, "V", "addTrainingDataResponse", y)
	.jcall(java_YARF, "V", "finalizeTrainingData")
	if (verbose){
		cat("YARF training data finalized...\n")
	}
	
	.jcall(java_YARF, "V", "setTrainingDataNames", colnames(model_matrix_training_data))
	
	if (!is.null(Xother)){
		.jcall(java_YARF, "V", "setOtherDataNames", as.character(colnames(Xother)))
		#now load the "other" data into YARF
		for (i in 1 : n){
			row_as_char = as.character(Xother[i, ])
			row_as_char = replace(row_as_char, is.na(row_as_char), "NA") #this seems to be necessary for some R-rJava-linux distro-Java combinations
			.jcall(java_YARF, "V", "addOtherDataRow", row_as_char)
		}
		if (verbose){
			cat("YARF 'other' data finalized...\n")
		}
	}
	
	#now load the bootstrap indices into YARF
	for (t in 1 : num_trees){
		.jcall(java_YARF, "V", "addBootstrapIndices", as.integer(bootstrap_indices[, t] - 1), as.integer(t - 1)) ##Java is minus 1 from R's indexing
	}
	.jcall(java_YARF, "V", "initTrees") #immediately follows
	
	
	#build the YARF model and let the user know what type of model this is
	if (verbose){
		cat("Now building YARF for", pred_type, "...")
		if (use_missing_data){
			cat("Missing data feature ON. ")
		}
		cat("\n")
	}
	.jcall(java_YARF, "V", "setWait", wait)
	.jcall(java_YARF, "V", "Build")
	
	yarf_mod = list(
		allow_missingness_in_y = allow_missingness_in_y,
		num_trees = num_trees,
		boostrap_indices = boostrap_indices, 
		mtry = mtry,
		mtry_fun = mtry_fun,
		nodesize = nodesize,
		nodesize_fun = nodesize_fun,
		cost_calc_fun = cost_calc_fun,
		node_assign_fun = node_assign_fun,
		aggregation_fun = aggregation_fun,
		shared_funs = shared_funs, 
		use_missing_data = use_missing_data,
		replace_missing_data_with_x_j_bar = replace_missing_data_with_x_j_bar,
		mem_cache_for_speed = mem_cache_for_speed,
		serialize = serialize,
		seed = seed,
		wait = wait,
		verbose = verbose,
		debug_log = debug_log,
		X = X,
		y = y,
		pred_type = pred_type,
		t0 = t0,
		java_YARF = java_YARF,
		y_levels = y_levels,	
		num_y_levels = num_y_levels,
		n = n,
		p = p,
		model_matrix_training_data = model_matrix_training_data,
		training_data_features = colnames(model_matrix_training_data),
		predictors_which_are_factors = predictors_which_are_factors
	)

	
	
	#Let's serialize the object if the user wishes
	if (serialize){
		YARF_serialize(yarf_mod)
	}
	
	#use R's S3 object orientation
	class(yarf_mod) = "YARF"
	yarf_mod
}

#' Computes the out-of-bag (OOB) predictions for the training data. 
#' This gives a good sense of out-of-sample performance in the future.
#' 
#' @param yarf_mod 							The yarf model object
#' @param oob_metric						If regression, we will return L1, MAE, L2 and RMSE and if classification,
#' 											we will return the confusion matrix with use/test errors and overall error rates. 
#' 											If you wish for another metric to be computed, pass the function in here as a string of 
#' 											javascript code. Default is \code{NULL} for no additional metric to be computed.		
#' @return 									The OOB results
#' 
#' @author Adam Kapelner
#' @export
YARF_update_with_oob_results = function(yarf_mod, oob_metric = NULL){
		y = yarf_mod$y
		n = yarf_mod$n
		#get it from java multithreaded
		num_cores = as.integer(get("YARF_NUM_CORES", YARF_globals))
		y_oob = t(sapply(.jcall(yarf_mod$java_YARF, "[[D", "evaluateOutOfBagEstimates", num_cores), .jevalArray))
		
		if (is.null(y_oob)){
			stop("OOB estimates cannot be computed since not every observation existed out of bag. Use more trees next time.")
		}
		
		#return a bunch more stuff
		if (yarf_mod$pred_type == "regression"){
			yarf_mod$y_oob = y_oob
			yarf_mod$residuals = y - y_oob
			yarf_mod$L1_err_oob = sum(abs(yarf_mod$residuals))
			yarf_mod$L2_err_oob = sum(yarf_mod$residuals^2)
			yarf_mod$PseudoRsqoob = 1 - yarf_mod$L2_err_oob / sum((y - mean(y))^2)
			yarf_mod$rmse_oob = sqrt(yarf_mod$L2_err_oob / n)
			yarf_mod$mae_oob = yarf_mod$L1_err_oob / n
		} else {		
			#convert results to factor
			yarf_mod$y_oob = factor(y_oob, levels = yarf_mod$y_levels)
			
			#calculate confusion matrix
			n_levels = yarf_mod$num_y_levels
			
			confusion_matrix = as.data.frame(matrix(NA, nrow = n_levels + 1, ncol = n_levels + 1))
			rownames(confusion_matrix) = c(paste("actual", yarf_mod$y_levels), "use errors")
			colnames(confusion_matrix) = c(paste("predicted", yarf_mod$y_levels), "model errors")
			
			#set the confusion counts
			confusion_matrix[1 : n_levels, 1 : n_levels] = as.integer(table(y, y_oob))
			#set all test errors
			for (k in 1 : n_levels){
				confusion_matrix[k, n_levels + 1] = 1 - confusion_matrix[k, k] / sum(confusion_matrix[k, 1 : n_levels])
			}
			#set all use errors
			for (k in 1 : n_levels){
				confusion_matrix[n_levels + 1, k] = 1 - confusion_matrix[k, k] / sum(confusion_matrix[1 : n_levels, k])
			}
			#set overall error
			yarf_mod$misclassification_error = (sum(confusion_matrix[1 : n_levels, 1 : n_levels]) - diag(confusion_matrix[1 : n_levels, 1 : n_levels])) / n
			confusion_matrix[n_levels + 1, n_levels + 1] = yarf_mod$misclassification_error
			#return the whole thing
			yarf_mod$confusion_matrix = confusion_matrix
		}
		if (yarf_mod$serialize){ #if they did it before, do it now too
			YARF_serialize(yarf_mod)
		}
		#send it back
		yarf_mod
}

#' Prints out a message reflecting the progress of the YARF model construction
#' 
#' @param yarf_mod 							The yarf model object
#' @param console_message					Should we print a message to console? Default is \code{TRUE}.
#' @return 									The number of trees and the proportion completed.
#' 
#' @author Kapelner
#' @export
YARF_progress = function(yarf_mod, console_message = TRUE){
	if (.jcall(yarf_mod$java_YARF, "Z", "stopped")){
		stop("Construction of this model was halted.")
	}
	
	num_trees_completed = .jcall(yarf_mod$java_YARF, "I", "progress")
	progress = num_trees_completed / yarf_mod$num_trees 
	
	time_remaining_estimate = NULL
	
	if (progress < 1){
		time_elapsed_in_min = (Sys.time() - as.numeric(yarf_mod$t0)) / 60
	} else {
		time_elapsed_in_min = (.jcall(yarf_mod$java_YARF, "J", "getCompletionTime") / 1000 - as.numeric(yarf_mod$t0)) / 60
	}
	
	#now estimate how long it will take to complete
	if (num_trees_completed >= 1 & progress < 1){
		total_time_estimate = time_elapsed_in_min / progress
		time_remaining_estimate = total_time_estimate - time_elapsed_in_min
	}
	
	if (console_message){
		cat(num_trees_completed, " / ", yarf_mod$num_trees, " trees completed (", round(progress * 100, 1), "% done in ", round(time_elapsed_in_min, 1), " minutes)", sep = "")	
		
		if (num_trees_completed >= 1 && progress < 1){
			total_time_estimate = time_elapsed_in_min / progress
			time_remaining_estimate = total_time_estimate - time_elapsed_in_min
			cat("We estimate the YARF model will complete in ", round(time_elapsed_in_min, 1), " minutes)", sep = "")
		} else if (progress < 1) {
			cat("No time estimate for completion until the first tree is constructed.\n")
		}
	}	
	invisible(list(
		num_trees_completed = num_trees_completed, 
		progress = progress,
		done = (progress == 1),
		time_elapsed_in_min = time_elapsed_in_min,
		time_remaining_estimate = time_remaining_estimate
	))
}

#' Prints out a messages reflecting the progress of the YARF model construction until completion
#' 
#' @param yarf_mod 							The yarf model object
#' @param time_delay_in_seconds				Frequency of messages in seconds. Default is \code{10} seconds.
#' 
#' @author Kapelner
#' @export
YARF_progress_reports = function(yarf_mod, time_delay_in_seconds = 10){
	while (TRUE){
		if (YARF_progress(yarf_mod)$done){
			break
		}
		Sys.sleep(time_delay_in_seconds)
	}
}

#' Halts the model building.
#' 
#' @param yarf_mod 								The yarf model object
#' 
#' @author Adam Kapelner
#' @export
YARF_stop = function(yarf_mod){
	yarf_mod$stopped = TRUE
	.jcall(yarf_mod$java_YARF, "V", "StopBuilding")
}

#' Serializes the model so the user can use \code{save} and \code{save.image}
#' to write it to a file that can be then loaded into another and/or future R session.
#' 
#' @param yarf_mod 								The yarf model object
#' 
#' @author Adam Kapelner
#' @export
YARF_serialize = function(yarf_mod){
	cat("serializing in order to be saved for future R sessions...")
	.jcache(yarf_mod$java_YARF)
	cat("done\n")	
}

