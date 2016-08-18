YARF_MAX_MEM_MB_DEFAULT = 1100 #1.1GB is the most a 32bit machine can give without throwing an error or crashing
YARF_NUM_CORES_DEFAULT = 1 #Stay conservative as a default

##build a BART model
YARF = function(
		#data arguments
		X = NULL, y = NULL, Xy = NULL,
		mtry = "
			function(X_node, y_node, n, p, is_classification){
				
				if (is_classification)
			}
		",
		num_trees = 500, 
		oob_estimates = TRUE,
		#everything that has to do with possible missing values
		use_missing_data = FALSE,
		covariates_to_permute = NULL, #PRIVATE
		use_missing_data_dummies_as_covars = FALSE,
		impute_missingness_with_x_j_bar_for_lm = TRUE,
		#other arguments
		mem_cache_for_speed = TRUE,
		serialize = FALSE,
		seed = NULL,
		verbose = TRUE){
	if (verbose){
		cat("YARF initializing with", num_trees, "trees...\n")	
	}	
	t0 = Sys.time()
	
	if (use_missing_data_dummies_as_covars && replace_missing_data_with_x_j_bar){
		stop("You cannot impute by averages and use missing data as dummies simultaneously.")
	}
	
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
	if (verbose){
		cat("YARF data input checked...\n")
	}	
	#we are about to construct a YARF object. First, let R garbage collect
	#to clean up previous YARF objects that are no longer in use. This is important
	#because R's garbage collection system does not "see" the size of Java objects. Thus,
	#you are at risk of running out of memory without this invocation. 
	gc() #Delete at your own risk!	

	#now take care of classification or regression
	y_levels = levels(y)
	num_levels = length(y_levels)
	if (class(y) == "numeric" || class(y) == "integer"){ #if y is numeric, then it's a regression problem
		#java expects doubles, not ints, so we need to cast this now to avoid errors later
		if (class(y) == "integer"){
			y = as.numeric(y)
		}		
		java_YARF = .jnew("bartMachine.bartMachineRegressionMultThread")
		y_remaining = y
		pred_type = "regression"
		if (class(y) == "integer"){
			cat("Warning: The response y is integer, bartMachine will run regression.\n")
		}
	} else if (class(y) == "factor"){ #if y is a factor and binary
		java_YARF = .jnew("bartMachine.bartMachineClassificationMultThread")
		y_remaining = ifelse(y == y_levels[1], 1, 0)
		pred_type = "classification"
	} else { #otherwise throw an error
		stop("Your response must be either numeric, an integer or a factor.\n")
	}

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
	
	if (sum(is.na(y_remaining)) > 0){
		stop("You cannot have any missing data in your response vector.")
	}
	
	#if we're not using missing data, go on and get rid of it
	if (!use_missing_data && !replace_missing_data_with_x_j_bar){
		rows_before = nrow(X)
		X = na.omit(X)
		rows_after = nrow(X)
		if (rows_before - rows_after > 0){
			stop("You have ", rows_before - rows_after, " observations with missing data. \nYou must either omit your missing data using \"na.omit()\" or turn on the\n\"use_missing_data\" or \"replace_missing_data_with_x_j_bar\" feature in order to use bartMachine.\n")
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
	
	pre_process_obj = pre_process_training_data(X, use_missing_data_dummies_as_covars)
	model_matrix_training_data = cbind(pre_process_obj$data, y_remaining)
	p = ncol(model_matrix_training_data) - 1 # we subtract one because we tacked on the response as the last column
	factor_lengths = pre_process_obj$factor_lengths
	if (verbose){
		cat("YARF after preprocess...", ncol(model_matrix_training_data), "total features...\n")
	}

	#this is a private parameter ONLY called by cov_importance_test
	if (!is.null(covariates_to_permute)){
		#first check if these covariates are even in the matrix to begin with
		for (cov in covariates_to_permute){
			if (!(cov %in% colnames(model_matrix_training_data)) && class(cov) == "character"){
				stop("Covariate \"", cov, "\" not found in design matrix.")
			}
		}
		permuted_order = sample(1 : nrow(model_matrix_training_data), nrow(model_matrix_training_data))
		model_matrix_training_data[, covariates_to_permute] = model_matrix_training_data[permuted_order, covariates_to_permute]
	}
	
	#now set whether we want the program to log to a file
	if (debug_log & verbose){
		cat("warning: printing out the log file will slow down the runtime significantly.\n")
		.jcall(java_YARF, "V", "writeStdOutToLogFile")
	}
	

	
	#if the user hasn't set a number of cores, set it here
	if (!exists("YARF_NUM_CORES", envir = bartMachine_globals)){
		assign("YARF_NUM_CORES", YARF_NUM_CORES_DEFAULT, bartMachine_globals)
	}
	#load the number of cores the user set
	num_cores = get("YARF_NUM_CORES", bartMachine_globals)
	
	#build bart to spec with what the user wants
	.jcall(java_YARF, "V", "setNumCores", as.integer(num_cores)) #this must be set FIRST!!!
	.jcall(java_YARF, "V", "setNumTrees", as.integer(num_trees))
	.jcall(java_YARF, "V", "setVerbose", verbose)
	.jcall(java_YARF, "V", "setMemCacheForSpeed", mem_cache_for_speed)
	
	if (!is.null(seed)){
		#set the seed in R
		set.seed(seed)
		#set the seed in Java
		.jcall(java_YARF, "V", "setSeed", as.integer(seed))
	}
	
	#now load the training data into BART
	for (i in 1 : nrow(model_matrix_training_data)){
		row_as_char = as.character(model_matrix_training_data[i, ])
		row_as_char = replace(row_as_char, is.na(row_as_char), "NA") #this seems to be necessary for some R-rJava-linux distro-Java combinations
		.jcall(java_YARF, "V", "addTrainingDataRow", row_as_char)
	}
	.jcall(java_YARF, "V", "finalizeTrainingData")
	if (verbose){
		cat("YARF training data finalized...\n")
	}
	
	#build the bart machine and let the user know what type of BART this is
	if (verbose){
		cat("Now building YARF for", pred_type, "...")
		if (use_missing_data){
			cat("Missing data feature ON. ")
		}
		if (use_missing_data_dummies_as_covars){
			cat("Missingness used as covariates. ")
		}
		cat("\n")
	}
	.jcall(java_YARF, "V", "Build")
	
	#now once it's done, let's extract things that are related to diagnosing the build of the BART model
	
	l = as.list(match.call())
	l[[1]] = NULL
	yarf_mod = c(l, 
			time_to_build = Sys.time() - t0,
			y_levels = y_levels,			
			n = nrow(model_matrix_training_data),
			p = p,
			model_matrix_training_data = model_matrix_training_data,
			training_data_features = colnames(model_matrix_training_data)[1 : ifelse(use_missing_data && use_missing_data_dummies_as_covars, (p / 2), p)],
			training_data_features_with_missing_features = colnames(model_matrix_training_data)[1 : p], #always return this even if there's no missing features
			
	)
	
	#once its done gibbs sampling, see how the training data does if user wants
	if (oob_estimates){
		if (verbose){
			cat("evaluating in sample data oob...")
		}
#			y_hat_train = t(sapply(.jcall(bart_machine$java_bart_machine, "[[D", "getGibbsSamplesForPrediction", .jarray(model_matrix_training_data, dispatch = TRUE), as.integer(num_cores)), .jevalArray))
		
		#return a bunch more stuff
		yarf_mod$y_hat_train = y_hat_train
		yarf_mod$residuals = y_remaining - y_hat_train
		yarf_mod$L1_err_train = sum(abs(yarf_mod$residuals))
		yarf_mod$L2_err_train = sum(yarf_mod$residuals^2)
		yarf_mod$PseudoRsq = 1 - yarf_mod$L2_err_train / sum((y_remaining - mean(y_remaining))^2) #pseudo R^2 acc'd to our dicussion with Ed and Shane
		yarf_mod$rmse_train = sqrt(yarf_mod$L2_err_train / yarf_mod$n)
		yarf_mod$mae = yarf_mod$L1_err_train / yarf_mod$n
		
		if (pred_type == "classification"){
#			p_hat_train =	t(sapply(.jcall(yarf_mod$java_bart_machine, "[[D", "getGibbsSamplesForPrediction", .jarray(model_matrix_training_data, dispatch = TRUE), as.integer(num_cores)), .jevalArray))
			yarf_mod$y_hat_train = labels_to_y_levels(y_hat_train)
			
			#return a bunch more stuff
			yarf_mod$p_hat_train = p_hat_train
			
			#calculate confusion matrix
			confusion_matrix = as.data.frame(matrix(NA, nrow = 3, ncol = 3))
			rownames(confusion_matrix) = c(paste("actual", y_levels), "use errors")
			colnames(confusion_matrix) = c(paste("predicted", y_levels), "model errors")
			
			confusion_matrix[1 : 2, 1 : 2] = as.integer(table(y_remaining, y_hat_train)) 
			confusion_matrix[3, 1] = round(confusion_matrix[2, 1] / (confusion_matrix[1, 1] + confusion_matrix[2, 1]), 3)
			confusion_matrix[3, 2] = round(confusion_matrix[1, 2] / (confusion_matrix[1, 2] + confusion_matrix[2, 2]), 3)
			confusion_matrix[1, 3] = round(confusion_matrix[1, 2] / (confusion_matrix[1, 1] + confusion_matrix[1, 2]), 3)
			confusion_matrix[2, 3] = round(confusion_matrix[2, 1] / (confusion_matrix[2, 1] + confusion_matrix[2, 2]), 3)
			confusion_matrix[3, 3] = round((confusion_matrix[1, 2] + confusion_matrix[2, 1]) / sum(confusion_matrix[1 : 2, 1 : 2]), 3)
			
			yarf_mod$confusion_matrix = confusion_matrix
#			bart_machine$num_classification_errors = confusion_matrix[1, 2] + confusion_matrix[2, 1]
			yarf_mod$misclassification_error = confusion_matrix[3, 3]
		}
		if (verbose){
			cat("done\n")
		}
	}
	
	
	#Let's serialize the object if the user wishes
	if (serialize){
		cat("serializing in order to be saved for future R sessions...")
		.jcache(yarf_mod$java_YARF)
		cat("done\n")
	}
	
	#use R's S3 object orientation
	class(yarf_mod) = "YARF"
	yarf_mod
}

##private function that creates a duplicate of an existing bartMachine object.
bart_machine_duplicate = function(bart_machine, X = NULL, y = NULL, cov_prior_vec = NULL, num_trees = NULL, run_in_sample = NULL, covariates_to_permute = NULL, verbose = NULL, ...){	
	if (is.null(X)){
		X = bart_machine$X
	}
	if (is.null(y)){
		y = bart_machine$y
	}
	if (is.null(cov_prior_vec)){
		cov_prior_vec = bart_machine$cov_prior_vec
	}
	if (is.null(num_trees)){
		num_trees = bart_machine$num_trees
	}	
	if (is.null(run_in_sample)){
		run_in_sample = FALSE
	}
	if (is.null(covariates_to_permute)){
		covariates_to_permute = bart_machine$covariates_to_permute
	}
	if (is.null(verbose)){
		verbose = FALSE
	}	
	build_bart_machine(X, y,
		num_trees = num_trees, #found many times to not get better after this value... so let it be the default, it's faster too 
		num_burn_in = bart_machine$num_burn_in, 
		num_iterations_after_burn_in = bart_machine$num_iterations_after_burn_in, 
		alpha = bart_machine$alpha,
		beta = bart_machine$beta,
		k = bart_machine$k,
		q = bart_machine$q,
		nu = bart_machine$nu,
		prob_rule_class = bart_machine$prob_rule_class,
		mh_prob_steps = bart_machine$mh_prob_steps, #only the first two matter
		run_in_sample = run_in_sample,
		s_sq_y =  bart_machine$s_sq_y, # "mse" or "var"
		cov_prior_vec = cov_prior_vec,
		use_missing_data = bart_machine$use_missing_data,
		covariates_to_permute = covariates_to_permute, #PRIVATE
		num_rand_samps_in_library = bart_machine$num_rand_samps_in_library, #give the user the option to make a bigger library of random samples of normals and inv-gammas
		use_missing_data_dummies_as_covars = bart_machine$use_missing_data_dummies_as_covars,
		replace_missing_data_with_x_j_bar = bart_machine$replace_missing_data_with_x_j_bar,
		impute_missingness_with_rf_impute = bart_machine$impute_missingness_with_rf_impute,
		impute_missingness_with_x_j_bar_for_lm = bart_machine$impute_missingness_with_x_j_bar_for_lm,
		mem_cache_for_speed = bart_machine$mem_cache_for_speed,
		serialize = FALSE, #we do not want to waste CPU time here since these are created internally by us
		verbose = verbose)
}

#build a BART-cv model
build_bart_machine_cv = function(X = NULL, y = NULL, Xy = NULL, 
		num_tree_cvs = c(50, 200),
		k_cvs = c(2, 3, 5),
		nu_q_cvs = list(c(3, 0.9), c(3, 0.99), c(10, 0.75)),
		k_folds = 5, 
		verbose = FALSE,
		...){
	
	if ((is.null(X) && is.null(Xy)) || is.null(y) && is.null(Xy)){
		stop("You need to give bartMachine a training set either by specifying X and y or by specifying a matrix Xy which contains the response named \"y.\"\n")
	} else if (!is.null(X) && !is.null(y) && !is.null(Xy)){
		stop("You cannot specify both X,y and Xy simultaneously.")	
	} else if (is.null(X) && is.null(y)){ #they specified Xy, so now just pull out X,y
		if (class(Xy) != "data.frame"){
			stop(paste("The training data Xy must be a data frame."), call. = FALSE)	
		}
		y = Xy$y
		Xy$y = NULL
		X = Xy
	}
	
	y_levels = levels(y)
	if (class(y) == "numeric" || class(y) == "integer"){ #if y is numeric, then it's a regression problem
		pred_type = "regression"
	} else if (class(y) == "factor" & length(y_levels) == 2){ #if y is a factor and and binary, then it's a classification problem
		pred_type = "classification"
	} else { #otherwise throw an error
		stop("Your response must be either numeric, an integer or a factor with two levels.\n")
	}
	
	if (pred_type == "classification"){
		nu_q_cvs = list(c(3, 0.9)) #ensure we only do this once, the 3 and the 0.9 don't actually matter, they just need to be valid numbers for the hyperparameters
	}
	
	min_rmse_num_tree = NULL
	min_rmse_k = NULL
	min_rmse_nu_q = NULL
	min_oos_rmse = Inf
	min_oos_misclassification_error = Inf
	
	cv_stats = matrix(NA, nrow = length(k_cvs) * length(nu_q_cvs) * length(num_tree_cvs), ncol = 6)
	colnames(cv_stats) = c("k", "nu", "q", "num_trees", "oos_error", "% diff with lowest")
	
  ##generate a single set of folds to keep using
	temp = rnorm(length(y))
	folds_vec = cut(temp, breaks = quantile(temp, seq(0, 1, length.out = k_folds + 1)), 
	                include.lowest= T, labels = F)
  
    #cross-validate
	run_counter = 1
	for (k in k_cvs){
		for (nu_q in nu_q_cvs){
			for (num_trees in num_tree_cvs){
				
				if (pred_type == "regression"){
					cat(paste("  bartMachine CV try: k:", k, "nu, q:", paste(as.numeric(nu_q), collapse = ", "), "m:", num_trees, "\n"))	
				} else {
					cat(paste("  bartMachine CV try: k:", k, "m:", num_trees, "\n"))
				}
				
				k_fold_results = k_fold_cv(X, y, 
          			k_folds = k_folds,
					folds_vec = folds_vec, ##will hold the cv folds constant 
					num_trees = num_trees,
					k = k,
					nu = nu_q[1],
					q = nu_q[2], 
					verbose = verbose,
					...)
				
				if (pred_type == "regression" && k_fold_results$rmse < min_oos_rmse){
					min_oos_rmse = k_fold_results$rmse					
					min_rmse_k = k
					min_rmse_nu_q = nu_q
					min_rmse_num_tree = num_trees
				} else if (pred_type == "classification" && k_fold_results$misclassification_error < min_oos_misclassification_error){
					min_oos_misclassification_error = k_fold_results$misclassification_error					
					min_rmse_k = k
					min_rmse_nu_q = nu_q
					min_rmse_num_tree = num_trees					
				}
				
				cv_stats[run_counter, 1 : 5] = c(k, nu_q[1], nu_q[2], num_trees, 
					ifelse(pred_type == "regression", k_fold_results$rmse, k_fold_results$misclassification_error))
				run_counter = run_counter + 1
			}
		}
	}
	if (pred_type == "regression"){
		cat(paste("  bartMachine CV win: k:", min_rmse_k, "nu, q:", paste(as.numeric(min_rmse_nu_q), collapse = ", "), "m:", min_rmse_num_tree, "\n"))
	} else {
		cat(paste("  bartMachine CV win: k:", min_rmse_k, "m:", min_rmse_num_tree, "\n"))
	}
	#now that we've found the best settings, return that bart machine. It would be faster to have kept this around, but doing it this way saves RAM for speed.
	bart_machine_cv = build_bart_machine(X, y,
			num_trees = min_rmse_num_tree,
			k = min_rmse_k,
			nu = min_rmse_nu_q[1],
			q = min_rmse_nu_q[2], ...)
	
	#give the user some cv_stats ordered by the best (ie lowest) oosrmse
	cv_stats = cv_stats[order(cv_stats[, "oos_error"]), ]
	cv_stats[, 6] = (cv_stats[, 5] - cv_stats[1, 5]) / cv_stats[1, 5] * 100
	bart_machine_cv$cv_stats = cv_stats
  	bart_machine_cv$folds = folds_vec
	bart_machine_cv
}

##private function for filling in missing data with averages for cont. vars and modes for cat. vars
imputeMatrixByXbarjContinuousOrModalForBinary = function(X_with_missing, X_for_calculating_avgs){
	for (i in 1 : nrow(X_with_missing)){
		for (j in 1 : ncol(X_with_missing)){
			if (is.na(X_with_missing[i, j])){
				#mode for factors, otherwise average
				if (class(X_with_missing[, j]) == "factor"){
					X_with_missing[i, j] = names(which.max(table(X_for_calculating_avgs[, j])))
				} else {
					X_with_missing[i, j] = mean(X_for_calculating_avgs[, j], na.rm = TRUE)
				}
			}
		}
	}
	#now we have to go through and drop columns that are all NaN's if need be
	bad_cols = c()
	for (j in colnames(X_with_missing)){
		if (sum(is.nan(X_with_missing[, j])) == nrow(X_with_missing)){
			bad_cols = c(bad_cols, j)
		}
	}
	for (j in bad_cols){
		X_with_missing[, j] = NULL
	}
	X_with_missing
}



testfun = function(a, b, c){
	sum = a+b+c
	
	l = as.list(match.call())
	l[[1]] = NULL
	c(l, sum = sum)
}