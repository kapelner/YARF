#' Tests the effect of H0: one, or two, ... or all covariates are not predictive (out of sample) via a permutation test. 
#' We permute the covariates column(s) and build YARF models \code{num_permutation_samples} times.
#' The p-val is determined by a permutation-like test.
#'  
#' @param yarf_mod 					The fit model to test against permuted training data.
#' @param covariates 				The covariates to permute as a vector of indices or names. If multiple covariates
#' 									they are permuted as a single block to not break collinearity. If this parameter is 
#' 									specified as \code{NULL}, we test all the covariates by permuting y instead. 
#' 									Default is \code{NULL}.
#' @param num_permutation_samples 	How many different YARF models should be built - one for each different
#' 									permutation of the variables tests. The is the resolution of the test. The 
#' 									default is \code{100}.
#' @param plot 						Plot a histogram of the permutation samples with the original sample displayed?
#' 									Default is \code{TRUE}.
#' @return 							A list with the following components: \code{permutation_samples_of_error} whose value
#' 									is a vector with entries being the error values for each permutation sample, 
#' 									\code{observed_error_estimate} is the error value of the original model built with 
#' 									unpermuted data and \code{pval} is the estimated significance level of the permutation test.
#' 
#' @author Adam Kapelner
#' @export
cov_importance_test = function(yarf_mod, covariates = NULL, num_permutation_samples = 100, plot = TRUE){
	check_serialization(yarf_mod) #ensure the Java object exists and fire an error if not
	
	all_covariates = yarf_mod$training_data_features
	
	if (is.null(covariates)){
		title = "YARF omnibus test for covariate importance\n"
	} else if (length(covariates) <= 3){
		if (class(covariates[1]) == "numeric"){
			cov_names = paste(all_covariates[covariates], collapse = ", ")
		} else {
			cov_names = paste(covariates, collapse = ", ")
		}
		title = paste("YARF test for importance of covariate(s):", cov_names, "\n")
	} else {
		title = paste("YARF test for importance of", length(covariates), "covariates", "\n")
	}
	cat(title)
	
	#get this model's results if necessary
	if (is.null(yarf_mod$y_oob)){
		yarf_mod = YARF_update_with_oob_results(yarf_mod)
	}
	
	#the error in the true, unpermuted model - the yardstick we'll be comparing to
	if (!is.null(yarf_mod$oob_cost_calculation)){
		permutation_samples_of_error[nsim] = yarf_mod$y_oob_average_cost
	} else if (object$pred_type == "regression"){
		permutation_samples_of_error[nsim] = yarf_mod$pseudo_rsq_oob
	} else {
		permutation_samples_of_error[nsim] = yarf_mod$misclassification_error
	}
	observed_error_estimate = ifelse(yarf_mod$pred_type == "regression", yarf_mod$PseudoRsq, yarf_mod$misclassification_error)
	
	permutation_samples_of_error = array(NA, num_permutation_samples)
	for (nsim in 1 : num_permutation_samples){
		cat(".")
		if (nsim %% 50 == 0){
			cat("\n")
		}	
		indices_permutation = sample(1 : yarf_mod$n)
		
		if (is.null(covariates)){
			#omnibus F-like test - just permute y (same as permuting ALL the columns of X and it's faster)
			yarf_samp = yarf_duplicate(yarf_mod, y = yarf_mod$y[indices_permutation]) #we have to turn verbose off otherwise there would be too many outputs
			
		} else {
			#partial F-like test - permute the columns that we're interested in seeing if they matter
			X_samp = yarf_mod$X #copy original design matrix
			
			
			for (cov in covariates){
				if (cov %in% colnames(X_samp)){
					X_samp[, cov] = X_samp[indices_permutation, cov]
				}
			}
			
			#build a model then get its oob results
			yarf_samp = yarf_duplicate(yarf_mod, X = X_samp)
			yarf_samp = YARF_update_with_oob_results(yarf_samp)
		}
		#record permutation result
		if (!is.null(yarf_mod$oob_cost_calculation)){
			permutation_samples_of_error[nsim] = yarf_samp$y_oob_average_cost
		} else if (object$pred_type == "regression"){
			permutation_samples_of_error[nsim] = yarf_samp$pseudo_rsq_oob
		} else {
			permutation_samples_of_error[nsim] = yarf_samp$misclassification_error
		}
	}
	cat("\n")
	
	##compute p-value
	pval = ifelse(yarf_mod$pred_type == "regression", sum(observed_error_estimate < permutation_samples_of_error), sum(observed_error_estimate > permutation_samples_of_error)) / (num_permutation_samples + 1)
	
	if (plot){
		hist(permutation_samples_of_error, 
				xlim = c(min(permutation_samples_of_error, 0.99 * observed_error_estimate), max(permutation_samples_of_error, 1.01 * observed_error_estimate)),
				xlab = paste("permutation samples\n pval = ", round(pval, 3)),
				br = num_permutation_samples / 10,
				main = paste(title, "Null Samples of", ifelse(yarf_mod$pred_type == "regression", "Pseudo-R^2's", "Misclassification Errors")))
		abline(v = observed_error_estimate, col = "blue", lwd = 3)
	}
	cat("p_val = ", pval, "\n")
	invisible(list(
		permutation_samples_of_error = permutation_samples_of_error, 
		observed_error_estimate = observed_error_estimate, 
		pval = pval
	))
}

#' Convenience method for testing a model's fit. The strategy is simple: one builds a YARF model from the 
#' residuals of the model you wish to fit and performs an omnibus test of all covariates against those 
#' residuals. This effecticely answers the question: "is there out-of-sample predictive information left over after 
#' this model was fit?" The p-val is determined by a permutation-like test.
#' 
#' @param X							The training data frame 
#' @param y 						The vector of training responses
#' @param lin_mod 					An already-fit model that you wish to test if there
#' 									is any predictive power left over. The default is \code{NULL}
#' 									which indicates the standard linear model. Missingness not allowed.
#' @param num_permutation_samples 	The resolution of the test. See \link{cov_importance_test}. The 
#' 									default is \code{100}.
#' @param plot 						Plot the result as a histogram. Default is \code{TRUE}.
#' @param ... 						Additional parameters to be passed to \link{YARF}.
#' @return 							The same as \link{cov_importance_test}.
#' 
#' @author Adam Kapelner
#' @export
model_fit_test = function(X, y, model_fit = NULL, num_permutation_samples = 100, plot = TRUE, ...){
	if (is.null(model_fit)){
		model_fit = lm(y ~ as.matrix(X))
	}
	y_hat = predict(model_fit, X)
	yarf_mod = YARF(X, y - y_hat, ...)
	cov_importance_test(yarf_mod, num_permutation_samples = num_permutation_samples, plot = plot)	
}


#' Duplicates the settings of YARF model. Wait is \code{TRUE}, all verbose logging is turned
#' off and serializing is turned off as well.
#' 
#' @param yarf_mod	The YARF model with settings you wish to duplicate 
#' @param X 		The training data. Default is \code{NULL} indicating the original training data.
#' @param y 		The training responses. Default is \code{NULL} indicating the original training responses.
#' @return 			
#' 
#' @author Adam Kapelner
#' @export
yarf_duplicate = function(yarf_mod, X = NULL, y = NULL){	
	if (is.null(X)){
		X = yarf_mod$X
	}
	if (is.null(y)){
		y = yarf_mod$y
	}
	YARF(X = X, y = y,
		Xother = yarf_mod$Xother,
		allow_missingness_in_y = yarf_mod$allow_missingness_in_y,		
		num_trees = yarf_mod$num_trees,
		bootstrap_indices = yarf_mod$bootstrap_indices,
		other_indices = yarf_mod$other_indices,
		mtry = yarf_mod$mtry,
		nodesize = yarf_mod$nodesize,
		mtry_script = yarf_mod$mtry_script,
		node_to_leaf_script = yarf_mod$node_to_leaf_script,
		cost_single_node_calc_script = yarf_mod$cost_single_node_calc_script,
		node_assign_script = yarf_mod$node_assign_script,
		after_node_birth_function_script = yarf_mod$after_node_birth_function_script,
		aggregation_script = yarf_mod$aggregation_script,
		oob_cost_calculation_script = yarf_mod$oob_cost_calculation_script,
		shared_scripts = yarf_mod$shared_scripts, 
		use_missing_data = yarf_mod$use_missing_data,
		replace_missing_data_with_x_j_bar = yarf_mod$replace_missing_data_with_x_j_bar,
		seed = yarf_mod$seed,
		wait = TRUE, #we need to wait because it goes in serial
		serialize = FALSE, #we do not want to waste CPU time here since these are created internally by us
		verbose = FALSE, #no way 
		debug_log = FALSE #no way
	)
}