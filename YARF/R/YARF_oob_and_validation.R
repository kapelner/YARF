#' Computes the out-of-bag (OOB) predictions for the training data. 
#' This gives a good sense of out-of-sample performance in the future.
#' 
#' @param yarf_mod 							The yarf model object
#' @param oob_cost_calculation_script		An optional custom Javascript function which calculates the cost of a prediction given the true
#' 											value of the prediction (see below). If is likely similar to \code{cost_single_node_calc_script}. It
#' 											is recommended to share code between them by writing a function included in \code{shared_scripts} which is
#' 											passed into \code{YARF} upon construction and can be referenced when calculating OOB results.
#' 
#' 											  function oobCost(y_hat, y)\{ //y_hat is the predicted value and y is the true value (both are of type double)
#' 
#' 											    ...
#' 
#' 											    return double //where a larger number indicates a higher cost to the error between y and y_hat.
#' 
#' 											  \}
#' 
#' @param indices							The indices to compute OOB performance. This is generally a private argument
#' 											for use by the validation / test functions.
#' 
#' @author Adam Kapelner
#' @export
YARF_update_with_oob_results = function(yarf_mod, oob_cost_calculation_script = NULL, indices = NULL){
	y = yarf_mod$y
	n = yarf_mod$n
	
	if (is.null(oob_cost_calculation_script)){
		.jcall(yarf_mod$java_YARF, "V", "setOob_cost_calculation_str", .jnull(class = "java/lang/String"))
	} else {
		if (class(oob_cost_calculation_script) != "character"){
			stop("'oob_cost_calculation_script' must be a character string of Javascript code")
		}
		.jcall(yarf_mod$java_YARF, "V", "setOob_cost_calculation_str", oob_cost_calculation_script)
	}
	
	yarf_mod$oob_cost_calculation_script = oob_cost_calculation_script

	#get it from java multithreaded
	num_cores = as.integer(get("YARF_NUM_CORES", YARF_globals))
	y_oob = .jcall(yarf_mod$java_YARF, "[D", "predictOutOfBag", num_cores)
	
	if (!is.null(indices)){
		y = y[indices]
		y_oob = y_oob[indices]
		n = length(indices)
	}
	
	y_oob[is.nan(y_oob)] = NA
	yarf_mod$num_oob_obs_missing = sum(is.na(y_oob))
	n = n - yarf_mod$num_oob_obs_missing
	
	if (!is.null(yarf_mod$oob_cost_calculation)){
		yarf_mod$y_oob = y_oob
		yarf_mod$y_oob_costs = .jcall(yarf_mod$java_YARF, "[D", "customOutOfBagCostCalc", y, y_oob)
		yarf_mod$y_oob_total_cost = sum(yarf_mod$y_oob_costs)
		yarf_mod$y_oob_average_cost = yarf_mod$y_oob_total_cost / n
		yarf_mod$y_oob_median_cost = median(yarf_mod$y_oob_costs)
	} else if (yarf_mod$pred_type == "regression"){
		yarf_mod$y_oob = y_oob
		yarf_mod$residuals = y - y_oob
		yarf_mod$L1_err_oob = sum(abs(yarf_mod$residuals), na.rm = TRUE)
		yarf_mod$L2_err_oob = sum(yarf_mod$residuals^2, na.rm = TRUE)
		yarf_mod$pseudo_rsq_oob = 1 - yarf_mod$L2_err_oob / sum((y - mean(y))^2)
		yarf_mod$rmse_oob = sqrt(yarf_mod$L2_err_oob / n)
		yarf_mod$mae_oob = yarf_mod$L1_err_oob / n
	} else {		
		#convert results to factor
		numeric_labels = names(table(yarf_mod$y))
		yarf_mod$y_oob = factor(y_oob, levels = numeric_labels, labels = yarf_mod$y_levels)
		y = factor(yarf_mod$y, levels = numeric_labels, labels = yarf_mod$y_levels)
		
		#calculate confusion matrix
		n_levels = yarf_mod$num_y_levels
		
		confusion_matrix = as.data.frame(matrix(NA, nrow = n_levels + 1, ncol = n_levels + 1))
		rownames(confusion_matrix) = c(paste("actual", yarf_mod$y_levels), "use errors")
		colnames(confusion_matrix) = c(paste("predicted", yarf_mod$y_levels), "model errors")
		
		#set the confusion counts
		confusion_matrix[1 : n_levels, 1 : n_levels] = data.matrix(table(y, yarf_mod$y_oob, useNA = "no")) #should coerce to ints so we get rounding nicely later
		#set all test errors
		for (k in 1 : n_levels){
			confusion_matrix[k, n_levels + 1] = round(1 - confusion_matrix[k, k] / sum(confusion_matrix[k, 1 : n_levels]), 3)
		}
		#set all use errors
		for (k in 1 : n_levels){
			confusion_matrix[n_levels + 1, k] = round(1 - confusion_matrix[k, k] / sum(confusion_matrix[1 : n_levels, k]), 3)
		}
		#set overall error
		yarf_mod$misclassification_error = (sum(confusion_matrix[1 : n_levels, 1 : n_levels]) - sum(diag(as.matrix(confusion_matrix[1 : n_levels, 1 : n_levels])))) / n
		yarf_mod$classification_accuracy = 1 - yarf_mod$misclassification_error
		confusion_matrix[n_levels + 1, n_levels + 1] = round(yarf_mod$misclassification_error, 3)
		#return the whole thing
		yarf_mod$confusion_matrix = confusion_matrix
	}
	if (yarf_mod$serialize){ #if they did it before, do it now too
		YARF_serialize(yarf_mod)
	}
	#send it back
	yarf_mod
}

#' Computes the out-of-bag (OOB) predictions for the validation subset of the training data. 
#' This allows the user to try many different models while still leaving test data for 
#' true out-of-sample performance estimation.
#' 
#' @param yarf_mod 							The yarf model object
#' @param validation_proportion				The percentage of the data to use for validation data
#' 											(and the remaining is test data). The default is 80\%.
#' 
#' @author Adam Kapelner
#' @export
YARF_update_with_oob_validation_results = function(yarf_mod, validation_proportion = 0.8){
	n = yarf_mod$n
	n_validation = round(n * validation_proportion)
	validation_indices = yarf_mod$validation_test_indices[1 : n_validation]
	yarf_mod = YARF_update_with_oob_results(yarf_mod, validation_indices)
	cat("Validation Results (", round(validation_proportion * 100, 1), "% of the data) for...\n", sep = "")
	print(yarf_mod)
	#it's critical to return n_validation so the test results function can be run
	yarf_mod$n_validation = n_validation
	yarf_mod
}

#' Computes the out-of-bag (OOB) predictions for the test subset of the training data. 
#' This function should only be run once. If it run again you risk adapting your model to the 
#' test data which will lead to overfitting and hence a biased out-of-sample performance estimate.
#' 
#' @param yarf_mod 							The yarf model object such that \code{YARF_update_with_oob_validation_results}
#' 											has been run before on a user-determined proportion of OOB samples 
#' 											as data validation.
#' 
#' @author Adam Kapelner
#' @export
YARF_update_with_oob_test_results = function(yarf_mod){
	n_validation = yarf_mod$n_validation
	n = yarf_mod$n
	if (is.null(n_validation)){
		stop("You must first run \"yarf_mod = YARF_update_with_oob_validation_results(yarf_mod, .)\" to establish a validation subset.")
	}
	if (!is.null(yarf_mod$test_data_revealed)){
		warning("You have already peeked at the test data!!!")
	}
	test_indices = yarf_mod$validation_test_indices[(n_validation + 1) : n]
	yarf_mod = YARF_update_with_oob_results(yarf_mod, test_indices)
	cat("FINAL Test Results (", round((n - n_validation - 1) / n * 100, 1), "% of the data) for...\n", sep = "")
	print(yarf_mod)
	yarf_mod$test_data_revealed = TRUE
	yarf_mod
}



