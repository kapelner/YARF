#' Prints a summary of the model to the console
#' 
#' @param object 					A YARF model object.
#' @param ... 						Other methods passed to summary
#' @method summary YARF
#' 
#' @author Adam Kapelner
#' @export
summary.YARF = function(object, ...){	
	cat(paste("YARF v", VERSION, ifelse(object$pred_type == "regression", " for regression", " for classification"), "\n", sep = ""))
	if (object$use_missing_data){
		cat("Missing data feature ON.\n")
	}
	n = object$n
	#first print out characteristics of the training data
	converged = .jcall(object$java_YARF, "Z", "converged")
	if (converged){
		cat(paste("training data n =", n, "and p =", object$p, "\n"))
	} else {
		cat(paste(object$num_trees, "trees,", "training data n =", n, "and p =", object$p, "\n"))
	}
	
	
	progress_report = YARF_progress(object, console_message = FALSE)
	
	if (progress_report$done){
		cat("Model construction completed in", round(progress_report$time_elapsed_in_min, 2), "minutes.\n")
	} else {
		#just print out a progress report
		YARF_progress(object, console_message = TRUE)
	}
	
	#print out OOB info if calculated (even if model not fully constructed yet...)
	if (!is.null(object$y_oob)){ 
		miss = object$num_oob_obs_missing
		pct_reporting = round((n - miss) / n * 100, 2)
		if (pct_reporting == 0){
			cat("No OOB results to show (no trees have been fit as of yet).\n")
		}
		else {
			if (miss > 0){
				cat("OOB results on ", pct_reporting, "% of the observations (", miss, " missing)", sep = "")
			} else {
				cat("OOB results on all observations")
			}	
			
			if (!is.null(object$oob_cost_calculation)){
				cat(" for your custom cost function:\n")
				cat("  average cost:", round(object$y_oob_average_cost, 5), "\n")
				cat("  median cost:", round(object$y_oob_median_cost, 5), "\n")
				cat("  total cost:", round(object$y_oob_total_cost, 5), "\n")
			} else if (object$pred_type == "regression"){
				cat(":\n")
				cat("  R^2:", round(object$pseudo_rsq_oob, 5), "\n")
				cat("  RMSE:", round(object$rmse_oob, 3), "\n")
				cat("  MAE:", round(object$mae_oob, 3), "\n")
				cat("  L2:", round(object$L2_err_oob, 2), "\n")
				cat("  L1:", round(object$L1_err_oob, 2), "\n")
			} else {
				cat(" as a confusion matrix:\n")
				print(object$confusion_matrix)
				cat("    Accuracy: ", round(object$classification_accuracy * 100, 3), "%\n", sep = "")
			}			
		}
	} else {
		cat("Run the 'YARF_update_with_oob_results' function to get out of sample\nperformance estimates using the out of bag predictions.\n")
	}
}

#' Alias for summary.
#' 
#' @param x		 					A YARF model object.
#' @param ... 						Other methods passed to print
#' @method print YARF
#' 
#' @author Adam Kapelner
#' @export
print.YARF = function(x, ...){summary(x, ...)}
