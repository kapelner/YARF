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
	#first print out characteristics of the training data
	cat(paste("training data n =", object$n, "and p =", object$p, "\n"))
	
	progress_report = YARF_progress(object, console_message = FALSE)
	
	if (progress_report$done){
		cat("Model construction completed in", round(progress_report$time_elapsed_in_min, 2), "minutes.\n")
		
		if (!is.null(yarf_mod$y_oob)){ #print out OOB info if calculated 
			cat("OOB results:\n\n")
			if (yarf_mod$pred_type == "regression"){
				cat("L2:", round(yarf_mod$L2_err_oob, 3), "\n")
				cat("RMSE:", round(yarf_mod$rmse_oob, 3), "\n")
				cat("R^2:", round(yarf_mod$PseudoRsqoob, 3), "\n")
				cat("L1:", round(yarf_mod$L1_err_oob, 3), "\n")
				cat("MAE:", round(yarf_mod$mae_oob, 3), "\n")
			} else {		
				cat("confusion matrix:\n")
				print(confusion_matrix)
			}
		} else {
			cat("Run the 'YARF_update_with_oob_results' function to get out of sample\nperformance estimates using the out of bag predictions.\n")
		}
	} else {
		#just print out a progress report
		YARF_progress(object, console_message = TRUE)
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
