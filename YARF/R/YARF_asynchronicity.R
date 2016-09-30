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
		time_elapsed_in_min = (as.numeric(Sys.time()) - as.numeric(yarf_mod$t0)) / 60
	} else {
		time_elapsed_in_min = (.jcall(yarf_mod$java_YARF, "J", "getCompletionTime") / 1000 - as.numeric(yarf_mod$t0)) / 60
	}
	
	#now estimate how long it will take to complete
	if (num_trees_completed >= 1 & progress < 1){
		total_time_estimate = time_elapsed_in_min / progress
		time_remaining_estimate = total_time_estimate - time_elapsed_in_min
	}
	
	if (console_message){
		cat(num_trees_completed, " / ", yarf_mod$num_trees, " trees completed (", round(progress * 100, 1), "% done in ", round(time_elapsed_in_min, 1), "min).\n", sep = "")	
		
		if (num_trees_completed >= 1 && progress < 1){
			cat("Estimated time until completion: ", round(time_remaining_estimate, 1), "min.\n", sep = "")
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
#' @param plot_oob_error					Create a plot of oob error rate as trees are built to assess 
#' 											convergence of the YARF model
#' 
#' @author Kapelner
#' @export
YARF_progress_reports = function(yarf_mod, time_delay_in_seconds = 10, plot_oob_error = FALSE){
	previous_num_trees_completed = 0
	trees = c()
	errors = c()
	while (TRUE){
		progress = YARF_progress(yarf_mod)
		if (progress$done){
			break
		}
		if (plot_oob_error & progress$num_trees_completed > previous_num_trees_completed){
			previous_num_trees_completed = progress$num_trees_completed
			yarf_mod = YARF_update_with_oob_results(yarf_mod)
			trees = c(trees, previous_num_trees_completed)
			if (yarf_mod$pred_type == "regression"){
				errors = c(errors, yarf_mod$pseudo_rsq_oob)
				plot(trees, errors, type = "o", xlab = "# trees completed", ylab = "oob Pseudo-Rsq")
			} else {
				errors = c(errors, yarf_mod$misclassification_error * 100)
				plot(trees, errors, type = "o", xlab = "# trees completed", ylab = "oob Misclassification Error (%)")
			}

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