#' Prints out a message reflecting the progress of the YARF model construction
#' 
#' @param yarf_mod 							The yarf model object
#' @param console_message					Should we print a message to console? Default is \code{TRUE}.
#' @return 									The number of trees and the proportion completed.
#' 
#' @author Kapelner
#' @export
YARF_progress = function(yarf_mod, console_message = TRUE){
	num_trees_completed = .jcall(yarf_mod$java_YARF, "I", "progress")
	progress = num_trees_completed / yarf_mod$num_trees
	
	if (.jcall(yarf_mod$java_YARF, "Z", "stopped")){
		cat("Construction of this model was halted at", num_trees_completed, "trees before all", yarf_mod$num_trees, "trees were constructed.\n")
		return
	}
	
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
#' @param trail_pts							If \code{plot_oob_error} is \code{TRUE}, this optional parameter
#' 											if non null will plot a secondary window with only the trees in the last \code{trail_trees}
#' 											number of progress updates defined by \code{time_delay_in_seconds} 
#' 											versus error which allows assessing the convergence more closely.
#' 											Default is \code{5}. 								
#' 
#' @author Kapelner
#' @export
YARF_progress_reports = function(yarf_mod, time_delay_in_seconds = 10, plot_oob_error = FALSE, trail_pts = 5){
	if (.jcall(yarf_mod$java_YARF, "Z", "stopped")){
		YARF_progress(yarf_mod)
		return
	}
	previous_num_trees_completed = 0
	trees = c()
	fit_metrics = c()
	while (TRUE){
		progress = YARF_progress(yarf_mod)
		if (progress$done){
			break
		}
		if (plot_oob_error & progress$num_trees_completed > previous_num_trees_completed){
			previous_num_trees_completed = progress$num_trees_completed
			yarf_mod = YARF_update_with_oob_results(yarf_mod)
			trees = c(trees, previous_num_trees_completed)
			if (!is.null(trail_pts) & length(trees) > trail_pts){
				par(mfrow = c(1, 2))
				num_samples = length(trees)
			} else {
				par(mfrow = c(1, 1))
			}
			if (yarf_mod$pred_type == "regression"){
				fit_metrics = c(fit_metrics, yarf_mod$pseudo_rsq_oob)
				plot(trees, fit_metrics, type = "o", xlab = "# trees completed", ylab = "oob Pseudo-Rsq")
				if (!is.null(trail_pts) & length(trees) > trail_pts){
					plot(trees[(num_samples - trail_pts) : num_samples], fit_metrics[(num_samples - trail_pts) : num_samples], type = "o", xlab = "# trees completed", ylab = "oob Pseudo-Rsq")
				}
			} else {
				fit_metrics = c(fit_metrics, yarf_mod$misclassification_error * 100)
				plot(trees, 100 - fit_metrics, type = "o", xlab = "# trees completed", ylab = "oob % Correctly Classified")
				if (!is.null(trail_pts) & length(trees) > trail_pts){
					plot(trees[(num_samples - trail_pts) : num_samples], 100 - fit_metrics[(num_samples - trail_pts) : num_samples], type = "o", xlab = "# trees completed", ylab = "oob % Correctly Classified")
				}
			}

		}
		Sys.sleep(time_delay_in_seconds)
	}
	list(trees = trees, fit_metrics = fit_metrics)
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
	yarf_mod
}