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
	
	if (progress < 1){
		time_elapsed_in_min = (as.numeric(Sys.time()) - as.numeric(yarf_mod$t0)) / 60
	} else {
		time_elapsed_in_min = (.jcall(yarf_mod$java_YARF, "J", "getCompletionTime") / 1000 - as.numeric(yarf_mod$t0)) / 60
	}	
	time_remaining_estimate = NULL
	
	converged = .jcall(yarf_mod$java_YARF, "Z", "converged")
	stopped = .jcall(yarf_mod$java_YARF, "Z", "stopped")
	if (console_message && stopped && !converged){
		cat("Construction of this model was halted at", num_trees_completed, "trees before all", yarf_mod$num_trees, "trees were constructed.\n")
	} else if (console_message && .jcall(yarf_mod$java_YARF, "Z", "converged")){
		cat("This model converged at", num_trees_completed, "trees (as defined by the user-specified\noob cost function and tolerance level).\n");
	} else {
		#now estimate how long it will take to complete
		if (num_trees_completed >= 1 & progress < 1){
			total_time_estimate = time_elapsed_in_min / progress
			time_remaining_estimate = total_time_estimate - time_elapsed_in_min
		}
		
		if (console_message){
			cat(num_trees_completed, " / ", yarf_mod$num_trees, " trees completed (", round(progress * 100, 1), "% done in ", round(time_elapsed_in_min, 1), "min).\n", sep = "")	
			
			if (num_trees_completed >= 1 && progress < 1){
				cat("Estimated time until completion: ", round(time_remaining_estimate, 1), "min (barring convergence).\n", sep = "")
			} else if (progress < 1) {
				cat("No time estimate for completion until the first tree is constructed.\n")
			}
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
#' @param time_delay_in_seconds				Frequency of messages in seconds. Default is \code{5} seconds.
#' @param trail_pts							If \code{plot_oob_error} is \code{TRUE}, this optional parameter
#' 											if non null will plot a secondary window with only the trees in the last \code{trail_trees}
#' 											number of progress updates defined by \code{time_delay_in_seconds} 
#' 											versus error which allows assessing the convergence more closely.
#' 											Default is \code{50}. 								
#' 
#' @author Kapelner
#' @export
YARF_convergence = function(yarf_mod, time_delay_in_seconds = 5, trail_pts = 50){
	if (!is.null(yarf_mod$oob_cost_calculation)){
		ylab = "oob total cost (custom)"
	} else if (yarf_mod$pred_type == "regression"){
		ylab = "oob 1-R^2"
	} else {
		ylab = "oob misclassification rate"
	}
	
	repeat {
		#first get the iteration data
		oob_costs_by_iteration = .jcall(yarf_mod$java_YARF, "[D", "OOBCostsByIteration")
		t = length(oob_costs_by_iteration)
#		cat("t", t, "trail_pts", trail_pts, "\n")
		
		#now plot it if the user wishes
		if (t > 0){
			if (t > trail_pts){
				par(mfrow = c(1, 2))
			} else {
				par(mfrow = c(1, 1))
			}
			
			plot(1 : t, oob_costs_by_iteration, type = "o", xlab = "# trees completed", ylab = ylab)
			if (t > trail_pts){
				plot((t - trail_pts) : t, oob_costs_by_iteration[(t - trail_pts) : t], type = "o", xlab = "# trees completed", ylab = ylab)
			}

		}
		#now ditch if the model is done, converged or halted by the user
		if (.jcall(yarf_mod$java_YARF, "Z", "stopped")){
			break
		}
		progress = YARF_progress(yarf_mod)
		if (progress$done){
			break
		}
		#otherwise take a break and then repeat
		Sys.sleep(time_delay_in_seconds)
	}
	invisible(list(oob_cost_by_iteration = oob_costs_by_iteration))
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