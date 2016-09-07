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
		cat(num_trees_completed, " / ", yarf_mod$num_trees, " trees completed (", round(progress * 100, 1), "% done in ", round(time_elapsed_in_min, 1), " minutes).\n", sep = "")	
		
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