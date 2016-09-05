

#' Computes a response prediction for new data observation(s). Predictions are an aggregation from each of the 
#' individual trees' predictions based on the function passed into the model creation step. If no function was 
#' passed in, the default aggregation is the sample average for regression and the modal class for classification. 
#' 
#' @param object 					A YARF model object.
#' @param new_data 					A n* x p matrix where each row is an observation you wish to predict for. Here,
#' 									p is the same length as the original training data.
#' @param ... 						Other methods passed to predict
#' @method predict YARF
#' @return 							The yhat prediction for the \code{new_data} record(s).
#' 
#' @author Kapelner
#' @export
predict.YARF = function(object, new_data, ...){
	check_serialization(object) #ensure the Java object exists and fire an error if not
	
	if (class(new_data) != "data.frame"){		
		stop("\"new_data\" needs to be a data frame with the same column names as the training data.")
	}
	if (!object$use_missing_data){
		nrow_before = nrow(new_data)
		new_data = na.omit(new_data)
		if (nrow_before > nrow(new_data)){
			cat(nrow_before - nrow(new_data), "rows omitted due to missing data. Try using the missing data feature when building YARF to be able to predict on all observations.\n")
		}
	}
	
	if (nrow(new_data) == 0){
		stop("No rows to predict.\n")
	}
	
	#now process and make dummies if necessary
	new_data = pre_process_new_data(new_data, object)
	
	num_cores = as.integer(get("YARF_NUM_CORES", YARF_globals))
	
	y_hats = .jcall(object$java_YARF, "[D", "Evaluate", .jarray(new_data, dispatch = TRUE), num_cores)
	if (object$pred_type == "classification"){	
		y_hats = factor(y_hats, levels = yarf_mod$y_levels)
	}
	y_hats
}

#private method to see if the user saved the Java object within the R object and throw an error if not.
check_serialization = function(yarf){
	if (is.jnull(yarf$java_YARF)){
		stop("This YARF object was loaded from an R image but was not serialized.\n  Please build YARF using the option \"serialize = TRUE\" next time.\n")
	}
}
#see http://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode
sample_mode = function(arr) {
	ux = unique(arr)
	ux[which.max(tabulate(match(arr, ux)))]
}