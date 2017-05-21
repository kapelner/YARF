#' Computes a response prediction for new data observation(s). Predictions are an aggregation from each of the 
#' individual trees' predictions based on the function passed into the model creation step. The default 
#' aggregation is the sample average for regression and the modal class for classification. You can set
#' a custom aggregation method via the \code{YARF_set_aggregation_method} function.
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
	if (object$pred_type == "classification"){ #convert back to the native factor representation
		y_hats = factor(y_hats, labels = object$y_levels)
	}
	y_hats
	
	###needs type = raw and type = prob
	
	###needs impurity measures of var importance
}

#' Computes a response prediction for new data observation(s) for all trees (unaggregated). 
#' 
#' @param yarf_mod 					A YARF model object.
#' @param new_data 					A n* x p matrix where each row is an observation you wish to predict for. Here,
#' 									p is the same length as the original training data.
#' @return 							A matrix of yhat predictions for the \code{new_data} record(s) for all trees 
#' 									(size n* x num_trees).
#' 
#' @author Adam Kapelner
#' @export
YARF_predict_all_trees = function(yarf_mod, new_data){
	check_serialization(yarf_mod) #ensure the Java object exists and fire an error if not
	
	if (class(new_data) != "data.frame"){		
		stop("\"new_data\" needs to be a data frame with the same column names as the training data.")
	}
	if (!yarf_mod$use_missing_data){
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
	new_data = pre_process_new_data(new_data, yarf_mod)
	
	y_hats = matrix(NA, nrow = nrow(new_data), ncol = yarf_mod$num_trees)
	for (i in 1 : nrow(new_data)){
		y_hats[i, ] = .jcall(yarf_mod$java_YARF, "[D", "allNodeAssignments", as.numeric(new_data[i, ]))

	}
	if (yarf_mod$pred_type == "classification"){ #convert back to the native factor representation
		y_hats = factor(y_hats, labels = yarf_mod$y_levels)
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