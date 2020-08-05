#' Computes a response prediction for new data observation(s). 
#' 
#' Predictions are an aggregation from each of the 
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
	
	if (!("data.frame" %in% class(new_data))){
		stop("Data frame required for \"new_data\".")
	}
	
	#cast it just to make sure (doesn't work with tbl_df's e.g.) but ensure it keeps the same column names. Casting using data.frame 
	#changes "(Intercept)" to "X.Intercept." for some terrrrrrible reason.
	original_col_names = colnames(new_data)
	new_data = data.frame(new_data)
	colnames(new_data) = original_col_names
	
	#now we take the columns from the training set
	
	
	if (!isTRUE(all.equal(colnames(new_data), colnames(object$X)))){
		warning("Prediction set column names did not match training set column names.\nAttempting to subset to training set columns.\n")
		
		tryCatch(
			{new_data = new_data[, colnames(object$X)]},
			error = function(e){
				stop("one or more features in the training data are not found in the prediction set")
			},
			finally = {}
		)
	}
	### we should now pull out the columns that are in new_data and then if there
	#are any left over, make NA cols - this is a convenience for the user
	
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
	
	y_hats = .jcall(object$java_YARF, "[D", "Evaluate", .jarray(new_data, dispatch = TRUE), num_cores, simplify = TRUE)
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
#' @param new_data 					A n* x p data frame where each row is an observation you wish to predict for. Here,
#' 									p is the same length as the original training data and \code{new_data} has the same
#' 									column names. If a matrix object is passed in, it is automatically cast to a data frame. 
#' 									This may not always give you what you want. 
#' @return 							A matrix of yhat predictions for the \code{new_data} record(s) for all trees 
#' 									(size n* x num_trees).
#' 
#' @author Adam Kapelner
#' @export
YARF_predict_all_trees = function(yarf_mod, new_data){
	check_serialization(yarf_mod) #ensure the Java object exists and fire an error if not
	
	if (class(new_data) != "data.frame"){
		new_data = data.frame(new_data)
		setNames(new_data, names(yarf_mod$X))
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
		y_hats[i, ] = .jcall(yarf_mod$java_YARF, "[D", "allNodeAssignments", as.numeric(new_data[i, ]), simplify = TRUE)

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



#' Sets the Tree Aggregation Method
#' 
#' This function sets custom code to be run when the Forest is making a decision from its many trees. This function should be run
#' before you run \code{predict} or \code{YARF_update_with_oob_results} so that your custom aggregation can be employed. This function 
#' is optional as the default aggregation method corresponds to (a) the sample average for regression and (b) the modal category 
#' for classification. One further note of warning: once this script has been set, it will be retained in the YARF model until 
#' this function is run again for this YARF model object with a new aggregation script. Set \code{aggregation_script = NULL} if you
#' wish to reset. 
#' 
#' @param yarf_mod 					The yarf model object
#' @param aggregation_script		A custom javascript function which aggregates the predictions in the trees for one observations 
#' 									into one scalar prediction (see below). 
#' 
#' 									function aggregateYhatsIntoOneYhat(y_hats, yarf)\{ //y_hats is an array of doubles of size num_trees
#' 										//and yarf provides access to the entire random forest object if needed (of type YARF.YARF)
#' 
#' 										...
#' 
#' 										return double //this is the final predicted value aggregated from all tree predictions
#' 
#' 									\}
#' @return 							The yarf model object (invisibly)
#' 
#' @author Adam Kapelner
#' @export
YARF_set_aggregation_method = function(yarf_mod, aggregation_script){
	if (is.null(aggregation_script)){
		.jcall(yarf_mod$java_YARF, "V", "setAggregation_function_str", .jnull("java/lang/String"))
	} else {
		if (class(aggregation_script) != "character"){
			stop("'aggregation_script' must be a character string of Javascript code if non-null")
		}
		.jcall(yarf_mod$java_YARF, "V", "setAggregation_function_str", aggregation_script)
	}
	
	yarf_mod$aggregation_script = aggregation_script
	invisible(yarf_mod)
}


