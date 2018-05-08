
#' ROC and ROC-like curves
#' 
#' This generates data with a plot of an ROC curve or an ROC like curve for binary response
#' 
#' @param X 			The covariate data frame
#' @param y 			The binary response vector
#' @param x_axis 		The rate to plot on the x-axis of the ROC like plot as a 3-character case-insensitive string. 
#' 						The default is "FPR" but it canbe any of the following:
#' 
#' 							"tpr" - true positive rate (AKA "sensitivity" or "recall") i.e the number predicted positive among those truly positive
#' 							"fnr" - false negative rate (AKA "miss rate") i.e the number predicted negative among those truly positive
#' 							"fpr" - false positive rate (AKA "fall-out") i.e. the number predicted positive among those truly negative
#' 							"tnr" - true negative rate (AKA "specificity") i.e. the number predicted negative among those truly negative
#' 							"ppv" - positive predictive value i.e. the number truly positive among those predicted positive 
#' 							"fdr" - false discovery rate i.e. the number truly negative among those predicted positive
#' 							"fomr" - false omission rate i.e. the number truly positive among those predicted negative
#' 							"npv" - negative predictive value i.e. the number truly negative among those predicted negative 
#' 	
#' @param y_axis		The rate to plot on the y-axis of the ROC like plot as a 3-character string. 
#' 						The default is "TPR" but it can be any of those
#' 						listed in the documentation for the \code{x_axis} parameter. Must be different from \code{x-axis}.
#' @param ...			Additional arguments passed to YARF construction. This cannot include \code{bootstrap_indicies}.
#' @return 				A data frame containing all
#' 						models with the percent of first class in \code{levels(y)} for the bootstrap indices to replicate
#' 						any point on the plot.
#' 
#' @author Adam
#' @export
YARFROC = function(X, y, 
		x_axis = "FPR", 
		y_axis = "TPR", 
		use_prop_data = 1,
		minimum_class_proportion = 0.02,
		desired_interval = 0.05, 
		tolerance = 0.001,
		y_axis_fine_resolution = FALSE,
		plot = TRUE, 
		verbose = TRUE, 
		...){
	
  x_axis = tolower(x_axis)
  y_axis = tolower(y_axis)
	
  #all the error checking
  if (x_axis == y_axis){
		stop("\"x_axis\" and \"y_axis\" must be different.")
  }
	if (!(x_axis %in% c("tpr","fnr","fpr","tnr","ppv","fdr","fomr","npv"))){
		stop("\"x_axis\" must be a legal metric.")
	}
	if (!(y_axis %in% c("tpr","fnr","fpr","tnr","ppv","fdr","fomr","npv"))){
		stop("\"y_axis\" must be a legal metric.")
	}
  if (identical(c(x_axis, y_axis), c("tpr", "fnr")) || 
      identical(c(x_axis, y_axis), c("fnr", "tpr")) ||
      identical(c(x_axis, y_axis), c("fpr", "tnr")) ||
      identical(c(x_axis, y_axis), c("tnr", "fpr")) ||
      identical(c(x_axis, y_axis), c("fomr", "npv")) ||
      identical(c(x_axis, y_axis), c("npv", "fomr")) ||
      identical(c(x_axis, y_axis), c("ppv", "fdr")) ||
      identical(c(x_axis, y_axis), c("fdr", "ppv"))
  ){
    stop("Trivial plot since y = 1 - x by definition.")
  }
	
	y_levels = levels(y)
	num_y_levels = length(y_levels)
	if (class(y) != "factor" || num_y_levels != 2){
		stop("Your response must be a factor with two levels.")
	}
	
	pos_class = y_levels[1]
	neg_class = y_levels[2]
	y_pos_indices = which(y == pos_class)
	y_neg_indices = which(y == neg_class)
	
	#let's nail the x-axis first

	
	
	ROCResults = R6Class("ROCResults",
    public = list(
      results = NULL,
      x_axis = NULL,
      y_axis = NULL,
      initialize = function(x_axis, y_axis) {
        self$x_axis = x_axis
        self$y_axis = y_axis
        self$results = data.frame(matrix(NA, nrow = 0, ncol = 3))
        colnames(self$results) = c(x_axis, y_axis, "p_pos")       
      },
      addResults = function(x) {
        self$results = rbind(self$results, x)
        colnames(self$results) = c(x_axis, y_axis, "p_pos") 
      }
    )
	)
	
	#pass by reference... easiest way I could think of unfortunately is to use R6...
	roc_results <- ROCResults$new(x_axis, y_axis)
	p_pos = 0.5
	YARF_OOB_for_proportion_pos_class_recursive(X, y, roc_results, y_pos_indices, y_neg_indices, use_prop_data, minimum_class_proportion, p_pos, desired_interval, x_axis, y_axis, FALSE, tolerance, plot, ...)
	YARF_OOB_for_proportion_pos_class_recursive(X, y, roc_results, y_pos_indices, y_neg_indices, use_prop_data, p_pos, 1 - minimum_class_proportion, desired_interval, x_axis, y_axis, FALSE, tolerance, plot, ...)
	if (y_axis_fine_resolution){
	  p_pos = 0.5 + rnorm(1, 0, 0.05) #seems reasonable to begin + a little noise to prevent x, y resolution points bunching up
	  YARF_OOB_for_proportion_pos_class_recursive(X, y, roc_results, y_pos_indices, y_neg_indices, use_prop_data, minimum_class_proportion, p_pos, desired_interval, y_axis, x_axis, TRUE, tolerance, plot, ...)
	  YARF_OOB_for_proportion_pos_class_recursive(X, y, roc_results, y_pos_indices, y_neg_indices, use_prop_data, p_pos, 1 - minimum_class_proportion, desired_interval, y_axis, x_axis, TRUE, tolerance, plot, ...)
	}
	#return the results ordered so it's easy to pick out the point that suits your needs
	roc_results$results[order(roc_results$results[, 1]), ]
}

YARF_OOB_for_proportion_pos_class_recursive = function(X, y, 
    roc_results, y_pos_indices, y_neg_indices, use_prop_data, 
		a, b, desired_interval, primary_metric, secondary_metric, switched,
		tolerance, plot, ...){
	
  #if we are splitting hairs, no point...
  if (abs(a - b) < tolerance){
    # cat("tolerance hit.\n")
    return()
  }
  
  cat("primary metric: ", primary_metric, "a: ", a, "b: ", b, "\n")
  n = length(y)
  bootstrap_indices_a = make_bootstrap_vecs(y_pos_indices, y_neg_indices, n, use_prop_data, a)
  bootstrap_indices_b = make_bootstrap_vecs(y_pos_indices, y_neg_indices, n, use_prop_data, b)
	
  mod_a = YARF(X, y, bootstrap_indices = bootstrap_indices_a, verbose = FALSE, ...)
	mod_b = YARF(X, y, bootstrap_indices = bootstrap_indices_b, verbose = FALSE, ...)
	
	a_primary_metric = assessModelMetric(mod_a, primary_metric)
	a_secondary_metric = assessModelMetric(mod_a, secondary_metric)
	b_primary_metric = assessModelMetric(mod_b, primary_metric)
	b_secondary_metric = assessModelMetric(mod_b, secondary_metric)
	
	#add this to our running list of results
  if (switched){
    roc_results$addResults(c(a_secondary_metric, a_primary_metric, a))
    roc_results$addResults(c(b_secondary_metric, b_primary_metric, b))    
  } else {
    roc_results$addResults(c(a_primary_metric, a_secondary_metric, a))
    roc_results$addResults(c(b_primary_metric, b_secondary_metric, b))    
  }

	
	print(roc_results$results)
	cat("metric diff", abs(a_primary_metric - b_primary_metric), "\n")
	
	if (plot){
	  print(ggplot(roc_results$results, aes_string(colnames(roc_results$results)[1], colnames(roc_results$results)[2])) + 
      xlim(0, 1) +
      ylim(0, 1) +
      geom_point() + 
      geom_smooth(method = 'loess', fill = NA))
	}
	
	if (is.nan(a_primary_metric) || is.nan(b_primary_metric)){ #we gotta jet now otherwise the below condition will fail
	  return()
	}
	if (abs(a_primary_metric - b_primary_metric) > desired_interval){
		#we gotta keep going!
		ab = mean(c(a, b))
		YARF_OOB_for_proportion_pos_class_recursive(X, y, roc_results, y_pos_indices, y_neg_indices, use_prop_data, a, ab, desired_interval, primary_metric, secondary_metric, switched, tolerance, plot, ...)
		YARF_OOB_for_proportion_pos_class_recursive(X, y, roc_results, y_pos_indices, y_neg_indices, use_prop_data, ab, b, desired_interval, primary_metric, secondary_metric, switched, tolerance, plot, ...)
	}
}

make_bootstrap_vecs = function(y_pos_indices, y_neg_indices, n, use_prop_data, p_pos){
	n_pos = round(p_pos * n)
	n_neg = n - n_pos
	# cat("n", n, "p_pos", p_pos, "n_pos", n_pos, "n_neg", n_neg, "\n")
	bootstrap_indices = list()
	B = 10000 #get("MAX_TREES", YARF_globals)
	for (b in 1 : B){
		bootstrap_indices[[b]] = c(
									sample(y_pos_indices, size = n_pos, replace = TRUE), 
									sample(y_neg_indices, size = n_neg, replace = TRUE)
								 )
	}
	
	bootstrap_indices
}


assessModelMetric = function(yarf_binary_mod, metric){
	#first obtain the confusion matrix
	conf = YARF_update_with_oob_results(yarf_binary_mod)$confusion_matrix
	switch(metric,
		"tpr" = conf[1, 1] / sum(conf[1, ]),
		"fnr" = conf[1, 2] / sum(conf[1, ]),
		"fpr" = conf[2, 1] / sum(conf[2, ]),
		"tnr" = conf[2, 2] / sum(conf[2, ]),
		"ppv" = conf[1, 1] / sum(conf[, 1]),
		"fdr" = conf[2, 1] / sum(conf[, 1]),
		"fomr" = conf[1, 2] / sum(conf[, 2]),
		"npv" = conf[2, 2] / sum(conf[, 2])
		#no default: throw an error!
	)
}


#' Calculates the area under the curve
#'
#' @param roc_results 		The results from the function \code{YARFROC}.
#' @param plot 				Make a plot of the ROC with the AUC shaded in? Default is \code{TRUE}.
#'
#' @return					The area under the curve approximated via numerical integration.
#' @export
calcAUC = function(roc_results, plot = TRUE){
  roc_results = roc_results[, 1 : 2]
  if (plot){
    print(ggplot(roc_results, aes_string(colnames(roc_results)[1], colnames(roc_results)[2])) + 
      geom_point() + 
      xlim(0, 1) +
      ylim(0, 1) +
      geom_smooth(method = 'loess', fill = NA) + 
      # stat_function(fun = function(x){x}, xlim = c(0, 1)) +
      geom_ribbon(aes(ymin = 0, ymax = predict(loess(roc_results[, 2] ~ roc_results[, 1]))), alpha = 0.3, fill = 'green'))
  }
  #do an approximate numerical integration
  sintegral(roc_results$fpr, roc_results$tpr)$int
}
