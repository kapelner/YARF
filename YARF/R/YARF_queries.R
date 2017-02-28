#' Function which queries the YARF model about how many times the variables are used when splitting
#' 
#' @param yarf_mod 	The model in which to query
#' @param type 		Either "\code{trees}" which counts the number of trees where each variable was used one (or more) times
#' 					or "\code{splits}" which counts the number of times each variable was used across the whole model.
#' @return 			A vector with length \code{p}, the number of variables considered in this YARF model, with either a binary
#' 					value for \code{type = "trees"} or a count value for \code{type = "splits"}.
#' 
#' @author Adam Kapelner
#' @export
query_variable_counts = function(yarf_mod, type = "splits"){
	check_serialization(yarf_mod) #ensure the Java object exists and fire an error if not
	
	if (!(type %in% c("trees", "splits"))){
		stop("type must be \"trees\" or \"splits\"")
	}
	C = t(.jcall(yarf_mod$java_YARF, "[I", "getCountsForAllAttribute", type), .jevalArray)
	names(C) = yarf_mod$training_data_features
	C
}

#' Function which queries the YARF model about the proportion of times the variables are used when splitting.
#' This is a convenience method which merely normalizes the result of \code{query_variable_counts}.
#' 
#' @param yarf_mod 	The model in which to query
#' @param type 		Either "\code{trees}" which computes the proportion of trees where each variable was used one (or more) times
#' 					or "\code{splits}" which computes the proportion of times each variable was used across the whole model.
#' @return 			A vector with length \code{p}, the number of variables considered in this YARF model, with values between
#' 					0 and 1.
#' 
#' @author Adam Kapelner
#' @export
query_variable_proportions_across_trees = function(yarf_mod, type = "splits"){
	counts = query_variable_counts(yarf_mod, type)
	counts / sum(counts)
}


#' This function investigates all two-way interactions in the forest by measuring how many times splits of one
#' variable is nested in splits of another variable.
#' 
#' @param yarf_mod			The YARF model in which to query
#' @param plot 				This controls whether or not the function plots the interaction strengths. If so, it plots 
#' 							in order of strength from highest to lowest. Default is \code{TRUE}.
#' @param num_var_plot 		How many first order interactions to plot? All would be p-choose-2 which
#' 							can be large. The default is \code{50}. Use \code{Inf} to plot all p-choose-2.
#' @param cut_bottom_prop 	Cut the bottom of the barplot off at a percent of the minimum interaction strength. Default is
#' 							\code{NULL} for no cutting.
#' @param bottom_margin 	Allow for larger bottom margins. This is useful when the variables have long names.
#' @param ... 				Additional parameters to be passed into the \code{barplot} function.
#' @return 					A list with two components: (1) \code{interaction_counts} is a p x p sized matrix
#' 							of interaction counts between the variables and (2) \code{sorted_counts} is a vector of
#' 							the top interactions in order. Returns invisibly.
#' 
#' @author Adam Kapelner
#' @export
first_order_interaction_investigator = function(yarf_mod, plot = TRUE, num_var_plot = 50, cut_bottom_prop = NULL, bottom_margin = 10, ...){
	check_serialization(yarf_mod) #ensure the Java object exists and fire an error if not
	
	interaction_counts = sapply(.jcall(yarf_mod$java_YARF, "[[I", "getInteractionCounts"), .jevalArray)
	rownames(interaction_counts) = yarf_mod$training_data_features
	colnames(interaction_counts) = yarf_mod$training_data_features
	
	#now vectorize the interaction counts
	counts = array(NA, yarf_mod$p * (yarf_mod$p - 1) / 2)
	iter = 1
	for (i in 1 : yarf_mod$p){
		for (j in 1 : yarf_mod$p){
			if (j <= i){
				counts[iter] = interaction_counts[i, j]
				names(counts)[iter] = paste(rownames(interaction_counts)[i], "x", rownames(interaction_counts)[j])
				iter = iter + 1
			}
		}
	}
	num_total_interactions = yarf_mod$p * (yarf_mod$p + 1) / 2
	if (num_var_plot == Inf || num_var_plot > num_total_interactions){
		num_var_plot = num_total_interactions
	}
	
	sorted_indices = sort(counts, decreasing = TRUE, index.return = TRUE)$ix
	sorted_counts = counts[sorted_indices][1 : num_var_plot]
	
	if (is.null(cut_bottom_prop)){
		ylim_bottom = 0
	} else {
		ylim_bottom = cut_bottom_prop * min(sorted_counts)
	}
	if (plot){
		#now create the bar plot
		par(mar = c(bottom_margin, 6, 3, 0))
		bars = barplot(sorted_counts, 
				names.arg = names(sorted_counts), 
				las = 2, 
				ylab = "Relative Importance", 
				col = "gray",#rgb(0.39, 0.39, 0.59),
				ylim = c(ylim_bottom, max(sorted_counts)),
				xpd = FALSE, ...
		)	
	}
	
	invisible(list(interaction_counts = interaction_counts, sorted_counts = sorted_counts))
}


#' Gets information about each tree: the number of nodes, number of leaves and maximum depth.
#' 
#' @param yarf_mod		The YARF model in which to query 
#' @return 				A list with three integer vector components all of which have length equal to the 
#' 						number of trees completes: (1) \code{num_nodes} has entries equal to the number of nodes
#' 						in each tree, (2) \code{num_leaves} has entries equal to the number of leaves (terminal
#' 						nodes) in each tree and (3) \code{max_depths} has entries equal to the maximum depth of
#' 						each tree.
#' 
#' @author Adam Kapelner
#' @export
get_tree_num_nodes_leaves_max_depths = function(yarf_mod){
	list(
		num_nodes = sapply(.jcall(yarf_mod$java_YARF, "[I", "getNumNodes"), .jevalArray),
		num_leaves = sapply(.jcall(yarf_mod$java_YARF, "[I", "getNumLeaves"), .jevalArray),
		max_depths = sapply(.jcall(yarf_mod$java_YARF, "[I", "getMaxDepths"), .jevalArray)
	)
}

