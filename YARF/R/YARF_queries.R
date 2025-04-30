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
	assertClass(yarf_mod, "YARF")
	check_serialization(yarf_mod) #ensure the Java object exists and fire an error if not
	assertChoice(type, c("splits", "trees"))
	
	if (!(type %in% c("trees", "splits"))){
		stop("type must be \"trees\" or \"splits\"")
	}
	count_matrix = .jcall(yarf_mod$java_YARF, "[I", "getCountsForAllAttribute", type, simplify = TRUE)
	names(count_matrix) = yarf_mod$training_data_features
	count_matrix
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
	assertClass(yarf_mod, "YARF")
	check_serialization(yarf_mod) #ensure the Java object exists and fire an error if not
	assertChoice(type, c("splits", "trees"))
	
	counts = query_variable_counts(yarf_mod, type)
	counts / sum(counts)
}


#' Investigate First Order Interactions
#' 
#' This function investigates all two-way interactions in the forest by measuring how many times splits of one
#' variable is nested in splits of another variable.
#' 
#' @param yarf_mod			The YARF model in which to query
#' @param plot 				This controls whether or not the function plots the interaction strengths. If so, it plots 
#' 							in order of strength from highest to lowest. Default is \code{TRUE}.
#' @param only_non_self_interactions	Should we only show interactions between different variables? Default is \code{FALSE}
#' 										to show interactions of variables with themselves.
#' @param num_var_plot 		How many first order interactions to plot? All would be p-choose-2 which
#' 							can be large. The default is \code{50}. Use \code{Inf} to plot all p-choose-2.
#' @return 					A list with two components: (1) \code{interaction_counts} is a p x p sized matrix
#' 							of interaction counts between the variables and (2) \code{sorted_counts} is a vector of
#' 							the top interactions in order. Returns invisibly.
#' 
#' @author Adam Kapelner
#' @export
first_order_interaction_investigator = function(yarf_mod, plot = TRUE, only_non_self_interactions = FALSE, num_var_plot = 50){
	assertClass(yarf_mod, "YARF")
	check_serialization(yarf_mod) #ensure the Java object exists and fire an error if not
	assertLogical(plot)
	assertLogical(only_non_self_interactions)
	if (num_var_plot != Inf){
		assertCount(num_var_plot, positive = TRUE)
	}	
	
	interaction_counts = .jcall(yarf_mod$java_YARF, "[[I", "getInteractionCounts", simplify = TRUE)
	rownames(interaction_counts) = yarf_mod$training_data_features
	colnames(interaction_counts) = yarf_mod$training_data_features
	
	if (only_non_self_interactions){
		diag(interaction_counts) = NA
	}
	
	#now vectorize the interaction counts
	counts = array(NA, yarf_mod$p * (yarf_mod$p + 1) / 2)
	iter = 1
	for (i in 1 : yarf_mod$p){
		for (j in 1 : yarf_mod$p){
			if (j >= i){
				counts[iter] = interaction_counts[i, j]
				names(counts)[iter] = paste(rownames(interaction_counts)[i], "x", rownames(interaction_counts)[j])
				iter = iter + 1
			}
		}
	}

	
	sorted_counts = sort(counts, decreasing = TRUE)
	if (num_var_plot == Inf || num_var_plot > num_total_interactions){
		num_var_plot = length(sorted_counts)
	}
	sorted_counts = sorted_counts[1 : num_var_plot]
	
	if (plot){
		ggplot_df = data.frame(
				interaction = names(sorted_counts),
				relative_importance = sorted_counts
		)
		ggplot_df$interaction = factor(ggplot_df$interaction, levels = ggplot_df$interaction)
		ggplot_df = ggplot_df[ggplot_df$relative_importance > 0, ]
		
		plot(ggplot(ggplot_df) + 
			geom_bar(aes(x = interaction, y = relative_importance), stat = "identity") +
			theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)))
	}
	
	invisible(list(interaction_counts = interaction_counts, sorted_counts = sorted_counts))
}



