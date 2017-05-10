#' Returns the leaf nodes that are responsible for predictions for a given dataset. 
#' 
#' @param yarf_mod 					A YARF model object.
#' @param X 				        A n* x p matrix where each row is an observation you wish to obtain prediction information. Here,
#' 									p is the same length as the original training data. If \code{NULL} (the default), 
#' 									then this parameter is set to the model's training data.
#' @param oob_only          		If \code{X} is \code{NULL}, setting this to \code{TRUE} (the default) will only return node information
#' 									for trees where each observation is out-of-bag. If \code{X} is not \code{NULL}, this
#' 									parameter has no effect on output.
#' @return 							A list where values are rJava YARFNode objects for all the prediction nodes. The first key 
#' 									indexes the row number in \code{X} and the second, the tree number.
#'                          
#' 
#' @author Kapelner
#' @export
prediction_nodes = function(yarf_mod, X = NULL, oob_only = TRUE){
	check_serialization(yarf_mod) #ensure the Java object exists and fire an error if not

	nodes = list()	
	#if the user just wants nodes oob for the training data's prediction nodes
	if (is.null(X) && oob_only){
		nodes_matrix = get_nodes_matrix(yarf_mod$X, yarf_mod)
		
		for (i in 1 : nrow(nodes_matrix)){
			oob_nodes = list()
			for (t in 1 : length(yarf_mod$bootstrap_indices)){
				if (i %in% yarf_mod$bootstrap_indices[[t]]){
					oob_nodes[[t]] = nodes_matrix[i, t][[1]]
				}
			}
			nodes[[i]] = oob_nodes
		}
	} else { #otherwise give them all nodes for all trees
		
		if (class(X) != "data.frame"){		
			stop("\"X\" needs to be a data frame with the same column names as the training data.")
		}
		if (!yarf_mod$use_missing_data){
			nrow_before = nrow(X)
			X = na.omit(X)
			if (nrow_before > nrow(X)){
				cat(nrow_before - nrow(X), "rows omitted due to missing data. Try using the missing data feature when building YARF to be able to predict on all observations.\n")
			}
		}
		
		if (nrow(X) == 0){
			stop("No rows to predict.\n")
		}
		
		#now process and make dummies if necessary

		nodes_matrix = get_nodes_matrix(X, yarf_mod)
		
		for (i in 1 : nrow(nodes_matrix)){
			nodes[[i]] = as.list(nodes_matrix[i, ])
		}
	}
	
	nodes
}

get_nodes_matrix = function(X, yarf_mod){	
	#important to release pointers
	gc()
	num_cores = as.integer(get("YARF_NUM_CORES", YARF_globals))	
	
	X = pre_process_new_data(X, yarf_mod)
#	nodes_matrix = .jcall(yarf_mod$java_YARF, "[[LYARF/YARFNode;", "predictNodes", .jarray(X, dispatch = TRUE), as.integer(num_cores))
	t(sapply(.jcall(yarf_mod$java_YARF, "[[LYARF/YARFNode;", "predictNodes", .jarray(X, dispatch = TRUE), as.integer(num_cores)), .jevalArray))
}

#' Computes information about the "proximity" of observations within the YARF model. Given two datasets,
#' information is computed for all pairs of observations. Information returned is the prediction nodes (for all trees) 
#' for both objects and their common node in the tree structure plus much information about the nodes.   
#' 
#' @param yarf_mod 					A YARF model object.
#' @param X1 				        A n* x p matrix where each row is an observation. If \code{NULL} (the default), 
#' 									then this parameter is set to the model's training data.
#' @param X2 				        A n x p matrix where each row is an observation. If \code{NULL} (the default), 
#' 									then this parameter is set to the model's training data.
#' @param oob_only_1          		If \code{X1} is \code{NULL}, setting this to \code{TRUE} (the default) 
#' 									will only return proximity information for trees where each observation is out-of-bag. 
#' 									If \code{X1} is not \code{NULL}, this parameter has no effect on output.
#' @param oob_only_2          		If \code{X2} is \code{NULL}, setting this to \code{TRUE} (the default) 
#' 									will only return proximity information for trees where each observation is out-of-bag. 
#' 									If \code{X2} is not \code{NULL}, this parameter has no effect on output.
#' @verbose							Print out periodic messages on progress of the computation of the proximity information.
#' 									The default is \code{TRUE}.
#' @return 							A list indexed by the row number of \code{X1} whose elements are a list indexed by
#' 									the row number of \code{X2} whose elements are a list indexed by tree number whose
#' 									elements are a list consisting of the following information: (a) X1 prediction node 
#' 									(b) X2 prediction node, (c) shared node, (d) locations of all three nodes indicating
#' 									left/right directions, (e) depth differences to shared node where 0 indicates they
#' 									are in the same node, (f) parent node's cost for all three (where the parent cost
#' 									of the root is the null model cost)
#'                          
#' 
#' @author Kapelner
#' @export
compute_raw_proximity_info = function(yarf_mod, X1 = NULL, X2 = NULL, oob_only_1 = TRUE, oob_only_2 = TRUE, verbose = TRUE){
	## error checks
	check_serialization(yarf_mod) #ensure the Java object exists and fire an error if not
	
	#important to release pointers
	gc()
	
	#get all nodes for both datasets
	if (is.null(X1)){
		nodes1 = prediction_nodes(yarf_mod, NULL, oob_only_1)
		X1 = yarf_mod$X
	} else { #error checking
		if (class(X1) != "data.frame"){		
			stop("\"X1\" needs to be a data frame with the same column names as the training data.")
		}
		if (!yarf_mod$use_missing_data){
			nrow_before = nrow(X1)
			X1 = na.omit(X1)
			if (nrow_before > nrow(X1)){
				cat(nrow_before - nrow(X1), "rows omitted in X1 due to missing data. Try using the missing data feature when building YARF to be able to predict on all observations.\n")
			}		
		}
		if (nrow(X1) == 0){
			stop("No rows to return proximity information in X1.\n")
		}
		nodes1 = prediction_nodes(yarf_mod, X1)
	}
	#get all nodes for both datasets
	if (is.null(X2)){
		if (is.null(X1) && oob_only_1 == oob_only_2){ #they will be the same, so no use running the function again
			nodes2 = nodes1
		} else {
			nodes2 = prediction_nodes(yarf_mod, NULL, oob_only_2)
		}
		X2 = yarf_mod$X
	} else { #error checking
		if (class(X2) != "data.frame"){		
			stop("\"X2\" needs to be a data frame with the same column names as the training data.")
		}
		if (!yarf_mod$use_missing_data){
			nrow_before = nrow(X2)
			X2 = na.omit(X2)
			if (nrow_before > nrow(X2)){
				cat(nrow_before - nrow(X2), "rows omitted in X1 due to missing data. Try using the missing data feature when building YARF to be able to predict on all observations.\n")
			}		
		}
		if (nrow(X2) == 0){
			stop("No rows to return proximity information in X2.\n")
		}
		nodes2 = prediction_nodes(yarf_mod, X2)
	}
	
	#check to see if X1 and X2 are the same and if so, only create a diagonal matrix (for efficiency)
	X1_X2_identical = TRUE
	if (nrow(X1) != nrow(X2) || ncol(X1) != ncol(X2)){
		X1_X2_identical = FALSE
	} else {
		for (i in 1 : nrow(X1)){
			if (!identical(X1[i, ], X2[i, ])){
				X1_X2_identical = FALSE
				break
			}
		}
	}
	
	#begin building the results objects
	proximities = list()
	for (i in 1 : nrow(X1)){
		proximities[[i]] = list()
		
		for (j in 1 : nrow(X2)){
			
			if (X1_X2_identical && j >= i){
				next
			}
			
			proximities[[i]][[j]] = list()
			
			for (t in 1 : yarf_mod$num_trees){
				#get node information and jump ship if its null
			  
				if (t > length(nodes1[[i]]) || t > length(nodes2[[j]])){
					next
				}
				node1 = nodes1[[i]][[t]]
				node2 = nodes2[[j]][[t]]				
				if (is.null(node1) || is.null(node2)){
					next
				}
				#start populating
				res = list()
				res$node1 = node1
				res$node2 = node2
				res$node1_location = .jcall(node1, "S", "stringLocation")
				res$node2_location = .jcall(node2, "S", "stringLocation")
				res$same_node = identical(res$node1_location, res$node2_location)
				res$node1_depth = node1$depth
				res$node2_depth = node2$depth
				res$node1_cost = .jcall(node1, "D", "cost")
				res$node2_cost = .jcall(node2, "D", "cost")
				res$node1_parent_cost = .jcall(node1, "D", "parentCost")
				res$node2_parent_cost = .jcall(node2, "D", "parentCost")
				res$node1_size = .jcall(node1, "I", "nodeSize")
				res$node2_size = .jcall(node2, "I", "nodeSize")
				res$shared_node_location = shared_initial_substring(res$node1_location, res$node2_location)
				res$node1_depth_diff = nchar(res$node1_location) - nchar(res$shared_node_location)
				res$node2_depth_diff = nchar(res$node2_location) - nchar(res$shared_node_location)
				
				#get shared node without java
				res$shared_node = node1
				if (res$node1_depth_diff > 0){
					for (d in 1 : res$node1_depth_diff){
						res$shared_node = res$shared_node$parent
					}
				}				
				
				res$shared_node_depth = res$shared_node$depth
				res$shared_node_cost = .jcall(res$shared_node, "D", "cost")
				res$shared_node_parent_cost = .jcall(res$shared_node, "D", "parentCost")
				res$shared_node_size = .jcall(res$shared_node, "I", "nodeSize")
				
				proximities[[i]][[j]][[t]] = res
			}
		}
		if (verbose){
		  cat("Row ", i, " of ", nrow(X1), " i.e. ", round(i / nrow(X1) * 100, 1), "% complete.\n", sep = "")
		}		
	}
	proximities
}

#' Computes information about the "proximity" of observations within the YARF model. Given two datasets,
#' information is computed for all pairs of observations. Information is averaged over the trees in the model.   
#' 
#' @param raw_proximity_info 		The information returned from the \code{compute_raw_proximity_info} function.
#' @return 							A list indexed by the row number of \code{X1} whose elements are a list indexed by
#' 									the row number of \code{X2} whose elements are an array consisting of the following 
#' 									information: (a) proportion of trees these two observations were in the same node,
#' 									(b) average node 1/2/shared depth (c) average node 1/2/shared cost (d) average node 
#' 									1/2/shared parent cost (e) average node 1/2/shared size (f) average depth difference 
#' 									between node 1/2 and shared
#'                          
#' 
#' @author Kapelner
#' @export
tree_average_proximity_info = function(raw){
	tree_average_proximity_info = list()
	for (i in 1 : length(raw)){
		raw_i = raw[[i]]
		tree_average_proximity_info[[i]] = list()
		for (j in 1 : length(raw_i)){
			raw_i_j = raw_i[[j]]
			
			res = as.list(sapply(
					c(
						"same_node",
						"node1_depth", 
						"node2_depth", 
						"shared_node_depth",
						"node1_depth_diff", 
						"node2_depth_diff", 
						"node1_size",
						"node2_size",
						"shared_node_size",
						"node1_cost", 
						"node2_cost", 
						"shared_node_cost",
						"node1_parent_cost", 
						"node2_parent_cost", 
						"shared_node_parent_cost"
					), 
					function(key) { 
						vals = array(NA, length(raw_i_j))
						for (t in 1 : length(raw_i_j)){
							raw_i_j_t = raw_i_j[[t]]
							if (!is.null(raw_i_j_t)){
								vals[t] = raw_i_j_t[[key]]
							}					
						}
						mean(vals, na.rm = TRUE)
					}
				)
			)
			
			tree_average_proximity_info[[i]][[j]] = res
		}
	}
	tree_average_proximity_info
}

shared_initial_substring = function(s1, s2){
	for (i in 1 : nchar(s1)){
		if (identical(substr(s1, 1, i), substr(s2, 1, i))){
			next
		} else {
			i = i - 1
			break
		}		
	}
	substr(s1, 1, i)
}