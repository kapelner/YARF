#' Prunes the trees in a YARF mode. This is an irreversible modification to 
#' the tree structure.
#' 
#' @param yarf_mod 			The YARF model whose trees are to be pruned.
#' @param prune_if_script	A custom javascript function (see below) which determines the pruning of 
#' 							a node (i.e. deletes the node's children and sets the node to a leaf and 
#' 							then sets a y_hat using the assign function).
#' 
#' 							function pruneIf(node)\{ //node is of type YARF.YARFNode
#' 
#' 								...
#' 								
#' 								return boolean //true if this node is to be pruned
#' 							\}
#' 
#' @param trees_to_prune 	An array of indices which specify which of the trees to prune. The default is \code{NULL}
#' 							which means all of them.
#' 
#' @author Adam Kapelner
#' @export
prune_YARF_model = function(yarf_mod, prune_if_script, trees_to_prune = NULL){
	assertClass(yarf_mod, "YARF")
	assertCharacter(prune_if_script, null.ok = TRUE)
	assertStringContains(prune_if_script, "function pruneIf(node){")
	
	.jcall(java_YARF, "V", "setPrune_if_function_str", prune_if_script)
	
	if (is.null(trees_to_prune)){
		trees_to_prune = 1 : yarf_mod$num_trees
	}
	
	#error check on trees_to_prune
	if (length(setdiff(trees_to_prune, 1 : yarf_mod$num_trees)) > 0){
		stop("\"trees_to_prune\" must be a subset of {1, 2, ..., num_trees}")
	}
	
	for (tree in trees_to_prune){
		.jcall(java_YARF, "V", "pruneTree", as.integer(tree))
	}
}