options(java.parameters = c("-Xmx4000m", "-Xss5m"))
pacman::p_load(YARF)


X = MASS::Boston[, 1 : 13]
y = MASS::Boston[, 14]

tree_mod = YARFCARTSTUMP(X, y)
get_tree_num_nodes_leaves_max_depths(tree_mod)
illustrate_trees(tree_mod, open_file = TRUE)
# prediction_nodes(tree_mod)
node_java = .jcall(tree_mod$java_YARF, "LYARF/YARFNode;", "getNode", as.integer(0), "LRRR", simplify = TRUE)

raw_node_data(tree_mod)

