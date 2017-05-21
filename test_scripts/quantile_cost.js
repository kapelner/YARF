function nodeCost(node){
	var q = quantile_from_R;
	var q_C_neg = q - 1;
	//assign the value now and not have to run the same operation twice
	node.y_pred = node.y_quantile(q * 100);
	var cost = 0;
	for each (y_i in node.node_ys()){
		var diff = y_i - node.y_pred;
		if (diff > 0){
			cost += q * diff;
		}
		else {
			cost += q_C_neg * diff;			
		}
//		print("   y_i:", y_i, "y_q:", y_q, "diff: ", diff, "cost", cost);
	}
//	print("nodeCost y_q:", y_q, "cost:", cost, "ys: ", Java.from(node.node_ys()));
	return cost;
}