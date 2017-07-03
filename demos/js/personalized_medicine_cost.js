function nodeCost(node){
	var y_bar_T = getAvgByTrt(node, 1);
	var y_bar_C = getAvgByTrt(node, 0);
	node.y_pred = y_bar_T - y_bar_C; //assign prediction value here
	return -Math.abs(node.y_pred); //always negative, and the more negative the better
}

function getAvgByTrt(node, tx){
//	print("getAvgByTrt" + node + "tx" + tx);
	var node_tx = Java.from(node.node_X_others());
	
	var sum_y = 0.0;
	var n_tx = 0;
//	print("node_tx" + node_tx + "n" + n);
	var ys = Java.from(node.node_ys());
	for (i = 0; i < node.nodeSize(); i++){
		if (node_tx[i, 0] == tx){
			sum_y += ys[i];
			n_tx += 1;
		}
	}
	if (n_tx == 0){
		return 0;
	}
	return sum_y / n_tx;
}