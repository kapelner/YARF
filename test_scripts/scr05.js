function nodeCost(node){
	var ys = Java.from(node.node_ys());
	//print("ys:", ys)
	var y_avg = sample_avg(ys);
  	var sse = 0.0;
  	for (i = 0; i < ys.length; i++){
    	sse += Math.pow(ys[i] - y_avg, 2);
  	}
  	//print("y_avg:", y_avg)
  	//print("sae:", sae)
  	return sse;
}