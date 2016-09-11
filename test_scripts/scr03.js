function nodeCost(node){
	var ys = Java.from(node.node_ys());
	//print("ys:", ys)
	var y_avg = sample_avg(ys);
  	var sae = 0.0;
  	for (i = 0; i < ys.length; i++){
    	sae += Math.abs(ys[i] - y_avg);
  	}
  	//print("y_avg:", y_avg)
  	//print("sae:", sae)
  	return sae;
}