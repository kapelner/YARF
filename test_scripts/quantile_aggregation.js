function aggregateYhatsIntoOneYhat(y_hat_trees, yarf){ //yarf unused
	var Percentile = Java.type('org.apache.commons.math3.stat.descriptive.rank.Percentile');
	var p = new Percentile();
//	print(" 0.1%", p.evaluate(y_hat_trees, 0.1));
//	print(" 1%", p.evaluate(y_hat_trees, 1));
//	print(" 5%", p.evaluate(y_hat_trees, 5));
//	print(" 10%", p.evaluate(y_hat_trees, 10));
//	print(" 20%", p.evaluate(y_hat_trees, 20));
//	print(" 30%", p.evaluate(y_hat_trees, 30));
//	print(" 40%", p.evaluate(y_hat_trees, 40));
//	print(" 50%", p.evaluate(y_hat_trees, 50));
//	print(" 60%", p.evaluate(y_hat_trees, 60));
//	print(" 70%", p.evaluate(y_hat_trees, 70));
//	print(" 80%", p.evaluate(y_hat_trees, 80));
//	print(" 90%", p.evaluate(y_hat_trees, 90));
//	print(" 99%", p.evaluate(y_hat_trees, 99));
//	print(" 100%", p.evaluate(y_hat_trees, 100));
//	
//	for each (y in Java.from(y_hat_trees).sort()){
//		print("  sorted", y);	
//	}

	return p.evaluate(y_hat_trees, quantile_from_R * 100); //gsub the value in
}