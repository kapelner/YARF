function oobCost(y_hat, y){
	var q = quantile_from_R;
	var q_C_neg = q - 1;
	var diff = y - y_hat;
	if (diff > 0){
		return q * diff;
	}
	else {
		return q_C_neg * diff;			
	}
}