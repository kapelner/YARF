#see http://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode
sample_mode = function(arr) {
	ux = unique(arr)
	ux[which.max(tabulate(match(arr, ux)))]
}

