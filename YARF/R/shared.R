assertStringContains = function(str, contains, null.ok = TRUE){
	if (null.ok & is.null(str)){
		TRUE
	} else if (!null.ok & is.null(str)){
		FALSE
	}
}