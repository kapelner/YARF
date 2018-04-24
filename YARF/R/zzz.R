.onLoad = function(libname, pkgname) {
	.jpackage(pkgname, lib.loc = libname)
	assign("YARF_globals", new.env(), envir = parent.env(environment()))
	assign("MAX_TREES", 10000, YARF_globals)
}

.onAttach = function(libname, pkgname){
  num_gigs_ram_available = .jcall(.jnew("java/lang/Runtime"), "J", "maxMemory") / 1e9
  
  #as a default, assign all cores save 1 or tell the user it cannot be detected
  num_cores = as.numeric(Sys.getenv('NUMBER_OF_PROCESSORS'))
  num_cores_message = "Parallelization is by default and YARF uses all available CPU cores save one.\nYou can change this via \"set_YARF_num_cores()\".\n"
  if (is.na(num_cores)){
	  num_cores_message = "YARF was unable to detect the number of CPU cores.\nYou can specify the number of cores available via \"set_YARF_num_cores()\" if you want parallelization.\n"
  } 
  
  packageStartupMessage(
	paste("Welcome to YARF v", VERSION, 
			"! You have ", round(num_gigs_ram_available, 2), 
			"GB memory available.\n\n", 
			"If you run out of memory, restart R, and use e.g.\n'options(java.parameters = \"-Xmx5g\")' for 5GB of RAM before you call\n'library(YARF)'.\n",
			num_cores_message,
			sep = "")
  )
  if (!is.na(num_cores)){
	  set_YARF_num_cores(max(num_cores - 1, 1))
  }
  
}