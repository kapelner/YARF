.onLoad = function(libname, pkgname) {
	.jpackage(pkgname, lib.loc = libname)
	assign("YARF_globals", new.env(), envir = parent.env(environment()))
	assign("MAX_TREES", 10000, YARF_globals)
}

.onAttach = function(libname, pkgname){
  num_gigs_ram_available = .jcall(.jnew("java/lang/Runtime"), "J", "maxMemory") / 1e9
  packageStartupMessage(
	paste("Welcome to YARF v", VERSION, 
			"! You have ", round(num_gigs_ram_available, 2), 
			"GB memory available.\n\n", 
			"If you run out of memory, restart R, and use e.g.\n'options(java.parameters = \"-Xmx5g\")' for 5GB of RAM before you call\n'library(YARF)'.\n\n",
			"By default, YARF uses all available CPU cores save one.\nYou can change this via \"set_YARF_num_cores()\".\n",
			sep = "")
  )
  #as a default, assign all cores save 1
  num_cores = as.numeric(Sys.getenv('NUMBER_OF_PROCESSORS'))
  set_YARF_num_cores(max(num_cores - 1, 1))
}