.onLoad = function(libname, pkgname) {
  .jpackage(pkgname, lib.loc = libname)
  assign("YARF_globals", new.env(), envir = parent.env(environment()))
  assign("YARF_NUM_CORES", 1, YARF_globals) #Stay conservative as a default
}

.onAttach = function(libname, pkgname){
  num_gigs_ram_available = .jcall(.jnew("java/lang/Runtime"), "J", "maxMemory") / 1e9
  packageStartupMessage(
	paste("Welcome to YARF v", VERSION, 
			"! You have ", round(num_gigs_ram_available, 2), 
			"GB memory available.\n\n", 
			"If you run out of memory, restart R, and use e.g.\n'options(java.parameters = \"-Xmx5g\")' for 5GB of RAM before you call\n'library(YARF)'.\n",
			sep = "")
  )
}