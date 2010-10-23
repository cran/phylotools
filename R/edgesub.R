edgesub <-
function(xxxx, pattern = "-", replacement = "?"){
	loc <- fmatch(dna = xxxx, pattern = pattern)
	newdna <- paste(paste(rep(x = replacement, times = loc), collapse = ""), 
			  substring(xxxx, loc+1, nchar(xxxx)), sep = "")		  
	revdna <- reverse(newdna)		  
	locrev <- fmatch(revdna, pattern = pattern)		  
	revdnaresult <- paste(paste(rep(x = replacement, times = locrev), 
	       collapse = ""), substring(revdna, locrev+1, nchar(newdna)), sep = "")
	dnaresult <- reverse(revdnaresult)
	return(dnaresult )
}

