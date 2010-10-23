fmatch <-
function(dna, pattern = ""){
    if(nchar(dna) == 1){
	    stop("The input string contained only one character.")
	}
	if(pattern == ""){
	    stop("You have to specify the regular expression pattern.")
	}
	splitdna = substring(dna, 1:nchar(dna), 1:nchar(dna))
    logic = grepl(pattern, splitdna)
    result = c()
    for(i in 1:length(logic)){
        if(i == 1){
           first = logic[i]
    	}
        if(i > 1){
           first = c(first, logic[i])
    	}
    	result[i] = (all(first))
    	if(!all(result)){
    	    return(length(result)-1)
    	}
    }
}
