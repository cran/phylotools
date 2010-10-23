write.mat <- 
function(supermat, file = NULL){
	if(is.null(file)){
	    stop("File name have to be specified.")
	}
    res <- dat2phy(supermat)	   
    writeLines(res, file)
}
