framsub <-
function(x, pattern = "-", replacement = "?"){
     result <- rep(NA, 2 * nrow(x))
     dim(result) <- c(nrow(x),2)
     for(i in 1:nrow(x)){
         result[i, 2] <- edgesub(xxxx = x[i, 2], pattern = pattern, replacement = replacement) 
     }
     result[,1] <- x[,1]
     return(result)
}

