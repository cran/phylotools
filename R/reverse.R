reverse <-
function(dna){
     splitstr <- substring(dna,1:nchar(dna),1:nchar(dna))
     result <- paste(splitstr[length(splitstr):1], collapse = "")
     return(result)
}

