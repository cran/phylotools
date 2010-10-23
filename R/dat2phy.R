dat2phy <-
function(input, write = TRUE)
{
   row1.1 <- nrow(input)
   row1.2 <- nchar(input[1,2])
   row1 <- paste(row1.1, row1.2)
   space <- as.vector(11 - nchar(input[,1]))
   res <- c()
   for(i in 1:length(space)){
        res[i] <- paste(input[i,1],paste(rep(" ", 
		          space[i]), collapse = ""), input[i,2])
   }
   
   res <- c(row1, res)
   return(res)
}

