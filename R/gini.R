gini <-
function (x) 
{
    n <- length(x)
    x <- sort(x)
    res <- 2 * sum(x * 1:n)/(n * sum(x)) - 1 - (1/n)
    return(res)
}

