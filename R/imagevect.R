imagevect <- 
function (x, labels, contour = FALSE, gridsize = 20, axes = TRUE, nlabx = 5, nlaby = 5, ...) 
{
    sort.x <- x[order(formatXY(labels))]
    rrr <- dimension(labels, unique = TRUE, sort = TRUE)
    dims <- c(length(rrr[[2]]), length(rrr[[1]]))
    dim(sort.x) <- dims
    par(xaxs = "i", yaxs = "i")
    image.plot(nnn <- t(sort.x), axes = FALSE, ...)

    if (contour) {
        contour(nnn, add = TRUE, ...)
    }
    if (axes) {
        points(0, 0, pch = " ", cex = 3)
        
     get.axis.ticks <- 
     function(nlabs = NULL, gridsize = NULL, limit_max = NULL){
         ngrid <- (limit_max-0)/gridsize
         per_grid <- 1/((limit_max-0)/gridsize)
         n_small <- 1+1/((limit_max-0)/gridsize)
         start <- 0 - per_grid/2
         stop <- 1+per_grid/2
         
         lab <-c(0, (1:nlabs)*(limit_max/nlabs), limit_max)
         at <- c(start, (1:nlabs)*(stop - start)/nlabs, stop)
         
         return(list(lab, at))
     }
       xaxis.position <- get.axis.ticks(nlabs = nlabx, gridsize = 20, limit_max = gridsize * nrow(nnn))
       yaxis.position <- get.axis.ticks(nlabs = nlaby, gridsize = 20, limit_max = gridsize * ncol(nnn))
        
        axis(1, labels = xaxis.position[[1]], at = xaxis.position[[2]])
        axis(2, labels = yaxis.position[[1]], at = yaxis.position[[2]])
    }
    invisible(nnn)
}
