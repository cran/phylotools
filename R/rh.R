rh <-
function(tree, times = 1)
{
    tree$node.label <- NULL
    h = (as.hclust(multi2di(tree))$height)
    hmax = max(h)
    hmin = min(h)
    res <- runif(hmin, hmax,n = times)
    return(res)
}

