plotcut <-
function(tree, n)
{
    nnn <- rh(tree, times = n)
    plot(tree)
    axis(1)
    tree$node.label <- NULL
    maxh <- max(as.hclust(multi2di(tree))$height)
    for(i in 1:length(nnn)){
        abline(v = (maxh)- nnn[i], col = 2)
    }
}

