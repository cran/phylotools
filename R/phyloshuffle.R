phyloshuffle <-
function(tree){
    labels <- tree$tip.label
    randmz.label <- sample(labels)
    tree$tip.label <- randmz.label
    return(tree)
}

