RMPD <-
function(subtree, tree){
    mean(as.dist(cophenetic(subtree)))/mean(as.dist(cophenetic(tree)))
}

