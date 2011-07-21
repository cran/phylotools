resid.tree <-
function(tree, deltree){
    drop.tip(tree, deltree$retain)
}

