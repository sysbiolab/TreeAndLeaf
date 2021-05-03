##This function is used for argument checking
tal.checks <- function(name, para){
    if(identical(name, "tal")) {
        if(!is.tal(para)){
            stop("'tal' must be an 'igraph' object created by the 
                 TreeAndLeaf() function!", call.=FALSE)
        }
    } else if(identical(name, "theme")){
        if(!is.singleInteger(para) || para > 5 || para < 1){
            stop("'theme' must be an integer ranging from 1 to 5!", 
                    call.=FALSE)
        }
    } else if(identical(name, "obj")){
        if (!any(class(para)%in%c("hclust","phylo","ggtree")))
            stop("'obj' must be a 'hclust', 'phylo', or 'ggtree' objects!", 
                 call.=FALSE)
    } else {
        stop("'name' option not available!", call.=FALSE)
    }
}

##------------------------------------------------------------------------------
is.tal <- function(para){
    "tal" %in% class(para)
}
is.singleInteger <- function(para){
    lg <- (is.integer(para) || is.numeric(para)) && length(para)==1L && 
        !is.na(para)
    if(lg) lg <- (para / ceiling(para)) == 1
    return(lg)
}
is.singleString <- function(para){
    is.character(para) && length(para) == 1L && !is.na(para)
}
is.singleLogical <- function(para){
    is.logical(para) && length(para) == 1L && !is.na(para)
}
