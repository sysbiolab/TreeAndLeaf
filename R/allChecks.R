##This function is used for argument checking
tal.checks <- function(name, para){
    if(name == "gg") {
        if(!igraph::is.igraph(para)){
            stop("NOTE: 'gg' must be an igraph!", call.=FALSE)
        } else if (is.null(para$intnodes)){
            stop("NOTE: 'gg' must be created by phylo2igraph() 
                    or hclust2igraph()!", call.=FALSE)
        }
    } else if(name == "theme"){
        if(!is.singleInteger(para) || para > 5 || para < 1){
            stop("NOTE: 'theme' must be an integer ranging from 1 to 5!", 
                    call.=FALSE)
        }
    } else if(name == "cleanalias"){
        if(!is.singleLogical(para)){
            stop("NOTE: 'cleanalias' must be a logical value!", call.=FALSE)
        }
    } else if(name == "obj"){
        if(!is(para, "RedPort")){
            stop("NOTE: 'obj' must be a RedPort object created by 
                    RedeR::RedPort()!", call.=FALSE)
        }
    } else if(name == "phy"){
        if(!is(para, "phylo")){
            stop("NOTE: 'phy' must be a 'phylo' object!", call.=FALSE)
        }
    }
}


##------------------------------------------------------------------------------
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
