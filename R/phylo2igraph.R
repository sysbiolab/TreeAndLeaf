#' Convert a phylo object to an igraph
#'
#' Function for converting a phylo object to an igraph.
#'
#' @param phy A phylo object with labeled tips <phylo>.
#' 
#' @return An igraph object.
#'
#' @examples
#' phy <- ape::rtree(10, tip.label = c(1:10))
#' gg <- phylo2igraph(phy)
#' 
#' @importFrom ape as.igraph.phylo
#' @export

phylo2igraph <- function(phy){
    tal.checks(name = "phy", para = phy)
    gg <- ape::as.igraph.phylo(phy, directed = FALSE)
    idx <- match(igraph::V(gg)$name, phy$tip.label)
    gg$intnodes <- igraph::V(gg)$name[is.na(idx)]
    return(gg)
}