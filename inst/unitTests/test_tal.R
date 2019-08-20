test_hclust2igraph <- function(){
    hc <- hclust(dist(USArrests), "average")
    gg <- hclust2igraph(hc)
    RUnit::checkTrue(is(gg, "igraph"))
    RUnit::checkTrue(!is.null(gg$intnodes))
}

test_phylo2igraph <- function(){
    data("phylo_tree")
    gg <- phylo2igraph(phylo_tree)
    RUnit::checkTrue(is(gg, "igraph"))
    RUnit::checkTrue(!is.null(gg$intnodes))
}

test_formatTree <- function(){
    data("phylo_tree")
    gg <- phylo2igraph(phylo_tree)
    gg <- formatTree(gg)
    RUnit::checkTrue(is(gg, "igraph"))
    RUnit::checkTrue(!is.null(igraph::V(gg)$nodeAlias))
    RUnit::checkTrue(!is.null(igraph::V(gg)$nodeFontSize))
    RUnit::checkTrue(!is.null(igraph::V(gg)$nodeSize))
    RUnit::checkTrue(!is.null(igraph::V(gg)$nodeLineWidth))
    RUnit::checkTrue(!is.null(igraph::V(gg)$nodeLineColor))
    RUnit::checkTrue(!is.null(igraph::V(gg)$nodeFontColor))
    RUnit::checkTrue(!is.null(igraph::V(gg)$nodeColor))
    RUnit::checkTrue(!is.null(igraph::E(gg)$edgeWidth))
    RUnit::checkTrue(!is.null(igraph::E(gg)$edgeColor))
}
