test_tal <- function(){
    hc <- hclust(dist(USArrests), "average")
    tal <- treeAndLeaf(hc)
    RUnit::checkTrue(is(tal, "igraph"))
    RUnit::checkTrue(!is.null(tal$gtype))
    RUnit::checkTrue(tal$gtype=="TreeAndLeaf")
}
test_theme<- function(){
    hc <- hclust(dist(USArrests), "average")
    tal <- treeAndLeaf(hc)
    tal <- formatTree(tal, theme = 5)
    RUnit::checkTrue(is(tal, "igraph"))
    RUnit::checkTrue(!is.null(tal$gtype))
    RUnit::checkTrue(tal$gtype=="TreeAndLeaf")
}


