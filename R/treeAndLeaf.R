#' Layout a TreeAndLeaf diagram.
#'
#' This function tranforms hclust and phylo objects into tree-and-leaf 
#' igraph objects.
#'
#' @param obj An object of class 'hclust' or 'phylo'.
#' 
#' @return A tree-and-leaf igraph object.
#'
#' @seealso \code{\link{formatTree}}
#' @seealso \code{\link[stats:hclust]{hclust}}
#' @seealso \code{\link[ape:as.phylo]{as.phylo}}
#' @seealso \code{\link[RedeR:addGraph]{addGraph}}
#' @seealso \code{\link[RedeR:relax]{relax}}
#'
#' @examples
#' library(RedeR)
#' rdp <- RedPort()
#' hc <- hclust(dist(USArrests), "ave")
#' tal <- treeAndLeaf(hc)
#' 
#' \dontrun{
#' calld(rdp)
#' addGraph(obj=rdp, tal)
#' }
#'
#' @importFrom igraph edge_betweenness is.igraph V E 'V<-' 'E<-' 
#' @importFrom ape as.phylo as.igraph.phylo
#' @importFrom RedeR RedPort
#' @export

treeAndLeaf <- function(obj){
    tal.checks(name="obj", para=obj)
    tal <- .setTaL(obj)
    return(tal)
}
# resetd(rdp)
# addGraph(rdp, tal)
#-------------------------------------------------------------------------------
.setTaL <- function(obj){
    if ("hclust" %in% class(obj)){
        phylo <- ape::as.phylo(obj)
        gg <- ape::as.igraph.phylo(phylo, directed=FALSE)
        coords <- .get.tree.coords(phylo, use.edge.length=FALSE)
    } else if("phylo" %in% class(obj)){
        gg <- ape::as.igraph.phylo(obj, directed=FALSE)
        coords <- .get.tree.coords(obj, use.edge.length=FALSE)
    } else if("ggtree" %in% class(obj)){
        lpar <- obj$layers[[1]]$stat_params$layout
        vpar <- c("daylight","ape","fan","equal_angle")
        if(!lpar%in%vpar)
            stop("Please, use one of the 'ggtree' layouts: ",
                 paste0(vpar, collapse = ", "),"!", call.=FALSE)
        # gdata <- ggplot_build(obj)$data[[1]]
        phylo <- ape::as.phylo(obj$data)
        gg <- ape::as.igraph.phylo(phylo, directed=FALSE)
        coords <- .get.tree.coords(phylo, use.edge.length=FALSE)
        coords$layout[,] <- as.matrix(obj$data[,c("x","y")])
    }
    if(!all(V(gg)$name%in%rownames(coords$layout)))
        stop("An unexpected error occurred while transforming the graph object!")
    #--- set layout
    layout <- coords$layout[V(gg)$name,]
    V(gg)$coordX <- layout[,"x"]
    V(gg)$coordY <- layout[,"y"]
    V(gg)$isLeaf <- V(gg)$name%in%coords$phylo$tip.label
    gg$centralVertex <- coords$centralVertex
    #--- set edge weights
    E(gg)$weight <- 0
    bt <- igraph::edge_betweenness(gg, directed = T)
    bt <- (1 - bt/max(bt))*100
    E(gg)$weight <- bt
    #--- set node and font sizes
    V(gg)$nodeAlias <- V(gg)$name
    V(gg)$nodeSize <- 5
    V(gg)$nodeSize[V(gg)$isLeaf] <- 30
    V(gg)$nodeFontSize <- 15
    V(gg)$nodeFontSize[!V(gg)$isLeaf] <- 1
    V(gg)$nodeColor <- "#ffcccc"
    V(gg)$nodeLineColor <- "#9999ff"
    V(gg)$nodeLineWidth <- 2
    E(gg)$edgeColor <- "#9999ff"
    E(gg)$edgeWidth <- 2
    #--- set zoom
    sz <- sum(V(gg)$isLeaf)
    sz <- sqrt(sz)
    # gg$zoom <- ceiling(max(0,100-sz))
    gg$gtype <- "TreeAndLeaf"
    class(gg) <- c("tal","igraph")
    return(gg)
}

#-------------------------------------------------------------------------------
# This function gets coordinates from an unrooted 'phylo' objects,
# derived from a currectly not exported function from the ape package
.get.tree.coords <- function(phy, use.edge.length = FALSE){
    if (is.null(phy$edge.length)) use.edge.length <- FALSE
    phy <- ape::reorder.phylo(phy)
    Ntip <- length(phy$tip.label)
    Nedge <- dim(phy$edge)[1]
    Nnode <- phy$Nnode
    nb.sp <- ape::node.depth(phy)
    XY <- if(use.edge.length){
        .unrooted.xy(Ntip, Nnode, phy$edge, phy$edge.length, nb.sp)
    } else {
        .unrooted.xy(Ntip, Nnode, phy$edge, rep(1, Nedge), nb.sp)
    }
    xx <- XY$M[, 1] - min(XY$M[, 1])
    yy <- XY$M[, 2] - min(XY$M[, 2])
    layout <- cbind(x=xx,y=yy)
    if(is.null(phy$node.label)) phy <- ape::makeNodeLabel(phy)
    if(anyDuplicated(c(phy$tip.label, phy$node.label))) 
        stop("Duplicated labels!")
    rownames(layout) <- c(phy$tip.label,phy$node.label)
    centralVertex <- phy$node.label[1]
    return(list(phylo=phy, layout=layout,centralVertex=centralVertex))
}

#-------------------------------------------------------------------------------
# This function gets coordinates from an unrooted 'phylo' objects,
# derived from a currectly not exported function from the ape package
.unrooted.xy <- function(Ntip, Nnode, edge, edge.length, nb.sp){
    foo <- function(node, ANGLE, AXIS) {
        ind <- which(edge[, 1] == node)
        sons <- edge[ind, 2]
        start <- AXIS - ANGLE/2
        for (i in 1:length(sons)) {
            h <- edge.length[ind[i]]
            angle[sons[i]] <<- alpha <- ANGLE*nb.sp[sons[i]]/nb.sp[node]
            axis[sons[i]] <<- beta <- start + alpha/2
            start <- start + alpha
            xx[sons[i]] <<- h*cos(beta) + xx[node]
            yy[sons[i]] <<- h*sin(beta) + yy[node]
        }
        for (i in sons)
            if (i > Ntip) foo(i, angle[i], axis[i])
    }
    Nedge <- dim(edge)[1]
    yy <- xx <- numeric(Ntip + Nnode)
    axis <- angle <- numeric(Ntip + Nnode)
    foo(Ntip + 1L, 2*pi, 0)
    M <- cbind(xx, yy)
    axe <- axis[1:Ntip]
    axeGTpi <- axe > pi
    axe[axeGTpi] <- axe[axeGTpi] - 2*pi
    list(M = M, axe = axe)
}
