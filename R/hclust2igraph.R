#' Convert an hclust to an igraph
#'
#' Function for converting a hclust object to an igraph.
#'
#' @param hc a hclust object.
#'
#' @return An igraph object.
#'
#' @seealso \code{\link[stats:hclust]{hclust}}
#'
#' @examples
#' hc <- hclust(dist(USArrests), "ave")
#' gg <- hclust2igraph(hc)                                 
#'
#' @importFrom igraph graph.edgelist
#' @export

hclust2igraph<-function(hc){
    if(!is(hc, "hclust"))stop("'hc' should be an 'hclust' object!")
    if(is.null(hc$labels))hc$labels=as.character(sort(hc$order))
    #options to remove nests
    rmnodes<-NULL
    if(!is.null(hc$cutoff)){
        #remove nests based on length cutoff
        tmap<-.treemap(hc)
        hcNodes<-tmap$hcNodes
        hcNests<-hcNodes[hcNodes$type=="nest",]
        hcEdges<-tmap$hcEdges
        nestList<-tmap$nest
        rmnodes<-hcEdges[hcEdges$edgeLength<hc$cutoff,]
        rmnodes<-unique(rmnodes$parentNode)
        rmnodes<-hcNodes$hcId[hcNodes$node%in%rmnodes]
        mergeBlocks=FALSE
    } else if(!is.null(hc$keep)){
        #option to assign parent nodes to be preserved
        ids<-as.numeric(hc$merge)
        ids<-sort(ids[ids>0])
        rmnodes<-ids[!ids%in%hc$keep]
        mergeBlocks=TRUE
    } else if(!is.null(hc$remove)){
        #option to assign parent nodes to be removed
        ids<-as.numeric(hc$merge)
        ids<-sort(ids[ids>0])
        rmnodes<-ids[ids%in%hc$remove]
        mergeBlocks=FALSE
    }
    #build igraph and return assigments
    gg <- .hclust2igraph(hc,rmnodes,mergeBlocks)
    gg$g$intnodes <- names(gg$nest)
    gg$g
}

.treemap<-function(hc){
    A=hc$merge
    B=list()
    C=list()
    D=list()
    E=list()
    nest=list()
    if(is.null(hc$labels))hc$labels=as.character(sort(hc$order))
    for(i in seq_len(nrow(A))){
        ai=A[i,1]
        if(ai < 0){
            B[[i]]= -ai
            C[[i]]=1
        } else {
            B[[i]]=B[[ai]]
            C[[i]]=C[[ai]]+1
        }
        ai=A[i,2]
        if(ai < 0){
            B[[i]]=sort(c(B[[i]],-ai))
        } else {
            B[[i]]=sort(c(B[[i]],B[[ai]]))
            C[[i]]=max(C[[i]],C[[ai]]+1)
        }
        p=match(i,A)
        D[[i]]=ifelse(p>nrow(A),p-nrow(A),p)
        nest[[i]]=hc$labels[B[[i]]]
    }
    D[[nrow(A)]]=nrow(A)+1
    for(i in seq_len(nrow(A))){
        step=1
        find=D[[i]]
        while(find<D[[nrow(A)]]){
            find=D[[find]]
            step=step+1
        }
        E[[i]]=step
    }

    # get dendogram xy position
    nn=nrow(A) + 1
    xaxis=c()
    yaxis=hc$height
    tp=rep(0,2)
    mm=match(seq_len(length(hc$order)),hc$order)
    for(i in seq_len((nn-1))) {
        ai=A[i,1]
        if(ai < 0){
            tp[1]=mm[-ai]
        } else {
            tp[1]=xaxis[ai]
        }
        ai=A[i,2]
        if(ai < 0){
            tp[2]=mm[-ai]
        } else {
            tp[2]=xaxis[ai]
        }
        xaxis[i]=mean(tp)
    }
    xyaxis=data.frame(xaxis=xaxis,yaxis=yaxis,stringsAsFactors=FALSE)
    # return res
    C=as.numeric(C)
    D=as.numeric(D)
    E=as.numeric(E)
    N=hc$merge>0
    N=N[,1]+N[,2]
    obj<-list(nest=nest,compids=B,labels=hc$labels,parent=D,leafdist=C,
                    rootdist=E,height=hc$height,nnest=N, xyaxis=xyaxis)
    #---get unified edges
    N<-nrow(hc$merge);nn<-N+1
    hcEdges<-NULL
    eLength<-NULL
    for(i in seq_len(N)){
        y1<-hc$merge[i,1]
        y2<-hc$merge[i,2]
        temptp <- tp
        if(y1>0){
            l1<-hc$height[i] - hc$height[y1]
        } else {
            l1<-hc$height[i]
        }
        if(y2>0){
            l2<-hc$height[i] - hc$height[y2]
        } else {
            l2<-hc$height[i]
        }
        temptp <- cbind(rbind(c(i,y1),c(i,y2)),c(l1,l2))
        hcEdges <- rbind(hcEdges,temptp)
    }
    rm(temptp)

    colnames(hcEdges)<-c("parentNode","childNode","edgeLength")
    hcEdges<-data.frame(hcEdges,stringsAsFactors=FALSE)
    hcEdges$parentHeight<-obj$height[hcEdges$parentNode]
    #---get unified nodes
    hcl<-data.frame(node=hc$labels,mergeId=-c(seq_len(nn)),hcId=c(seq_len(nn)),
                    type="leaf",stringsAsFactors=FALSE)
    hcn<-data.frame(node=paste("N",c(seq_len(N)),sep=""),mergeId=c(seq_len(N)),
                    hcId=c(seq_len(N)),type="nest",stringsAsFactors=FALSE)
    hcNodes<-rbind(hcl,hcn)
    hcEdges$parentNode<-hcNodes$node[match(hcEdges$parentNode,hcNodes$mergeId)]
    hcEdges$childNode<-hcNodes$node[match(hcEdges$childNode,hcNodes$mergeId)]
    obj$hcNodes<-hcNodes;obj$hcEdges<-hcEdges
    names(obj$nest)<-paste("N",c(seq_len(N)),sep="")
    #---get mest maps (optional)
    nestmap<-obj$nest
    nestall<-obj$nest
    nts<-names(obj$nest)
    for(i in seq_len(length(nts))){
        tp<-unlist(lapply(lapply(obj$nest,"%in%",obj$nest[[i]]),all))
        tp<-tp[-i]
        tp<-nts[which(tp)]
        nestmap[[i]]<-tp
        nestall[[i]]<-c(nestall[[i]],tp)
    }
    obj$nestmap<-nestmap
    obj$nestall<-nestall
    #---get node lineage
    getpar<-function(hcEdges,childNode){
        idx<-which(hcEdges$childNode==childNode)
        hcEdges$parentNode[idx]
    }
    root<-hcEdges$parentNode[length(hcEdges$parentNode)]
    lineage<-list()
    for(i in seq_len(nrow(obj$hcEdges))){
        ch<-obj$hcEdges$childNode[i]
        pn<-obj$hcEdges$parentNode[i]
        lineage[[ch]]<-pn
        while(pn!=root){
            pn<-getpar(obj$hcEdges,pn)
            lineage[[ch]]<-c(lineage[[ch]],pn)
        }
    }
    obj$lineage<-lineage
    obj$rootid<-root
    return(obj)
}

.hclust2igraph<-function(hc,rmnodes=NULL, mergeBlocks=TRUE){
    #get treemap
    tmap<-.treemap(hc)
    hcNodes<-tmap$hcNodes
    hcNests<-hcNodes[hcNodes$type=="nest",]
    hcEdges<-tmap$hcEdges
    nestList<-tmap$nest
    nestmap<-tmap$nestmap
    lineage<-tmap$lineage
    rootid<-tmap$rootid
    #set nests do be removed
    if(!is.null(rmnodes) && length(rmnodes)>0){
        rmnodes<-hcNodes[hcNodes$mergeId%in%rmnodes,]
        rmnodes<-rmnodes$node[!rmnodes$node==rootid]
        #update hcEdges and nestList
        if(length(rmnodes)>0){
            hcEdges<-.hcEdges.filter(hcEdges,hcNodes,rmnodes,lineage,rootid,
                                        mergeBlocks)
            nestList<-nestList[names(nestList)%in%hcEdges$parentNode]
        }
    }
    #build igraph and return assigments
    g<-igraph::graph.edgelist(as.matrix(hcEdges[,seq_len(2)]), directed=TRUE)
    #E(g)$edgeWeight<-hcEdges$edgeLength
    #tp<-hcEdges$parentHeight-min(hcEdges$parentHeight)
    #E(g)$edgeWeight<-(max(tp)-tp)/max(tp) * 100
    igraph::E(g)$arrowType<-0
    list(g=g,nest=nestList,lineage=lineage)
}
.hcEdges.filter<-function(hcEdges,hcNodes,rmnodes,lineage,rootid,
                            mergeBlocks=TRUE){
    getpar<-function(hcEdges,childNode){
        idx<-which(hcEdges$childNode==childNode)
        hcEdges$parentNode[idx]
    }
    #update rmnodes with any continous bottom-up struture not in rmnodes
    if(mergeBlocks){
        nids<-hcNodes$node[hcNodes$type=="nest"]
        nids<-nids[!nids%in%rmnodes]
        for(i in seq_len(length(nids))){
            pn<-getpar(hcEdges,nids[i])
            if(length(pn)>0 && !pn%in%rmnodes && !pn%in%rootid){
                rmnodes<-c(rmnodes,nids[i])
            }
        }
    }
    #update lineage
    lineage<-lapply(lineage,setdiff,rmnodes)
    #get the correct order
    rmNests<-hcNodes[hcNodes$node%in%rmnodes,]
    rmEdges<-hcEdges[hcEdges$childNode%in%rmnodes,]
    #get filtered hcEdges
    hcEdges<-hcEdges[!hcEdges$childNode%in%rmNests$node,]
    #update parents'ids
    for(i in seq_len(nrow(hcEdges))){
        newpn<-lineage[[hcEdges$childNode[i]]][1]
        hcEdges$parentNode[i]<-newpn
    }
    rownames(hcEdges)<-NULL
    hcEdges
}
