## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>"
)

## ---- eval=TRUE, message=FALSE------------------------------------------------
#-- Libraries required in this section:
#-- TreeAndLeaf(>=1.2.1), RedeR(>=1.38.1), Bioconductor >= 3.12 (R >= 4.0)
# BiocManager::install(c("TreeAndLeaf","RedeR"))
# install.packages(c("igraph","RColorBrewer"))

#-- Load packages
library("TreeAndLeaf")
library("RedeR")
library("igraph")
library("RColorBrewer")

## ---- eval=TRUE, message=FALSE------------------------------------------------
#-- Check data
dim(USArrests)
head(USArrests)

## ---- eval=TRUE, message=FALSE------------------------------------------------
hc <- hclust(dist(USArrests), "ave")
plot(hc, main="Dendrogram for the 'USArrests' dataset",
     xlab="", sub="")

## ---- eval=FALSE--------------------------------------------------------------
#  #--- Get the tree-and-leaf
#  tal <- treeAndLeaf(hc)

## ---- eval=FALSE--------------------------------------------------------------
#  #--- Map attributes to the tree-and-leaf
#  #Note: 'refcol = 0' indicates that 'dat' rownames will be used as mapping IDs
#  tal <- att.mapv(g = tal, dat = USArrests, refcol = 0)

## ---- eval=FALSE--------------------------------------------------------------
#  #--- Set graph attributes using the 'att.setv' wrapper function
#  pal <- brewer.pal(9, "Reds")
#  tal <- att.setv(g = tal, from = "Murder", to = "nodeColor",
#                  cols = pal, nquant = 5)
#  tal <- att.setv(g = tal, from = "UrbanPop", to = "nodeSize",
#                  xlim = c(10, 50, 5), nquant = 5)
#  
#  #--- Set graph attributes using 'att.addv' and 'att.adde' functions
#  tal <- att.addv(tal, "nodeFontSize", value = 15, index = V(tal)$isLeaf)
#  tal <- att.adde(tal, "edgeWidth", value = 3)

## ---- eval=FALSE--------------------------------------------------------------
#  #--- Call RedeR application
#  rdp <- RedPort()
#  calld(rdp)
#  resetd(rdp)

## ---- eval=FALSE--------------------------------------------------------------
#  #--- Send the tree-and-leaf to the interactive R/Java interface
#  addGraph(obj = rdp, g = tal, gzoom=75)
#  
#  #--- Call 'relax' to fine-tune the leaf nodes
#  relax(rdp, p1=25, p2=200, p3=5, p5=5, ps=TRUE)

## ---- eval=FALSE--------------------------------------------------------------
#  #--- Add legends
#  addLegend.color(obj = rdp, tal, title = "Murder Rate",
#                  position = "topright")
#  addLegend.size(obj = rdp, tal, title = "Urban Population Size",
#                 position = "bottomright")

## ---- eval=FALSE, message=FALSE-----------------------------------------------
#  #-- Libraries required in this section:
#  #-- TreeAndLeaf(>=1.2.1), RedeR(>=1.38.1), Bioconductor >= 3.12 (R >= 4.0)
#  # BiocManager::install(c("TreeAndLeaf","RedeR","ggtree))
#  # install.packages(c("igraph","ape", "dendextend", "dplyr",
#  #                    "ggplot2", "RColorBrewer"))
#  
#  #-- Load packages
#  library("TreeAndLeaf")
#  library("RedeR")
#  library("igraph")
#  library("ape")
#  library("ggtree")
#  library("dendextend")
#  library("dplyr")
#  library("ggplot2")
#  library("RColorBrewer")

## ---- eval=FALSE--------------------------------------------------------------
#  #--- Generate a random phylo tree
#  phylo_tree <- rcoal(300)
#  
#  #--- Set groups and node sizes
#  group <- size <- dendextend::cutree(phylo_tree, 10)
#  group[] <- LETTERS[1:10][group]
#  size[] <- sample(size)
#  group.df <- data.frame(label=names(group), group=group, size=size)
#  phylo_tree <- dplyr::full_join(phylo_tree, group.df, by='label')
#  
#  #--- Plot the phylo tree using a 'ggtree' layout
#  pal <- brewer.pal(10, "Set3")
#  ggt <- ggtree(phylo_tree, layout = 'daylight', branch.length='none')
#  ggt + geom_tippoint(aes(color=group, size=size)) +
#    scale_color_manual(values=pal) + scale_y_reverse()

## ---- eval=FALSE--------------------------------------------------------------
#  #--- Get the tree-and-leaf
#  tal <- treeAndLeaf(ggt)
#  
#  #--- Map attributes to the tree-and-leaf
#  #Note: 'refcol = 1' indicates that 'dat' col 1 will be used as mapping IDs
#  tal <- att.mapv(g = tal, dat = group.df, refcol = 1)
#  
#  #--- Set graph attributes using the 'att.setv' wrapper function
#  tal <- att.setv(g = tal, from = "group", to = "nodeColor",
#                  cols = pal)
#  tal <- att.setv(g = tal, from = "size", to = "nodeSize",
#                  xlim = c(10, 50, 5))
#  
#  #--- Set graph attributes using 'att.addv' and 'att.adde' functions
#  tal <- att.addv(tal, "nodeFontSize", value = 1)
#  tal <- att.addv(tal, "nodeLineWidth", value = 0)
#  tal <- att.addv(tal, "nodeColor", value = "black", index=!V(tal)$isLeaf)
#  tal <- att.adde(tal, "edgeWidth", value = 3)
#  tal <- att.adde(tal, "edgeColor", value = "black")

## ---- eval=FALSE--------------------------------------------------------------
#  #--- Call RedeR application
#  rdp <- RedPort()
#  calld(rdp)
#  resetd(rdp)

## ---- eval=FALSE--------------------------------------------------------------
#  #--- Send the tree-and-leaf to the interactive R/Java interface
#  addGraph(obj = rdp, g = tal, gzoom=50)
#  
#  #--- Select inner nodes, preventing them from relaxing
#  selectNodes(rdp, V(tal)$name[!V(tal)$isLeaf], anchor = TRUE)
#  
#  #--- Call 'relax' to fine-tune the leaf nodes
#  relax(rdp, p1=25, p2=100, p3=5, p5=1, p8=5, ps=TRUE)
#  
#  #--- Add legends
#  addLegend.color(obj = rdp, tal, title = "Group",
#                  position = "topright",vertical=T)
#  addLegend.size(obj = rdp, tal, title = "Size",
#                 position = "topleft",
#                 vertical=T, dxtitle=10)

## ---- eval=FALSE, message=FALSE-----------------------------------------------
#  #-- Libraries required in this section:
#  #-- TreeAndLeaf(>=1.2.1), RedeR(>=1.38.1), Bioconductor >= 3.12 (R >= 4.0)
#  # BiocManager::install(c("TreeAndLeaf","RedeR"))
#  # install.packages(c("igraph", "RColorBrewer"))
#  
#  #-- Load packages
#  library(TreeAndLeaf)
#  library(RedeR)
#  library(igraph)
#  library(RColorBrewer)

## ----echo=TRUE----------------------------------------------------------------
#-- Check data
dim(quakes)
head(quakes)

## ---- eval=TRUE, message=FALSE------------------------------------------------
#-- Building a large dendrogram
hc <- hclust(dist(quakes), "ave")
plot(hc, main="Dendrogram for the 'quakes' dataset",
     xlab="", sub="")

## ---- eval=FALSE--------------------------------------------------------------
#  #--- Get the tree-and-leaf
#  tal <- treeAndLeaf(hc)

## ---- eval=FALSE--------------------------------------------------------------
#  #--- Map attributes to the tree-and-leaf
#  #Note: 'refcol = 0' indicates that 'dat' rownames will be used as mapping IDs
#  tal <- att.mapv(tal, quakes, refcol = 0)
#  
#  #--- Set graph attributes using the 'att.setv' wrapper function
#  pal <- brewer.pal(9, "Greens")
#  tal <- att.setv(g = tal, from = "mag", to = "nodeColor",
#                  cols = pal, nquant = 10)
#  tal <- att.setv(g = tal, from = "depth", to = "nodeSize",
#                  xlim = c(40, 120, 20), nquant = 5)
#  
#  #--- Set graph attributes using 'att.addv' and 'att.adde' functions
#  tal <- att.addv(tal, "nodeFontSize", value = 1)
#  tal <- att.adde(tal, "edgeWidth", value = 10)

## ---- eval=FALSE--------------------------------------------------------------
#  #--- Call RedeR application
#  rdp <- RedPort()
#  calld(rdp)
#  resetd(rdp)

## ---- eval=FALSE--------------------------------------------------------------
#  #--- Send the tree-and-leaf to the interactive R/Java interface
#  addGraph(obj = rdp, g = tal, gzoom=10)
#  
#  #--- Call 'relax' to fine-tune the leaf nodes
#  relax(rdp, p1=25, p2=200, p3=10, p4=100, p5=10, ps=TRUE)

## ---- eval=FALSE--------------------------------------------------------------
#  #--- Add legends
#  addLegend.color(obj = rdp, tal, title = "Richter Magnitude",
#                  position = "bottomright")
#  addLegend.size(obj = rdp, tal, title = "Depth (km)")

## ---- eval=FALSE, message=FALSE-----------------------------------------------
#  #-- Libraries required in this section:
#  #-- TreeAndLeaf(>=1.2.1), RedeR(>=1.38.1), Bioconductor >= 3.12 (R >= 4.0)
#  # BiocManager::install(c("TreeAndLeaf","RedeR","geneplast))
#  # install.packages(c("igraph","ape", "RColorBrewer"))
#  
#  #-- Load packages
#  library(TreeAndLeaf)
#  library(RedeR)
#  library(igraph)
#  library(ape)
#  library(geneplast)
#  library(RColorBrewer)

## ---- eval=FALSE, message=FALSE-----------------------------------------------
#  #-- Load data and plot the phylogenetic tree
#  data("spdata")
#  data("gpdata.gs")
#  plot(phyloTree)

## ---- eval=FALSE--------------------------------------------------------------
#  #--- Drop organisms not listed in the 'spdata' annotation
#  phyloTree$tip.label <- as.character(phyloTree$tip.label)
#  tokeep <- phyloTree$tip.label %in% spdata$tax_id
#  pruned.phylo <- drop.tip(phyloTree, phyloTree$tip.label[!tokeep])

## ---- eval=FALSE--------------------------------------------------------------
#  #--- Get the tree-and-leaf
#  tal <- treeAndLeaf(pruned.phylo)
#  
#  #--- Map attributes to the tree-and-leaf
#  #Note: 'refcol = 1' indicates that 'dat' col 1 will be used as mapping IDs
#  tal <- att.mapv(g = tal, dat = spdata, refcol = 1)
#  
#  #--- Set graph attributes using the 'att.setv' wrapper function
#  pal <- brewer.pal(9, "Purples")
#  tal <- att.setv(g = tal, from = "genome_size_Mb",
#                  to = "nodeSize", xlim = c(120, 250, 1), nquant = 5)
#  tal <- att.setv (g = tal, from = "proteins",
#                   to = "nodeColor", nquant = 5,
#                   cols = pal, na.col = "black")

## ---- eval=FALSE--------------------------------------------------------------
#  #--- Add graph attributes using 'att.adde' and 'att.addv' functions
#  tal <- att.addv(tal, "nodeFontSize", value = 10)
#  tal <- att.adde(tal, "edgeWidth", value = 20)
#  
#  # Set species names to 'nodeAlias' attribute
#  tal <- att.setv(tal, from = "sp_name", to = "nodeAlias")
#  
#  # Select a few names to highlight in the graph
#  tal <- att.addv(tal, "nodeFontSize", value = 100,
#         filter=list('name'=sample(pruned.phylo$tip.label,30)))
#  tal <- att.addv(tal, "nodeFontSize", value = 100,
#                  filter=list('name'="9606")) #Homo sapiens

## ---- eval=FALSE--------------------------------------------------------------
#  # Call RedeR
#  rdp <- RedPort()
#  calld(rdp)
#  resetd(rdp)
#  
#  #--- Send the tree-and-leaf to the interactive R/Java interface
#  addGraph(obj = rdp, g = tal, gzoom=10)
#  
#  #--- Call 'relax' to fine-tune the leaf nodes
#  relax(rdp, p1=50, p8=15, ps=TRUE)

## ---- eval=FALSE--------------------------------------------------------------
#  #--- Add legends
#  addLegend.color(rdp, tal, title = "Proteome Size (n)")
#  addLegend.size(rdp, tal, title = "Genome Size (Mb)")

## ---- eval=FALSE--------------------------------------------------------------
#  #-- Libraries required in this section:
#  #-- TreeAndLeaf(>=1.2.1), RedeR(>=1.38.1), Bioconductor >= 3.12 (R >= 4.0)
#  # BiocManager::install(c("TreeAndLeaf","RedeR","geneplast))
#  # install.packages(c("igraph","ape", "RColorBrewer"))
#  
#  #-- Load packages
#  library(TreeAndLeaf)
#  library(RedeR)
#  library(igraph)
#  library(ape)
#  library(geneplast)
#  library(RColorBrewer)

## ---- eval=FALSE, message=FALSE-----------------------------------------------
#  #-- Load data
#  data("spdata")
#  data("phylo_tree")

## ---- eval=FALSE, message=FALSE-----------------------------------------------
#  #--- Drop organisms not listed in the 'spdata' annotation
#  tokeep <- phylo_tree$tip.label %in% spdata$tax_id
#  pruned.phylo <- drop.tip(phylo_tree, phylo_tree$tip.label[!tokeep])

## ---- eval=FALSE--------------------------------------------------------------
#  #--- Get the tree-and-leaf
#  tal <- treeAndLeaf(pruned.phylo)

## ---- eval=FALSE--------------------------------------------------------------
#  #--- Map attributes to the tree-and-leaf using "%>%" operator
#  tal <- tal %>%
#    att.mapv(dat = spdata, refcol = 1) %>%
#    att.setv(from = "genome_size_Mb", to = "nodeSize",
#             xlim = c(120, 250, 1), nquant = 5) %>%
#    att.setv(from = "proteins", to = "nodeColor", nquant = 5,
#             cols = brewer.pal(9, "Blues"), na.col = "black") %>%
#    att.setv(from = "sp_name", to = "nodeAlias") %>%
#    att.adde(to = "edgeWidth", value = 20) %>%
#    att.addv(to = "nodeFontSize", value = 10) %>%
#    att.addv(to = "nodeFontSize", value = 100,
#        filter = list("name" = sample(pruned.phylo$tip.label, 30))) %>%
#    att.addv(to = "nodeFontSize", value = 100,
#             filter = list("name" = "9606"))

## ---- eval=FALSE--------------------------------------------------------------
#  # Call RedeR
#  rdp <- RedPort()
#  calld(rdp)
#  resetd(rdp)
#  
#  #--- Send the tree-and-leaf to the interactive R/Java interface
#  addGraph(obj = rdp, g = tal, gzoom=5)
#  
#  #--- Call 'relax' to fine-tune the leaf nodes
#  relax(rdp, p1=50, p8=15, ps=TRUE)

## ---- eval=FALSE--------------------------------------------------------------
#  #--- Add legends
#  addLegend.color(rdp, tal, title = "Proteome Size (n)")
#  addLegend.size(rdp, tal, title = "Genome size (Mb)")

## ----label='Session information', eval=TRUE, echo=FALSE-----------------------
sessionInfo()

