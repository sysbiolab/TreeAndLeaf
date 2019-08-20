\name{TreeAndLeaf-package}
\alias{TreeAndLeaf-package}
\alias{TreeAndLeaf}
\docType{package}

\title{
TreeAndLeaf: An alternative to dendrogram visualization and 
insertion of multiple layers of information.
}

\description{
TreeAndLeaf package comes as an alternative to solve problems regarding 
dendrogram plotting, such as the lack of space when the dendrogram is too
large and the need for adding more layers of information. It treats a whole
dendrogram as a tree, in which the observations are represented by the leaves.
}

\details{
\tabular{ll}{
Package: \tab TreeAndLeaf\cr
Type: \tab Package\cr
Depends: \tab R (>= 3.3)\cr 
Imports: \tab RedeR, igraph, ape \cr
Suggests: \tab knitr, rmarkdown, BiocStyle, RUnit, BiocGenerics, stringr,
RColorBrewer\cr 
License: \tab Artistic-2.0\cr
biocViews: \tab NetworkEnrichment, GraphAndNetwork\cr
}
}

\section{Index}{

\tabular{ll}{
\link{formatTree}: \tab a function for formatting an igraph to be shown
in RedeR.\cr
\link{hclust2igraph}: \tab a function for converting a hclust object to
an igraph.\cr
\link{phylo2igraph}: \tab a function for converting a phylo object to an
igraph.\cr
\link{treeAndLeaf}: \tab a function for layout creation and plotting of
the TreeAndLeaf in RedeR.\cr
}

Further information is available in the vignettes by typing 
\code{vignette("TreeAndLeaf")}. Documented topics are also available in 
HTML by typing \code{help.start()} and 
selecting the TreeAndLeaf package from the menu.
}

\author{
Luis E. A. Rizzardi, Leonardo Kume, Mauro A. A. Castro
}

\references{
CASTRO, M. A. et al. RedeR: R/Bioconductor package for representing modular 
structures, nested networks and multiple levels of hierarchical associations.
\strong{Genome Biology}, v. 13, n. 4, p. R29, 2012. 

CASTRO, M. A. A. et al. Regulators of genetic risk of breast cancer identified 
by integrative network analysis. \strong{Nature Genetics}, v. 48,
n. 1, p. 12–21, jan. 2016. 

RUSU, A.; SANTIAGO, C. Grid Drawings of Binary Trees: An Experimental 
Study. \strong{Journal of Graph Algorithms and 
Applications}, v. 12, n. 2, p. 131–195, 2008. 
}

\keyword{ package }

