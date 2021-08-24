\name{TreeAndLeaf-package}
\alias{TreeAndLeaf-package}
\alias{TreeAndLeaf}
\docType{package}

\title{
TreeAndLeaf: A graph layout for binary trees with focus on dendrogram leaves.
}

\description{
The TreeAndLeaf package combines unrooted and force-directed graph algorithms 
in order to layout binary trees, aiming to represent multiple layers of 
information onto dendrogram leaves.
}

\details{
\tabular{ll}{
Package: \tab TreeAndLeaf\cr
Type: \tab Package\cr
Depends: \tab R (>= 4.0)\cr 
Imports: \tab RedeR, igraph, ape \cr
Suggests: \tab knitr, rmarkdown, BiocStyle, RUnit, BiocGenerics, stringr,
RColorBrewer\cr 
License: \tab Artistic-2.0\cr
biocViews: \tab NetworkEnrichment, GraphAndNetwork\cr
}
}

\section{Index}{

\tabular{ll}{
\link{formatTree}: \tab A theme function for tree-and-leaf igraph objects.\cr
\link{treeAndLeaf}: \tab Layout a TreeAndLeaf diagram.\cr
}

Further information is available from the vignettes 
\code{vignette("TreeAndLeaf")}.
}

\author{
Leonardo W. Kume, Luis E. A. Rizzardi, Milena A. Cardoso, Mauro A. A. Castro
}

\references{
CASTRO, M. A. et al. RedeR: R/Bioconductor package for representing modular 
structures, nested networks and multiple levels of hierarchical associations.
\strong{Genome Biology}, 13(4):R29, 2012.

CASTRO, M. A. A. et al. Regulators of genetic risk of breast cancer identified 
by integrative network analysis. \strong{Nature Genetics}, 48(1):12–21, 2016.

RUSU, A.; SANTIAGO, C. Grid Drawings of Binary Trees: An Experimental 
Study. \strong{Journal of Graph Algorithms and Applications}, 
12(2):131–195, 2008.
}

\keyword{package}

