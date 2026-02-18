### *TreeAndLeaf*: Displaying binary trees with focus on dendrogram leaves
 TreeAndLeaf implements a hybrid layout strategy that enhances leaf-level visualization in dendrograms. By integrating force-directed graph and tree layout algorithms, it enables projection of multiple layers of information onto graph–tree diagrams.

### Installation
TreeAndLeaf is availble from the Bioconductor repository:

```{r}
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("TreeAndLeaf")
```

For more information, visit the [TreeAndLeaf](https://www.bioconductor.org/packages/TreeAndLeaf/) landing page on Bioconductor.

