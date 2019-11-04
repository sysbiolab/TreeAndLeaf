# TreeAndLeaf 
A R package for reorganizing dendrograms and adding additional layers of information.

## Installation
Install TreeAndLeaf from github

```{r}
if (! requireNamespace("devtools", quietly = TRUE))
install.packages("devtools")
devtools::install_github("luisrizzardi/TreeAndLeaf", force = TRUE, build_vignettes = TRUE)
```

For more info on how to use the package, check its vignette:

```{r}
vignette("TreeAndLeaf")
```