## scVisualizeR

A simple package with a few functions for scRNAseq gene expression visualization. Designed as an addon for Monocle (https://github.com/cole-trapnell-lab/monocle-release). 

This package utilizes UMAP (https://github.com/lmcinnes/umap) emeddings and thus requires `umap-learn` in order to work. I suggest installing it as follows:

```
library(reticulate)
py_install("umap-learn")
```



Install scVisualizeR using the following:

```
library(devtools)
install_github('jaugust7/scVisualizeR')
```
Note: This package is still in development and was designed to make my life easier. It may need tweaking in order to work on your system. I'll add updates as I'm made aware of bugs that prevent it working on other setups.

Cheers!

-J
