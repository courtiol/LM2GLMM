# LM2GLMM

This is the repository for the R package LM2GLMM.
This R package is primarily aimed at the students of the course I am giving at the 
Freie Universität, Berlin. Without the bla bla it will be difficult to follow...

## Installation

1. install the R package ```drat``` (only if you don't have it):

```{r, eval = FALSE}
install.packages("drat")
```

2. install the R package LM2GLMM:

```{r, eval = FALSE}
drat::addRepo("courtiol")
install.packages("LM2GLMM")
```


## Other tips

To load the package and access to the vignettes:

```{r, eval = FALSE}
library("LM2GLMM")
browseVignettes(package = "LM2GLMM")  ## see vignettes in your web browser
get_vignettes() ## see folder cointaining vignettes as *.html files
```
