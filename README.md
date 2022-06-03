# LM2GLMM

This is the repository for the R package LM2GLMM.
This R package is primarily aimed at the students of the course I am giving at the 
Freie Universität, Berlin. Without the BLA BLA it may be difficult to follow...

## For students and other users

### Installation

1. install the R package ```drat``` (only if you don't have it):

```{r, eval = FALSE}
install.packages("drat")
```

2. install the R package LM2GLMM:

```{r, eval = FALSE}
drat::addRepo("courtiol")
install.packages("LM2GLMM")
```

### Usage

To load the package and access the vignettes (i.e. the course):

```{r, eval = FALSE}
library("LM2GLMM")
browseVignettes(package = "LM2GLMM")  ## see vignettes in your web browser
get_vignettes() ## see folder cointaining vignettes as *.html files
```

## For developers

### Procedure to update the package

1. install & restart (click on the R studio button); repeat twice if small issues.

2. make sure the working directory is the one of the project (and make sure you have deleted the cache and the cached figured if you want all chunks to be rerun and not just the one you changed), then:

```{r, eval = FALSE}
LM2GLMM:::.build_vignettes()
LM2GLMM:::.update_drat()
```

Note: the building of some vignette takes a lot of time. So you may want to knit the particular vignette you are revising directly to update the cache and then follow the instructions just mentioned.

3. commit and push

### Check

To check that it is all fine, just do what users do:
  
```{r, eval = FALSE}
drat::addRepo("courtiol")
install.packages("LM2GLMM")
library(LM2GLMM)
browseVignettes(package = 'LM2GLMM')
```

