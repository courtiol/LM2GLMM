## ----setup, include=FALSE------------------------------------------------
library(LM2GLMM)
set.seed(1L)

## ----load_package, eval = FALSE------------------------------------------
#  library(package = "LM2GLMM")

## ----vectors basics------------------------------------------------------
foo <- c(1, 5, 5, 10, 23, NA, NA)
foo[2]
foo > 3
foo[foo > 3]
foo2 <- c(a = 1, b = 5, c = 10, d = 23)
foo2["b"]

## ----vectors function----------------------------------------------------
length(foo)
summary(foo)
any(is.na(foo))
table(foo, useNA = "always")

## ----vectors classes-----------------------------------------------------
foo1 <- c(1, 5, 5, NA)
foo2 <- c(a = 1, b = 3)
foo3 <- foo1 > 3
foo4 <- c("a", "b", "c")
foo5 <- factor(foo3)
foo6 <- c(1L, 3L, 5L)

class(foo1)

## ----vectors class table, echo = FALSE-----------------------------------
classes <- unlist(lapply(as.list(names <- paste0("foo", 1:6)), function(x) class(get(x))))
knitr::kable(rbind(names, classes))

## ----vetors factors------------------------------------------------------
foo <-  factor(c("a", "b", "c"))
(foo <- foo[foo != "b"]) ## Tip: the brackets force the display
(foo <- droplevels(foo))

## ----attributes----------------------------------------------------------
foo <- c(a = 1, b = 5, c = 10, d = 23)
attributes(foo)
attr(foo, "names")  ## Tip: here a shortcut would be 'names(foo4)', but this is not general
foo <-  factor(c("a", "b", "c"))
attributes(foo)

## ----dataframe-----------------------------------------------------------
foo <- data.frame(
  x = c(1, 3, 5),
  z = factor(c("a", "b", "c"))
  )
foo
dim(foo) ## Tip: try nrow() and ncol()
dimnames(foo) ## Tip: try rownames() and colnames()

## ----dataframe indexing--------------------------------------------------
foo[2, ]
foo[, 2]
foo$x
foo[, "x"]

## ----matrix--------------------------------------------------------------
foo <- matrix(data = 1:4, nrow = 2, ncol = 2) ## Tip: try with byrow = TRUE
colnames(foo) <- c("a", "b"); rownames(foo) <- c("A", "B")
foo

## ----matrix indexing-----------------------------------------------------
foo[, 2]
foo[2, ]

## ----matrix indexing 2---------------------------------------------------
foo[1, 2]

## ----matrix transpose----------------------------------------------------
foo
t(foo)  ## transpose of foo
solve(foo)  ## inverse of foo

## ----matrix multiplication-----------------------------------------------
foo %*% matrix(c(-1, 1))  ## matrix multiplication; same as foo %*% c(0, 1, -1)
foo %*% foo  ## matrix multiplication
foo * foo  ## NOT MATRIX MULTIPLICATION!

## ----lists---------------------------------------------------------------
foo <- list("foo1" = c(1:10), "foo2" = factor(c("a", "b")))
foo
foo[["foo2"]]
foo$foo2

## ----function------------------------------------------------------------
addA_B <- function(a = 0, b = 0) {
  c <- a + b
  return(c)
  }

## ----function test-------------------------------------------------------
addA_B(a = 5, b = 7)
addA_B(a = 5)

## ----function short------------------------------------------------------
addA_B_bis <- function(a, b) a + b
addA_B_bis(2, 3)

## ----rnorm---------------------------------------------------------------
rnorm(5, mean = 2, sd = 0.1)  ## Tip: check ?Distributions for other distributions
rnorm(5, mean = 2, sd = 0.1)
set.seed(14353L)
rnorm(5, mean = 2, sd = 0.1)
set.seed(14353L)
rnorm(5, mean = 2, sd = 0.1)

