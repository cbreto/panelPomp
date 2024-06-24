if (file.exists("_options.R")) source("_options.R")
library(panelPomp,quietly=TRUE)

TESTS_PASS <- NULL
test <- function(expr1,expr2,all="TESTS_PASS",env=parent.frame(),...)
  panelPomp:::test(expr1,expr2,all=all,env=env,...)


NSEQ <- 43L

test(
  {
    df <- runif_panel_design(
      lower = c('a' = 0, 'b' = 10),
      upper = c('a' = 1, 'b' = 15),
      nseq = NSEQ
    )
    c(nrow(df), ncol(df))
  },
  c(NSEQ, 2L)
)

test(
  {
    df <- runif_panel_design(
      lower = c('a' = 0, 'b' = 10, 'a[u2]' = 0.5),
      upper = c('a' = 1, 'b' = 15, 'a[u2]' = 0.75),
      specific_names = c('a'),
      unit_names = paste0(rep('u', 5), 1:5),
      nseq = NSEQ
    )
    c(nrow(df), ncol(df))
  },
  c(NSEQ, 6L)
)

err <- wQuotes("Error : in ''runif_panel_design'': upper values should be at least as large as lower ones.\n")

test(
  runif_panel_design(
    lower = c('a' = 1),
    upper = c('a' = 0),
    nseq = NSEQ
  ),
  err
)

err <- wQuotes("Error : in ''runif_panel_design'': ''lower'' and ''upper'' must have the same length.\n")

test(
  runif_panel_design(
    lower = c('a' = 0, 'b' = 0),
    upper = c('a' = 1),
    nseq = NSEQ
  ),
  err
)

err <- wQuotes("Error : in ''runif_panel_design'': ''lower'' and ''upper'' must be named vectors.\n")

test(
  runif_panel_design(
    lower = c(0, 0),
    upper = c(1, 1),
    nseq = NSEQ
  ),
  err
)

err <- wQuotes("Error : in ''runif_panel_design'': names of ''lower'' and ''upper'' must match.\n")

test(
  runif_panel_design(
    lower = c('b' = 0),
    upper = c('a' = 1),
    nseq = NSEQ
  ),
  err
)

err <- wQuotes("Error : in ''runif_panel_design'': ''nseq'' must be greater than zero.\n")

test(
  runif_panel_design(
    lower = c('a' = 0),
    upper = c('a' = 1),
    nseq = -1
  ),
  err
)

err <- wQuotes("Error : in ''runif_panel_design'': No bounds were given for some parameters in ''specific_names''.\n")

test(
  runif_panel_design(
    lower = c('a' = 0),
    upper = c('a' = 1),
    specific_names = c('b'),
    unit_names = c(paste0("u", 1:5)),
    nseq = NSEQ
  ),
  err
)

err <- wQuotes("Error : in ''runif_panel_design'': If used, both ''specific_names'' and ''unit_names'' must be provided.\n")

test(
  runif_panel_design(
    lower = c('a' = 0, 'b' = 0),
    upper = c('a' = 1, 'b' = 1),
    specific_names = c('b'),
    nseq = NSEQ
  ),
  err
)

## check whether all tests passed
all(get(eval(formals(test))$all))
if (!all(get(eval(formals(test))$all))) stop("Not all tests passed!")
