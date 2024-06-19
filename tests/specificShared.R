## test codes in R/panelPomp_methods.R
if (file.exists("_options.R")) source("_options.R")
library(panelPomp,quietly=TRUE)

TESTS_PASS <- NULL
test <- function(expr1,expr2,all="TESTS_PASS",env=parent.frame(),...)
  panelPomp:::test(expr1,expr2,all=all,env=env,...)

ppo <-panelGompertz(U=5,N=5)

# shared, panelPomp-method

test(
  shared(ppo), ppo@shared
)

# specific, panelPomp-method

test(
  specific(ppo), ppo@specific
)

test(
  specific(ppo, format = 'vector'), coef(ppo)[grepl('^.*\\[.+\\]$', names(coef(ppo)))]
)

# shared<-, panelPomp-method

test(
  c(shared(ppo), 'tau' = 0.1),
  {shared(ppo) <- c(shared(ppo), 'tau' = 0.1); shared(ppo)}
)

test(
  setequal(
    c(r = 0.1, sigma = 0.5, tau = 0.1),
    {shared(ppo) <- c("sigma" = 0.5); shared(ppo)}
  ),
)

err <- wQuotes("Error : in ''shared<-'': ''value'' contains parameters not found in ''object''.\n")

test(
  shared(ppo) <- c("foobar" = 1),
  err
)

# specific<-, panelPomp-method

test(
  ppo@specific,
  matrix(rep(1, 10), nrow = 2, dimnames = list(c("K", "X.0"), paste0("unit", 1:5)))
)

test(
  ppo@shared,
  c('sigma' = 0.5, 'r' = 0.1, 'tau' = 0.1),
)

test(
  {
    sigma_shared <- shared(ppo)['sigma']
    specific(ppo) <- c('sigma[unit3]' = 0.75)
    all(

      # Check there is a row of all same sigma values, except unit3, which changed.
      all.equal(
        unname(specific(ppo)['sigma', ]),
        unname(c(sigma_shared, sigma_shared, 0.75, sigma_shared, sigma_shared))
      ),

      # Check sigma is no longer shared
      !c('sigma') %in% names(shared(ppo))
    )
  }
)

test(
  {
    r_shared <- shared(ppo)['r']
    specific(ppo) <- matrix(
    c(0.1, 0.2, 1, 2),
    byrow = TRUE, nrow = 2,
    dimnames = list(param = c('r', 'K'), unit = c('unit4', 'unit5'))
  )
  all(
    # Check r is in specific, and specified units match
    all.equal(
      ppo@specific['r', c('unit4', 'unit5')], c('unit4' = 0.1, 'unit5' = 0.2)
    ),

    # Check non-specified r values are unchanged
    all.equal(
      r_shared, ppo@specific['r', 'unit1'], ppo@specific['r', 'unit2'], ppo@specific['r', 'unit2']
    )
  )
  }
)

err1 <- wQuotes("Error : in ''specific<-'': ''value'' contains unit names not in ''object''.\n")
err2 <- wQuotes("Error : in ''specific<-'': ''value'' contains parameters not found in ''object''.\n")
err3 <- wQuotes("Error : in ''specific<-'': names of ''value'' must end in ''[unit_name]''.\n")

test(
  specific(ppo) <- c('K[unit101]' = 1),
  err1
)

test(
  specific(ppo) <- c("foo[unit1]" = 1),
  err2
)

test(
  specific(ppo) <- c("tau" = 0.1),
  err3
)

test(
  specific(ppo) <- matrix(1, dimnames = list(param = c("K"), unit = 'unit101')),
  err1
)

test(
  specific(ppo) <- matrix(1, dimnames = list(param = c("foo"), unit = 'unit1')),
  err2
)

## show
show(ppo)
show(panelPomp(unit_objects(ppo)))

## check whether all tests passed
all(get(eval(formals(test))$all))
if (!all(get(eval(formals(test))$all))) stop("Not all tests passed!")
