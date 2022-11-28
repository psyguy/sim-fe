## @knitr models

library(here)
library(glue)
library(tidyverse)
source(here("functions",
            "functions_data-generating-models.R"))


make_dgm.l2gaussian <- function(Model,
                              T,
                              N,
                              phi){
  if(tolower(Model) == "chiar" | tolower(Model) == "chi2ar"){
    model.name <- "Chi2AR"
    lev2.Mean <- 10
    lev2.Variance <- 10
  }
  if(tolower(Model) == "binar"){
    model.name <- "BinAR"
    lev2.Mean <- 2
    lev2.Variance <- 1
  }
  if(tolower(Model) == "dar"){
    model.name <- "DAR"
    lev2.Mean <- 2
    lev2.Variance <- 1
  }

  new_model(
    name = "skewed-timeseries",
    label = glue(
      "Population of {N} {model.name}(1) models with T = {T}, AR slope = {phi}, level2 Mean = {lev2.Mean}, level2 Variance = {lev2.Variance}"
    ),
    params = list(
      Model = Model,
      T = T,
      N = N,
      phi = phi
      # lev2.Mean = lev2.Mean,
      # lev2.Variance = lev2.Variance
    ),
    simulate = function(Model, T, N, phi, nsim) {
      # this function must return a list of length nsim
      # sampliong within-person mean from level 2 distribution
      Means <- rnorm(N, lev2.Mean, sqrt(lev2.Variance))

      o <- as.list(rep(NA, nsim))

      for(ns in 1:nsim){

        sample_df <- dgm_make.sample(Model = Model,
                                         Means = Means,
                                         T = T,
                                         phi = 0.4,
                                         seeds = NULL)

        o[[ns]] <- sample_df
      }
      return(o) # make each col its own list element
    }
  )
}



# make_dgm <- function(Model, n, prob) {
#   new_model(name = "contaminated-normal",
#             label = sprintf("Contaminated normal (n = %s, prob = %s)", n, prob),
#             params = list(n = n, mu = 2, prob = prob),
#             simulate = function(n, mu, prob, nsim) {
#               # this function must return a list of length nsim
#               contam <- runif(n * nsim) < prob
#               x <- matrix(rep(NA, n * nsim), n, nsim)
#               x[contam] <- rexp(sum(contam))
#               x[!contam] <- rnorm(sum(!contam))
#               x <- mu + x # true mean is mu
#               return(split(x, col(x))) # make each col its own list element
#             })
# }
