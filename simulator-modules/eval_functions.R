## @knitr metrics

# init --------------------------------------------------------------------


library(tidyverse)
library(here)
library(MplusAutomation)
library(simulator)


# trying to write an extractor function for MpA objects -------------------

m <- readModels(here("Mplus-files",
                     "chiar_n_100_t_100_it-5000_th-20_no-45.out"))

d.stdyx <- m[["parameters"]][["stdyx.standardized"]]

# plott mcmc chains -------------------------------------------------------

posterior_draws <- m$gh5$bayesian_data$parameters_autocorr$parameters %>%
  aperm(c(2,3,1))
# remove brunin
burn_in <- 1000
posterior_draws <- posterior_draws[(burn_in+1):dim(posterior_draws)[[1]],,]

param.names <- paste(m[["parameters"]][["unstandardized"]][["paramHeader"]],
                     m[["parameters"]][["unstandardized"]][["param"]],
                     sep = ".")
dimnames(posterior_draws)[[3]] <- param.names

library(hexbin)
library(bayesplot)

# mcmc_areas(posterior_draws,
#            param.names[1:3])

p_pairs <- mcmc_pairs(posterior_draws,
           param.names,
           diag_fun = "hist",
           off_diag_fun = "hex"
           )

p_acf <- mcmc_acf(posterior_draws,
         param.names,
         lags = 30) +
  hline_at(c(-0.1,0.1),
           linetype = 2,
           size = 0.15,
           color = "gray") +
  ggtitle(m[["input"]][["title"]])

cowplot::plot_grid(p_acf, p_pairs,
                   nrow = 2,
                   rel_heights = c(1,2))

mcmc_rhat_hist(posterior_draws)

# sample metrics ----------------------------------------------------------


stdyx <- new_metric("stdyx",
                    "Extract STDYX outputs of Mplus analysis",
                    metric = function(model, out){
                      d <- out[["parameters"]][["stdyx.standardized"]]
                      return(d)
                      }
                    )

his_loss <- new_metric("hisloss", "His loss function",
                        metric = function(model, out) {
                          return((model$mu - out$fit)^2)
})

her_loss <- new_metric("herloss", "Her loss function",
                        metric = function(model, out) {
                          return(abs(model$mu - out$fit))
                        })
