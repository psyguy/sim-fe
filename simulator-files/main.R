# This is the main simulator file

library(simulator) # this file was created under simulator version 0.2.3

source("model_functions.R")
source("method_functions.R")
source("eval_functions.R")

## @knitr init

name_of_simulation <- "experiment-mplu-hyperparameters"

## @knitr main
Sys.time()
system.time(
sim <- new_simulation(name = name_of_simulation,
                      label = "Deciding which hyperparameters (No. iterations, thinning) to use in Mplus input.") %>%
  generate_model(make_dgm.l2gaussian, seed = 123,
                 Model = as.list(c("ChiAR", "BinAR", "DAR")),
                 T = as.list(c(30, 100, 1000)),
                 N = as.list(c(100)),
                 phi = 0.4,
                 vary_along = c("Model", "T", "N")) %>%
  simulate_from_model(nsim = 1,
                      index = 50:60,
                      parallel = list(socket_names = 3)
  )
) %>%
  run_method(list(mpa_it.1k_th.01,
                  mpa_it.1k_th.02,
                  mpa_it.1k_th.10,
                  mpa_it.1k_th.20,
                  mpa_it.2k_th.01,
                  mpa_it.2k_th.02,
                  mpa_it.2k_th.10,
                  mpa_it.2k_th.20,
                  mpa_it.5k_th.01,
                  mpa_it.5k_th.02,
                  mpa_it.5k_th.10,
                  mpa_it.5k_th.20))
# %>%
  # evaluate(list(his_loss, her_loss))

## @knitr plots

plot_eval_by(sim, "hisloss", varying = "prob")

## @knitr tables

tabulate_eval(sim, "herloss", output_type = "markdown",
              format_args = list(digits = 1))
