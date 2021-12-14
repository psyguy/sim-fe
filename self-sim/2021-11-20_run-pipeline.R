# rm(list = ls())
library(tidyverse)
source(here::here("functions",
                  "functions_self-sim-pipeline.R"))

sim_refs.big <- make_sim_refs(save.directory = "self-sim_dar",
                              Reps = 1000)

sim_refs.big <- readRDS("~/sim-floor-effect/self-sim_dar/sim_refs 2021-11-20.rds")

sim_refs <- sim_refs.big %>%
  filter(T == 100,
         Model == "DAR")


Sys.time()
system.time(
t.sim <- do_sim_parallel(sim_refs = sim_refs,
                           save.directory = "self-sim_dar"
                           )
)


fit_refs <- make_fit_refs(sim_refs = sim_refs,
                          save.directory = "self-sim_dar")

fit_refs <- readRDS("~/sim-floor-effect/self-sim_dar/fit_refs 2021-11-20.rds")

fit.Files_done <- list.files(here::here("self-sim_dar"),
                            pattern = "fit_uSeed-")

fit_refs_remaining <- fit_refs %>%
  filter(!(fit.File %in% fit.Files_done))

Sys.time()
system.time(
  t.fit <- do_fit_parallel(fit_refs = fit_refs_remaining,
                           nClust = 48,
                           nPROC = 2,
                           sleeptime = 3,
                           save.directory = "self-sim_dar"
  )
)
Sys.time()


# Putting together the refs and harvest files --------------------------------------

# # No need to run anymore

sim_refs_all <- readRDS("~/sim-floor-effect/simulation-files/refs/sim_refs 2021-11-20.rds")

sim_refs_all$sim.Path <- "simulation-files/sim-files"

saveRDS(sim_refs_all,
        "sim-refs_Model-BinAR.ChiAR.DAR_N-100_T-100.rds")


fit_refs_binar.chiar <- readRDS("~/sim-floor-effect/self-sim/experiment-b/fit_refs.rds")
fit_refs_dar <- readRDS("~/sim-floor-effect/simulation-files/refs/fit_refs 2021-11-20.rds")

fit_refs_all <- rbind(fit_refs_binar.chiar, fit_refs_dar) %>%
  mutate(type = "resid.random",
         .after = thin)

fit_refs_all$sim.Path <- "simulation-files/sim-files"
fit_refs_all$fit.Path <- "simulation-files/fit-files"


saveRDS(fit_refs_all,
        "fit-refs_Model-BinAR.ChiAR.DAR_N-100_T-100_iter-2000_thin-5_type-resid.random.rds")


harv_binar.chiar <- readRDS("fit-harvest_all_binar-chiar.rds")
harv_dar <- readRDS("fit-harvest_all_dar.rds")

harv <- rbind(harv_binar.chiar, harv_dar)

saveRDS(harv,
        "fit-harvest_N-100_T-100_BinAR.ChiAR.DAR_resid-random.rds")



# save t.fit_540-reps_nClust-48_nPROC-2, 1500x1500
