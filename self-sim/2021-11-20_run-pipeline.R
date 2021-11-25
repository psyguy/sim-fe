# rm(list = ls())
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


# save t.fit_540-reps_nClust-48_nPROC-2, 1500x1500
