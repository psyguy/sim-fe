rm(list = ls())
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
