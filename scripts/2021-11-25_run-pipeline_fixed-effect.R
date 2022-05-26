# rm(list = ls())
source(here::here("functions",
                  "functions_self-sim-pipeline.R"))

# # We already have the simulation references table
# sim_refs.big <- make_sim_refs(save.directory = "simulation-files/refs",
#                               Reps = 1000)

sim_refs.big <- readRDS(here::here("simulation-files/refs",
                                   "sim-refs_Model-BinAR.ChiAR.DAR_N-100_T-100.rds"))

sim_refs <- sim_refs.big %>%
  filter(T == 100)


# # We already have the simulation datasets
# Sys.time()
# system.time(
# t.sim <- do_sim_parallel(sim_refs = sim_refs,
#                          save.directory = "self-sim_dar"
#                          )
# )

# # We already have the fit references table
# fit_refs <- make_fit_refs(sim_refs = sim_refs,
#                           save.directory = "self-sim_dar")


# # Manually making a new fit_refs for resid-fixed
fit_refs <- readRDS(here::here("simulation-files/refs",
                    "fit-refs_Model-BinAR.ChiAR.DAR_N-100_T-100_iter-2000_thin-5_type-resid.random.rds"))

fit_refs$type <- "resid.fixed"
fit_refs$fit.File <- fit_refs$fit.File %>%
  gsub("_thin-5_Rep-",
       "_thin-5_type-resid.fixed_Rep-",
       .)

fit.Files_done <- list.files(here::here("simulation-files/fit-files"),
                            pattern = "fit_uSeed-")

fit_refs_remaining <- fit_refs %>%
  filter(!(fit.File %in% fit.Files_done))

Sys.time()
system.time(
  t.fit <- do_fit_parallel(fit_refs = fit_refs_remaining,
                           nClust = 48,
                           nPROC = 2,
                           sleeptime = 3,
                           save.directory = "simulation-files/fit-files"
  )
)
Sys.time()


# Putting together the index and harvest files --------------------------------------

harvest.dir <- "simulation-files/fit-files"
l.files <- list.files(path = here(harvest.dir),
                      pattern = ".rds")


fit.files <- l.files %>%
  here(harvest.dir, .)


harv_fixed.resid <- do_harvest_parallel(fit.files)

harv <- rbind(harv_binar.chiar, harv_dar)

saveRDS(results,
        "fit-harvest_N-100_T-100_BinAR.ChiAR.DAR_resid-fixed.rds")



# save t.fit_540-reps_nClust-48_nPROC-2, 1500x1500
