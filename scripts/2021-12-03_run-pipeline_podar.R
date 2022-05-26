# rm(list = ls())
source(here::here("functions",
                  "functions_self-sim-pipeline.R"))

# # We already have the simulation references table
sim_refs_with.PoDAR <- make_sim_refs(conditions = list(T = c(30, 100),
                                                N = c(100),
                                                Model = c("BinAR",
                                                          "Chi2AR",
                                                          "DAR",
                                                          "PoDAR"),
                                                l2.dist = c("Gaussian", "Chi2"),
                                                phi = c(0.4)
                                                ),
save.directory = "simulation-files/sim-files",
                              Reps = 1000)

# saveRDS(sim_refs_with.PoDAR,here::here("simulation-files/refs",
#                                    "sim-refs_Model-BinAR.ChiAR.DAR.PoDAR_N-100_T-30.100.rds"))

sim_refs.big <- readRDS(here::here("simulation-files/refs",
                                   "sim-refs_Model-BinAR.ChiAR.DAR.PoDAR_N-100_T-30.100.rds"))

sim_refs <- sim_refs.big %>%
  filter(T == 100,
         Model == "PoDAR")


Sys.time()
system.time(
t.sim <- do_sim_parallel(sim_refs = sim_refs,
                         save.directory = "simulation-files/sim-files"
                         )
)
Sys.time()

# We already have the fit references table
fit_refs <- make_fit_refs(sim_refs = sim_refs,
                          save.directory = "simulation-files/fit-files")


# # Manually making a new fit_refs for resid-fixed
# fit_refs <- readRDS(here::here("simulation-files/refs",
#                     "fit-refs_Model-BinAR.ChiAR.DAR_N-100_T-100_iter-2000_thin-5_type-resid.random.rds"))

fit_refs_resid.random <- fit_refs
fit_refs_resid.random$type <- "resid.random"
fit_refs_resid.random$fit.File <- fit_refs_resid.random$fit.File %>%
  gsub("_thin-5_Rep-",
       "_thin-5_type-resid.random_Rep-",
       .)

fit_refs_resid.fixed <- fit_refs
fit_refs_resid.fixed$type <- "resid.fixed"
fit_refs_resid.fixed$fit.File <- fit_refs_resid.fixed$fit.File %>%
  gsub("_thin-5_Rep-",
       "_thin-5_type-resid.fixed_Rep-",
       .)

fit_refs <- rbind(fit_refs_resid.random, fit_refs_resid.fixed)

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

harvest.dir <- "simulation-files/fit-files/podar-fits"
l.files <- list.files(path = here(harvest.dir),
                      pattern = ".rds")


fit.files <- l.files %>%
  here(harvest.dir, .) %>%
  file.info() %>%
  filter(size > 512000) %>%
  rownames()

# harv <- do_harvest_parallel(fit.files)


plan("multisession")

system.time(
harv <- plyr::ldply(fit.files,
                    fit_extract,
                    .parallel = TRUE)
)

saveRDS(harv,
        "fit-harvest_N-100_T-100_PoDAR_resid-fixed.random.rds")



# save t.fit_540-reps_nClust-48_nPROC-2, 1500x1500
