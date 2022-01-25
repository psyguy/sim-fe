# rm(list = ls())
source(here::here("functions",
                  "functions_self-sim-pipeline.R"))
library(here)

#' We already have the following:
#'  1. simulation reference table `sim_refs`
#'  2. simulation files for N=100, T=100 in `simulation-files/sim-files`
#'  3. fit reference table `fit_refs`
#'
#' We only need to subset the sim files for smaller N & T, and update the fit
#' reference table (`fit_refs`) such that the subset N & T values are included
#' in fit the file names (and respective Mplus files)
#'
#' I do this by:
#'  1. First making a `sim_refs` with smaller N & T (25, 50, 100) while
#' keeping uSeed intact, and then
#' 2. Change the `make_fit_refs` function such that the fit.File includes
#' N and T values, and finally
#' 3. Change `do_fit_parallel` to subset sim datasets prior to fitting.
#'
#' Note that we are no longer interested in DAR, so they should be left out.
#'


sim_refs.big <- readRDS(here::here("simulation-files/refs",
                                   "sim-refs_Model-BinAR.ChiAR.DAR.PoDAR_N-100_T-30.100.rds"))

sim_refs_base <- sim_refs.big %>%
  filter(T == 100)


sim_refs_subsets <- NULL

for(TT in c(25, 50, 100)){
  for(NN in c(25, 50, 100)){
    if(TT==100 & NN==100) next
    sim_refs_subsets <- sim_refs_base %>%
      mutate(N = NN,
             T = TT) %>%
      rbind(sim_refs_subsets)
  }
}


sim_refs <- sim_refs_subsets %>%
  filter(Model != "DAR")

fit_refs <- make_fit_refs(sim_refs = sim_refs,
                          save.directory = "simulation-files/fit-files")

saveRDS(fit_refs,
  here::here("simulation-files/refs",
             "fit-refs_Model-BinAR.ChiAR.PoDAR_N-25.50.100_T-25.50.100_iter-2000_thin-5_type-resid.fixed.random.rds")
  )

fit_refs <- readRDS(here::here("simulation-files/refs",
                   "fit-refs_Model-BinAR.ChiAR.PoDAR_N-25.50.100_T-25.50.100_iter-2000_thin-5_type-resid.fixed.random.rds")
)


fit.Files_done <- list.files(here::here("simulation-files/fit-files"),
                            pattern = "fit_uSeed-")

fit_refs_remaining <- fit_refs %>%
  filter(!(fit.File %in% fit.Files_done)) %>%
  filter(T != 100, N != 100)

Sys.time()
system.time(
  t.fit <- do_fit_doFuture(fit_refs = fit_refs_remaining,
                           nClust = 48,
                           nPROC = 1,
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
