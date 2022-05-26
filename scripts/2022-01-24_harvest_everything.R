# rm(list = ls())
source(here::here("functions",
                  "functions_self-sim-pipeline.R"))

#' We need to
#'  1. remake `sim_refs` to include NAR
#'  2. simulate NAR for N=100, T=100 in `simulation-files/sim-files`
#'  3. make reference table `fit_refs`
#'
#' Then need to subset the sim files for smaller N & T, and update the fit
#' reference table (`fit_refs`) such that the subset N & T values are included
#' in fit the file names (and respective Mplus files)
#'
#' I do this by:
#'  1. First making a `sim_refs` with smaller N & T (25, 50, 100) while
#' keeping uSeed intact, and then
#'  2. Change the `make_fit_refs` function such that the fit.File includes
#' N and T values, and finally
#'  3. Change `do_fit_parallel` to subset sim datasets prior to fitting.
#'
#' Note that we are no longer interested in DAR, so they should be left out.
#'


# Putting together the index and harvest files --------------------------------------

harvest.dir <- "simulation-files/fit-files"
l.files <- list.files(path = here(harvest.dir),
                      pattern = glob2rx("fit_uSeed*.rds"))


fit.files <- l.files %>%
  here::here("simulation-files/fit-files",.)

Sys.time()
system.time(
harv <- do_harvest_doFuture(fit.files)
)
print("Harvest of 74,000 fit files started at 2022-01-24 11:11:17 finished at")
Sys.time()


saveRDS(harv,
        "fit-harvest_2022-01-24_74000-files.rds")

