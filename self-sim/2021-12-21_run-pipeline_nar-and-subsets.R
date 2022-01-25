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

sim_refs_with.NAR <- make_sim_refs(conditions = list(T = c(30, 100),
                                                     N = c(100),
                                                     Model = c("BinAR",
                                                               "Chi2AR",
                                                               "DAR",
                                                               "PoDAR",
                                                               "NAR"),
                                                       l2.dist = c("Gaussian",
                                                                   "Chi2"),
                                                       phi = c(0.4)
                                                     ),
                                   save.directory = "simulation-files/sim-files",
                                   Reps = 1000)

saveRDS(sim_refs_with.NAR,
        here::here("simulation-files/refs",
                   "sim-refs_Model-BinAR.ChiAR.DAR.PoDAR.NAR_N-100_T-30.100.rds")
)

sim_refs.big <- readRDS(here::here("simulation-files/refs",
                                   "sim-refs_Model-BinAR.ChiAR.DAR.PoDAR.NAR_N-100_T-30.100.rds"))


sim_refs_base <- sim_refs.big %>%
  filter(T == 100)

sim_refs_only.NAR <- sim_refs_base %>%
  filter(Model == "NAR")

## Simulating NAR for N=100, T=100

Sys.time()
system.time(
  t.sim <- do_sim_parallel(sim_refs = sim_refs_only.NAR,
                           save.directory = "simulation-files/sim-files"
  )
)
Sys.time()




sim_refs_subsets <- NULL

for(TT in c(25, 50, 100)){
  for(NN in c(25, 50, 100)){
    # if(TT==100 & NN==100) next
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
             "fit-refs_Model-BinAR.ChiAR.PoDAR.NAR_N-25.50.100_T-25.50.100_iter-2000_thin-5_type-resid.fixed.random.rds")
  )

fit_refs <- readRDS(here::here("simulation-files/refs",
                   "fit-refs_Model-BinAR.ChiAR.PoDAR.NAR_N-25.50.100_T-25.50.100_iter-2000_thin-5_type-resid.fixed.random.rds")
)

fit_refs_all <- fit_refs

fit.Files_done <- list.files(here::here("simulation-files/fit-files"),
                            pattern = "fit_uSeed-")

fit_refs_remaining <- fit_refs_all %>%
  filter(!(fit.File %in% fit.Files_done)) %>%
  filter(T != 100, N != 100) # older DGMs have different fit names
  # filter(T == 100, N == 100)


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
print("fit of remaining T != 100, N != 100 of everything (1770 replications) completed, which had started at 2022-01-02 10:28:16")

# Putting together the index and harvest files --------------------------------------

harvest.dir <- "simulation-files/fit-files"
l.files <- list.files(path = here(harvest.dir),
                      pattern = glob2rx("*_N-25_T-25*.rds"))


fit.files <- l.files %>%
  here(harvest.dir, .) %>%
  file.info() %>%
  filter(size > 9000) %>%
  rownames()

# harv <- do_harvest_parallel(fit.files)


plan("sequential")

harv <- h %>% filter(uSeed == 0)

system.time(
for(f in 1:length(fit.files)){
  if(!(f %% 100)) print(f)
  f.h <- fit_extract(fit.files[f])
  if(!is.data.frame(f.h)) next
  harv <- rbind(harv, f.h)
}
)

# system.time(
# harv <- plyr::ldply(fit.files,
#                     fit_extract,
#                     .parallel = FALSE)
# )

saveRDS(harv,
        "fit-harvest_N-25_T-25_BinAR.ChiAR.PoDAR.NAR_resid-fixed.random.rds")



# save t.fit_540-reps_nClust-48_nPROC-2, 1500x1500
