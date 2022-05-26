library(librarian)
shelf(tidyverse)
shelf(here)
shelf(primes)
library(glue)
rm(list = ls())

sim_refs <- readRDS(here::here("self-sim/experiment-b", "sim_refs.rds"))

# sim_refs <- sim_refs %>%
#   filter(l2.dist == "Gaussian")

hyperparameters <- list(iter = c(2000),
                        thin = c(5))

save.directory <- "self-sim/experiment-b"
dir.create(save.directory, showWarnings = FALSE)

nClust <- 48#/2
sort.by <- "uSeed"

h <- hyperparameters %>%
  expand.grid(stringsAsFactors = TRUE)

dd <- sim_refs[rep(1:nrow(sim_refs), times = nrow(h)),]
rownames(dd) <- NULL
hh <- h[rep(1:nrow(h), each = nrow(sim_refs)),]
rownames(hh) <- NULL

d <- cbind(dd,hh)
d <- d %>%
  mutate(fit.Path = save.directory,
         fit.File = NA
         # fit.StartTime = NA,
         # fit.EndTime = NA,
         # fit.ElapsedTime = NA
         ) %>%
  filter(T == 100) # not complicating the simulation


for (r in 1:nrow(d)) {
  only.headers <- names(hyperparameters) %>% c("uSeed",.,"Rep")
  r.values <-d[r, only.headers]
factor.columns <- sapply(r.values, is.factor)
r.values[factor.columns] <-
  sapply(r.values[factor.columns], as.character)
# only.headers[n.conditions-1] <- "rep"
d[r, "fit.File"] <- only.headers %>%
  paste0("-") %>%
  paste0(r.values) %>%
  paste(collapse = "_") %>%
  paste0(".rds") %>%
  paste0("fit_", .) # prefix for fitted datasets

}


# sorting by fit.File

d <- d %>%
  arrange(sort.by)


# Doing the parallel thing (ispired by parSim) ----------------------------

# Save the references data frame to a file
saveRDS(d,
        file = here::here(save.directory, "fit_refs.rds"))
write.csv(d,
          file = here::here(save.directory, "fit_refs.csv"),
          row.names = FALSE)


# Doing the parallel thing (inspired by parSim) ----------------------------


source(here::here("functions",
                  "functions_Mplus.R"))

cl <- snow::makeSOCKcluster(nClust,
                            outfile = here::here(save.directory,
                                                 "fit_clusterLOG.txt"))
debug <- TRUE

## Start clusters:
# Export the sim conditions:
snow::clusterExport(cl, c("d", "run_MplusAutomation", "debug"), envir = environment())


# # Export global objects:
# if (!missing(export)){
#   snow::clusterExport(cl, export)
# }

# Run the loop:

(snow.start <- Sys.time())

snow::clusterApplyLB(cl = cl,
                           seq_len(nrow(d)),
                           function(i) {

                             if (debug) {
                               cat("\nRunning iteration:",
                                   i,
                                   " / ",
                                   nrow(d),
                                   "\nTime:",
                                   as.character(Sys.time()),
                                   "\n")
                               print(d$sim.File)
                             }

                             # arguments <- split(d[1,1:length(conditions)],
                             #                    1)[[1]] %>%
                             #   as.list()
                             d_i <- as.list(d[i, ])

                             fit.StartTime <- Sys.time()

                             # tryRes <-
                             #   try(output.dataset <- run_MplusAutomation()
                             # if (is(tryRes,"try-error")){
                             #   if (debug){
                             #     browser()
                             #   }
                             #   return(list(error = TRUE, errorMessage = as.character(tryRes), id = d$id[i]))
                             # }

                             df <- readRDS(file = here::here(d_i$sim.Path,
                                                             d_i$sim.File))$sim.Dataset
                             file.name <- gsub(".rds", "", d_i$fit.File)

                             output.fit <- run_MplusAutomation(df = df,
                                                               PROCESSORS = 2,
                                                               BITERATIONS.min = d_i$iter,
                                                               THIN = d_i$thin,
                                                               file.name = file.name)

                             d_i$fit.Dataset <- output.fit
                             d_i$fit.StartTime <- fit.StartTime
                             d_i$fit.EndTime <- Sys.time()
                             d_i$fit.ElapsedTime <- d_i$sim.EndTime - d_i$sim.StartTime

                             saveRDS(d_i,
                                     file = here::here(d_i$fit.Path, d_i$fit.File))

                           })


# fit_refs <- dplyr::bind_rows(Results)

# Stop the cluster:
snow::stopCluster(cl)

# Save the references data frame to a file
# save(fit_refs,
#      file = here::here("self-sim", "fit_refs.Rdata"))
# write.csv(References_sim,
#           file = here::here("self-sim", "fit_refs.csv"),
#           row.names = FALSE)


(snow.end <- Sys.time())

snow.end - snow.start
