library(librarian)
shelf(tidyverse)
shelf(here)
shelf(primes)
library(glue)
rm(list = ls())

load(here::here("self-sim", "sim_refs.Rdata"))

hyperparameters <- list(iter = c(1000, 2000, 5000),
                        thin = c(1, 5, 10, 20))

save.directory <- "self-sim/fit_files"
dir.create(save.directory, showWarnings = FALSE)

nClust <- 8
sort.by <- NULL

h <- hyperparameters %>%
  expand.grid(stringsAsFactors = TRUE)

dd <- sim_refs[rep(1:nrow(sim_refs), times = nrow(h)),]
rownames(dd) <- NULL
hh <- h[rep(1:nrow(h), each = nrow(sim_refs)),]
rownames(hh) <- NULL

d <- cbind(dd,hh)
d <- d %>%
  mutate(fit.Path = save.directory,
         fit.File = NA,
         fit.StartTime = NA,
         fit.EndTime = NA,
         fit.ElapsedTime = NA)

for (r in 1:nrow(d)) {
  only.headers <- names(hyperparameters) %>% c("Rep", "uSeed",.)
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
  paste0("fit_",.)
}


# sorting by fit.File

d <- d %>%
  arrange(fit.File)


# Doing the parallel thing (ispired by parSim) ----------------------------

cl <- snow::makeSOCKcluster(nClust,
                            outfile = here::here(save.directory,
                                                 "clusterLOG.txt"))
debug <- TRUE

## Start clusters:
# Export the sim conditions:
snow::clusterExport(cl, c("d", "debug"), envir = environment())

# # Export global objects:
# if (!missing(export)){
#   snow::clusterExport(cl, export)
# }

# Run the loop:
Results <- snow::parLapply(cl = cl,
                           seq_len(nrow(d)),
                           function(i) {
                             source(here::here("functions",
                                               "functions_Mplus.R"))
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
                             d_i <- d[i, ]

                             d_i$fit.StartTime <- Sys.time()

                             # tryRes <-
                             #   try(output.dataset <- run_MplusAutomation()
                             # if (is(tryRes,"try-error")){
                             #   if (debug){
                             #     browser()
                             #   }
                             #   return(list(error = TRUE, errorMessage = as.character(tryRes), id = d$id[i]))
                             # }

                             df <- readRDS(file = here::here(d_i$sim.Path,
                                                             d_i$sim.File))
                             file.name <- gsub(".rds", "", d_i$fit.File)

                             output.fit <- run_MplusAutomation(df = df,
                                                               BITERATIONS.min = d_i$iter,
                                                               THIN = d_i$thin,
                                                               file.name = file.name)

                             d_i$fit.EndTime <- Sys.time()
                             d_i$fit.ElapsedTime <- d_i$sim.EndTime - d_i$sim.StartTime

                             d_i
                           })

fit_refs <- dplyr::bind_rows(Results)

# Stop the cluster:
snow::stopCluster(cl)

# Save the references data frame to a file
save(fit_refs,
     file = here::here("self-sim", "fit_refs.Rdata"))
write.csv(References_sim,
          file = here::here("self-sim", "fit_refs.csv"),
          row.names = FALSE)
