library(librarian)
shelf(tidyverse)
shelf(here)
shelf(primes)
library(glue)
# source(here("functions",
#             "functions_data-generating-models.R"))
rm(list = ls())


# data generating mechanism -----------------------------------------------

make_datasets <- function(Model = "DAR",
                            T = 100,
                            N = 100,
                            phi = 0.4,
                            l2.distribution = "Gaussian",
                            seed = 0) {
  # save global seed of the global env and set it back before leaving
  seed.old <- .Random.seed
  on.exit({
    .Random.seed <<- seed.old
  })
  set.seed(seed)

  if (tolower(Model) == "chiar" | tolower(Model) == "chi2ar") {
    model.name <- "Chi2AR"
    lev2.Mean <- 10
    lev2.Variance <- 10
    chi2.df <- 5
  }
  if (tolower(Model) == "binar") {
    model.name <- "BinAR"
    lev2.Mean <- 2
    lev2.Variance <- 1
    chi2.df <- 2.9
  }
  if (tolower(Model) == "dar") {
    model.name <- "DAR"
    lev2.Mean <- 2
    lev2.Variance <- 1
    chi2.df <- 2.9
  }

  # sampling within-person mean from level 2 distribution
  if (l2.distribution == "Gaussian")
    Means <- rnorm(2 * N, lev2.Mean, sqrt(lev2.Variance))
  if (l2.distribution == "Chi2")
    Means <- rchisq(2 * N, chi2.df)

  # removing out-of-bounds means
  Means[Means < 0] <- NA
  Means[Means > 100] <- NA
  if (model.name == "BinAR" |
      model.name == "DAR")
    Means[Means > 10] <- NA

  # keeping N samples from the in-bound means
  Means <- Means %>% na.omit() %>% sample(N)


  sample_df <- dgm_make.sample(
    Model = Model,
    Means = Means,
    T = T,
    phi = phi,
    seeds = NULL
  )
  return(sample_df)
}


# making the conditions dataframe -----------------------------------------


conditions <- list(
  T = c(30, 100, 1000),
  N = c(100),
  Model = c("BinAR", "Chi2AR", "DAR"),
  # Model = c("BinAR", "Chi2AR"),
  l2.dist = c("Gaussian", "Chi2"),
  phi = c(0.4)
)

Reps <- 4
seed <- 0
save.directory <- "self-sim/experiment-a_DAR"
nClust <- 48
sort.by <- NULL

# allow Reps to be used as a vector of indexes
if(length(Reps)<2) Reps = seq_len(Reps)

dir.create(save.directory, showWarnings = FALSE)

# save global seed of the global env and set it back before leaving
seed.old <- .Random.seed
on.exit({
  .Random.seed <<- seed.old
})

# soting conditions alphabetically
conditions <- conditions[order(names(conditions))]

conditions$sim.Seed <- seed
conditions$Rep <- Reps

n.conditions <- length(conditions)

# making the first columns of the data frame
d <- conditions %>%
  expand.grid(stringsAsFactors = TRUE)

# transforming factors to numerics
d.numeric <- d
factor.columns <- sapply(d.numeric, is.factor)
d.numeric[factor.columns] <-
  sapply(d.numeric[factor.columns], as.numeric)

# getting rid of non-integers and Rep
d.integer <- d.numeric[,-n.conditions] %>%
  apply(2,
        function(x)
          x %>%
          as.character() %>%
          gsub("\\.", "", .) %>%
          as.numeric())

# to make unique seeds, we must sum conditions weighted by prime numbers
# but the primes must not be among prime factors of conditions

# conditions prime factors
cpfs <- d.integer %>%
  unlist() %>%
  as.numeric() %>%
  unique() %>%
  primes::prime_factors() %>%
  unlist() %>%
  unique()

primes.seq <- c(cpfs,
                primes::generate_n_primes(ncol(d.integer) + length(cpfs)))

primes.seq <-
  primes.seq[!(duplicated(primes.seq) |
                 duplicated(primes.seq, fromLast = TRUE))]

uSeed <- d.integer %*% primes.seq %>%
  as.character() %>%
  paste0(as.character(d.numeric$Rep),
         .)

d.headers <- d %>%
  mutate(
    uSeed = uSeed,
    sim.Path = save.directory,
    sim.File = NA,
    sim.StartTime = NA,
    sim.EndTime = NA,
    sim.ElapsedTime = NA
  )


for (r in 1:nrow(d.headers)) {
  only.headers <- names(conditions)[-(n.conditions - 1)] %>%
    c("uSeed")
  r.values <- d.headers[r, only.headers]
  factor.columns <- sapply(r.values, is.factor)
  r.values[factor.columns] <-
    sapply(r.values[factor.columns], as.character)
  # only.headers[n.conditions-1] <- "rep"
  d.headers[r, "sim.File"] <- only.headers %>%
    paste0("-") %>%
    paste0(r.values) %>%
    paste(collapse = "_") %>%
    paste0(".rds") %>%
    paste0("sim_", .) # prefix for simulated datasets
}

if (is.null(sort.by))
  sort.by <- "uSeed"

d <- d.headers

# sorting per variable - doesn't work
# d <- d %>%
#   arrange(!!as.name(sort.by))

# getting rid of factors
factor.columns <- sapply(d, is.factor)
d[factor.columns] <- sapply(d[factor.columns], as.character)

rm(
  conditions,
  d.headers,
  d.integer,
  d.numeric,
  r.values,
  cpfs,
  factor.columns,
  n.conditions,
  Reps,
  only.headers,
  primes.seq,
  r
)

d <- d %>% filter(Model == "DAR")

# Doing the parallel thing (ispired by parSim) ----------------------------

cl <- snow::makeSOCKcluster(nClust,
                            outfile = here::here(save.directory,
                                                 "clusterLOG.txt"))
debug <- TRUE

## Start clusters:
# Export the sim conditions:
snow::clusterExport(cl, c("d", "make_datasets", "debug"), envir = environment())

# # Export global objects:
# if (!missing(export)){
#   snow::clusterExport(cl, export)
# }

# Run the loop:
Results <- snow::parLapply(cl = cl,
                           seq_len(nrow(d)),
                           function(i) {
                             source(here::here("functions",
                                               "functions_data-generating-models.R"))
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
                             arguments <-
                               as.list(d_i[1:(length(d_i) - 8)]) # %>% as.list()
                             arguments$seed <- d_i$uSeed


                             d_i$sim.StartTime <- Sys.time()

                             tryRes <-
                               try(output.dataset <- do.call(make_datasets, arguments))
                             # if (is(tryRes,"try-error")){
                             #   if (debug){
                             #     browser()
                             #   }
                             #   return(list(error = TRUE, errorMessage = as.character(tryRes), id = d$id[i]))
                             # }
                             saveRDS(output.dataset,
                                  file = here::here(d_i$sim.Path, d_i$sim.File))
                             d_i$sim.EndTime <- Sys.time()
                             d_i$sim.ElapsedTime <- d_i$sim.EndTime - d_i$sim.StartTime

                             d_i
                           })

sim_refs <- dplyr::bind_rows(Results)

# Stop the cluster:
snow::stopCluster(cl)

# Save the references data frame to a file
save(sim_refs,
     file = here::here("self-sim", "sim_refs.Rdata"))
write.csv(sim_refs,
          file = here::here("self-sim", "sim_refs.csv"),
          row.names = FALSE)
