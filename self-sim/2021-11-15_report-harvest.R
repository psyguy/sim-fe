library(dplyr)
library(here)
library(doSNOW)
# rm(list = ls())

# listing files to be read ------------------------------------------------

harvest.dir <- "self-sim/experiment-b"
l.files <- list.files(path = here(harvest.dir),
                      pattern = "fit_uSeed-")


fit.files <- l.files %>%
  here(harvest.dir, .)

# function to do the extraction (and error handling) ----------------------

fit_extract <- function(rds.file){

  m <- readRDS(rds.file)
  est.par <- m[["fit.Dataset"]][["results"]][["parameters"]]

  if(length(est.par) == 0) return(NA)
  if(is.null(est.par[["unstandardized"]])) return(NA)
  if(is.null(est.par[["stdyx.standardized"]])) return(NA)

  unstd <- est.par[["unstandardized"]] %>%
    mutate(param.name = paste(paramHeader,
                              param,
                              sep = ".")
    ) %>%
    select(-paramHeader:-param) %>%
    mutate(standardization = "unstd",
           .before = est)
  stdyx <- est.par[["stdyx.standardized"]] %>%
    mutate(param.name = paste(paramHeader,
                              param,
                              sep = ".")
    ) %>%
    select(-paramHeader:-param) %>%
    mutate(standardization = "stdyx",
           .before = est)

  m$fit.Dataset <- NULL

  res <- unstd %>%
    rbind(stdyx) %>%
    mutate(fit.ElapsedTime = (m[["fit.EndTime"]] - m[["fit.StartTime"]]) %>%
             as.numeric(),
           fit.File = gsub(".*/", "", m$fit.File)) %>%
    mutate(uSeed = m$uSeed,
           l2.dist = m$l2.dist,
           Model = m$Model,
           N = m$N,
           phi = m$phi,
           T = m$T,
           Rep = m$Rep,
           .before = standardization
    )

  return(res)

}


# reading in parallel -----------------------------------------------------


Sys.time()
nClust <- 48
t.fit <- system.time({

  cl <- makeCluster(nClust)
  registerDoSNOW(cl)

  pb <- txtProgressBar(min=1, max=length(fit.files), style=3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress=progress)

  t.snow <- snow.time({
    results <- foreach(i = 1:length(fit.files),
                       .packages = "dplyr",
                       .options.snow=opts,
                       .combine='rbind') %dopar% {

              res <- tryCatch({
                fit_extract(fit.files[i])
              }, error=function(e) NULL)


    }
  })
  stopCluster(cl)
})

Sys.time()
t.fit
t.snow
plot(t.snow)


# saving the extracted parameter estimates --------------------------------


saveRDS(results, "fit-harvest_1-to-700.rds")
