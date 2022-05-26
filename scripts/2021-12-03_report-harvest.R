library(dplyr)
library(here)
# rm(list = ls())

# Read files and extract X.with.PHI ---------------------------------------

harvest.dir <- "simulation-files/fit-files/"
l.files <- list.files(path = here(harvest.dir),
                      pattern = ".rds")


fit.files <- l.files %>%
  here(harvest.dir, .)

Sys.time()
fit.objects <- fit.files %>%
  plyr::llply(function(x){

    print(x)
    m <- readRDS(x)

    est.par <- m[["fit.Dataset"]][["results"]][["parameters"]]

    if(length(est.par) == 0) return(NA)

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

  } #,
  # .progress = "text"
  )

Sys.time()
harvest.dataset <- fit.objects %>%
  do.call(rbind,.)
Sys.time()

saveRDS(harvest.dataset, "harvest.dataset_1163.rds")






