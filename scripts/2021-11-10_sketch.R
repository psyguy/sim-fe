library(tidyverse)
library(plyr)
library(MplusAutomation)
library(here)
library(dplyr)

fit.files <- list.files("self-sim/experiment-b/", pattern = "fit_uSeed")
fit.objects <- fit.files %>%
  here("self-sim/experiment-b/",.) %>%
  plyr::llply(function(x) {
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

    res <- unstd %>%
      rbind(stdyx) %>%
      mutate(fit.ElapsedTime = (m[["fit.EndTime"]] - m[["fit.StartTime"]]) %>%
      as.numeric(),
      fit.File = gsub(".*/", "", m$fit.File))

    return(res)

      # cbind(fit.File = gsub(".*/", "", x),
      #       unstd,
      #       stdyx,
      #       elapsed.time)
    },
    .progress = "text")


f.d <- fit.objects[-323] %>%
  do.call(rbind,.) %>%
  inner_join(d,.,by = "fit.File")

ldply(fit.objects, dim)

fit.objects[322]

d.sim <- readRDS(here::here("self-sim/sim_files/",
                            list.files("self-sim/sim_files/",
                                       pattern = "13293")))

d.sim %>%
  pivot_wider(subject, t, values_from = x) %>%
  select(-subject) %>%
  t %>%
  frequencyHeatmap()

# d.sim$subject <- d.sim$subject %>% as.factor()
#
# detach(package:plyr)

d.sim %>%
  group_by(subject) %>%
  summarize(m = mean(x)) %>%
  pull(m) %>%
  hist()



