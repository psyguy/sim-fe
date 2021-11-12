library(tidyverse)
library(plyr)
library(MplusAutomation)
library(here)
library(dplyr)
# detach(package:dplyr)

fit.files <- list.files("self-sim/experiment-b/", pattern = "fit_uSeed-")
fit.objects <- fit.files %>%
  here::here("self-sim/experiment-b/",.) %>%
  llply(function(x) {
    m <- readRDS(x)
    m$fit.Dataset[["results"]][["parameters"]][["stdyx.standardized"]] %>%
      cbind(fit.File = gsub(".*/", "", x),
            .)
    },
    .progress = "tk")

fit_refs <- readRDS(here::here("self-sim/experiment-b", "fit_refs.rds"))


ldply(fit.objects, dim)

fit.objects[118]

f.d <- fit.objects[-118] %>% # excluding the one for which Mplus crashed
  do.call(rbind,.) %>%
  inner_join(fit_refs,.,by = "fit.File")

saveRDS(f.d, "fitted objects 118")
d.sim <- readRDS(here::here("self-sim/",
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



