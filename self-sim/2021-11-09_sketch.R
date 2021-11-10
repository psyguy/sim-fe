library(tidyverse)
library(plyr)
library(MplusAutomation)
library(here)
library(dplyr)

fit.files <- list.files("self-sim/fit_files/", pattern = ".rds")
fit.objects <- fit.files %>%
  here("self-sim/fit_files",.) %>%
  llply(function(x) {
    m <- readRDS(x)
    m[["results"]][["parameters"]][["stdyx.standardized"]] %>%
      cbind(fit.File = gsub(".*/", "", x),
            .)
    },
    .progress = "tk")


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



