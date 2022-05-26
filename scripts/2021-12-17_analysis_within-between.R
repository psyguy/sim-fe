library(tidyverse)
library(MplusAutomation)
library(ggthemes)
library(viridis)
librarian::shelf(future,doFuture,snow,doSNOW)
source("functions/functions_self-sim-pipeline.R")


sim_refs <- readRDS("sample-datasets/sim-refs_Model-BinAR.ChiAR.DAR.PoDAR_N-100_T-30.100.rds") %>%
  filter(T==100, N==100, Rep %in% c(1:50)) %>%
  filter(Model != "DAR") %>%
  mutate(sim.Path = "sample-datasets")

fit_refs <- make_fit_refs(sim_refs, list(iter = c(2000),
                             thin = c(5),
                             type = c("within.between")),
                          "sample-datasets")



Sys.time()
system.time(
  do_fit_doFuture(fit_refs = fit_refs,
                save.directory = "sample-datasets",
                )
)
Sys.time()

plan("multisession")

fit.files <- paste(fit_refs$fit.Path,
                  fit_refs$fit.File,
                  sep = "/")

system.time(
  harv <- plyr::ldply(fit.files,
                      fit_extract,
                      .parallel = TRUE)
)

harvVar <- harv %>% filter(param.name == "Variances.X",
                           standardization == "unstd")

f <- harvVar %>%
  select(-posterior_sd:-sig) %>%
  pivot_wider(names_from = BetweenWithin,
              values_from = est)

f %>%
  ggplot() +
  aes(x = Within,
      y = Between,
      color = Model
      ) +
  geom_point(aes(shape = l2.dist),
             alpha = 0.6,
             size = 5) +
  ggtitle("Within and between variances of 50 sample datasets") +
  # theme_fivethirtyeight() +
  scale_y_continuous(breaks = seq(0,16, by = 2),
                     minor_breaks = c(1:16)) +
  coord_fixed() +
  theme_calc() +
  # scale_color_ptol()
  scale_color_fivethirtyeight()
  scale_color_viridis_d(begin = 0.4,
                        end = 1)

()
f %>% colnames()
