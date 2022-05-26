##################################################################
## Here we collect and clean the harvested parameter estimates. ##
## The 2022-03-09 varsion includes all of the 144000 fit files. ##
##################################################################

library(tidyverse)
library(ggthemes)
library(viridis)
library(ggthemes)
library(hrbrthemes)
# rm(list = ls())

harv_resid.fixed <- readRDS("harvests/fit-harvest_N-100_T-100_BinAR.ChiAR.DAR_resid-fixed.rds") %>%
  mutate(type = "resid.fixed",
         .after = uSeed)
harv_resid.random <- readRDS("harvests/fit-harvest_N-100_T-100_BinAR.ChiAR.DAR_resid-random.rds") %>%
  mutate(type = "resid.random",
         .after = uSeed)
harv_podar <- readRDS("harvests/fit-harvest_N-100_T-100_BinAR.ChiAR.DAR.PoDAR_resid-fixed.random.rds")

harv_74000 <- readRDS("harvests/fit-harvest_2022-01-24_74000-files.rds")

harv_64k <- readRDS("harvests/fit-harvest_64k.rds")

harv0 <- rbind(harv_resid.fixed,
               harv_resid.random,
               harv_podar %>% select(-analysis.time),
               harv_74000,
               harv_64k)

harv <- harv %>%
  na.omit() %>%
  mutate(analysis.time = if_else(fit.ElapsedTime<5,
                                 fit.ElapsedTime*60,
                                 fit.ElapsedTime),
         .after = fit.ElapsedTime
  )

harv_1 <- readRDS(here::here("harvests",
                             "fit-harvest_N-100_T-100_BinAR.ChiAR.DAR.PoDAR_resid-fixed.random.rds")
)
harv_2 <- readRDS(here::here("harvests",
                             "fit-harvest_N-25_T-25_BinAR.ChiAR.PoDAR.NAR_resid-fixed.random.rds")
)


harv <- rbind(harv_resid.fixed,
              harv_resid.random,
              harv_podar %>% select(-analysis.time),
              harv_1 %>% select(-analysis.time),
              harv_2,
              harv_74000,
              harv_64k) %>%
  filter(Model != "DAR")

harv <- harv %>%
  na.omit() %>%
  mutate(analysis.time = if_else(fit.ElapsedTime<2,
                                 fit.ElapsedTime*60,
                                 fit.ElapsedTime),
         .after = fit.ElapsedTime
  )


table(harv[colnames(harv)[c(2:5,7,17)]] %>% filter(param.name == "X.WITH.PHI"))

h_important <- harv[colnames(harv)[c(1:5,7,9,16,17)]] %>%
  distinct() %>%
  select(-uSeed) %>%
  filter(Model != "DAR",
         standardization == "unstd",
         param.name == "X.WITH.PHI",
         BetweenWithin == "Between")
table(h_important)

h_unique <- unique(h_important)
table(h_unique)

# h_unique <- unique(harv[colnames(harv)[2:6]])

harv_unique <- harv %>%
  filter(phi == 0.4,
         Model != "DAR") %>%
  distinct()

harv_unique[colnames(harv_unique)[c(2:5,7,9,16,17)]] %>%
  # distinct() %>%
  # select(-uSeed) %>%
  filter(
    standardization == "unstd",
    param.name == "X.WITH.PHI",
    BetweenWithin == "Between") %>%
  table()

#### The final, cleanest harv dataframe is harv_unique.
#### Saving it for further use

saveRDS(harv_unique, "harvests/2022-03-09_clean-144k")

