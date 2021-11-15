library(tidyverse)
library(ggthemes)
library(here)
library(viridis)


# Harvesting fit parameters -----------------------------------------------

harvest.dir <- "self-sim/experiment-a_hyperparameters/fit_files"
l.files <- list.files(path = here(harvest.dir),
                      pattern = ".rds")


fit.files <- l.files %>%
  here(harvest.dir, .)

Sys.time()
fit.objects <- fit.files %>%
  plyr::llply(function(x){

    m <- readRDS(x)

    est.par <- m[["results"]][["parameters"]]

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
      mutate(fit.File = gsub(".*files/", "", x),
           .before = standardization)

    return(res)

  } ,
  .progress = "text"
  )

Sys.time()
fit_exp.a <- fit.objects %>%
  do.call(rbind,.) %>%
  full_join(fit_refs, ., by = "fit.File")
Sys.time()

saveRDS(fit_exp.a, "fit_exp.a.rds")

# Experiment results for hyperparameters

res_a <- fit_exp.a %>%
  filter(T == 100) %>%
  filter((iter==2000&thin==5) | (iter==5000&thin==20)) %>%
  mutate(iterXthin = paste(iter, thin, sep = "X")) %>%
  select(l2.dist,
         Model,
         Rep,
         uSeed,
         iterXthin,
         # thin,
         param.name,
         standardization:BetweenWithin)

write.csv(res_a, "experiment-a-summary-results.csv")

res_a$thin <- res_a$thin %>% as.factor
res_a$iter <- res_a$iter %>% as.factor

res_a.long <- res_a %>%
  pivot_longer(est:sig)
#
# res_a %>%
#   filter(param.name == PHI)
#   ggplot(aes(x = iter*thin,
#              y = ))



res_a %>%
  # filter(param.name == "X.WITH.PHI") %>%
  # filter(Rep == 2) %>%
  # filter(param.name %in% c("X.WITH.PHI",
  #                          "PHI|X.ON.X&1",
  #                          "Means.PHI")) %>%
  filter(T == 100) %>%
  ggplot() +
  aes(x = iter, y = est, group = RepXparam.name) +
  geom_line() +
  # facet_wrap(vars(param.name))
  geom_jitter(aes(shape = as.factor(thin),
                  color = as.factor(param.name)), size = 3) +
  # scale_color_gradient() +
  # scale_color_viridis_d() +
  theme_minimal() +
  # geom_smooth(method = "loess") +
  facet_grid(vars(l2.dist), vars(Model))
+
  facet_wrap(vars(param.name))



res_b <- readRDS(here("","fitted objects 118.rds"))

res_b <- res_b %>%
  # select(l2.dist:uSeed,
  #        iter:thin,
  #        paramHeader:BetweenWithin) %>%
  mutate(itXth = iter*thin,
         .after = thin) %>%
  mutate(param.name = paste(paramHeader,
                            param,
                            sep = "."),
         .after = uSeed) %>%
  select(-paramHeader:-param)

res_b$thin <- res_b$thin %>% as.factor
res_b$iter <- res_b$iter %>% as.factor

res_b.long <- res_b %>%
  pivot_longer(est:sig)





res_b %>%
  # filter(param.name == "X.WITH.PHI") %>%
  # filter(Rep == 2) %>%
  # filter(param.name %in% c("X.WITH.PHI",
  #                          "PHI|X.ON.X&1",
  #                          "Means.PHI")) %>%
  filter(T == 100) %>%
  filter(param.name == "X.WITH.PHI") %>%
  ggplot() +
  aes(x = est, group = Model) +
  # geom_line() +
  # facet_wrap(vars(param.name))
  geom_histogram() +
    # scale_color_gradient() +
  # scale_color_viridis_d() +
    theme_minimal() +
  # geom_smooth(method = "loess") +
    facet_grid(vars(l2.dist), vars(Model))
+
  facet_wrap(vars(param.name))
