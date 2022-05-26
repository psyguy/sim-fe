library(tidyverse)
library(ggthemes)
library(here)
library(viridis)

# Experiment results for hyperparameters

res_a <- readRDS(here("self-sim","fitted objects 479.rds"))

res_a <- res_a %>%
  select(l2.dist:uSeed,
         iter:thin,
         paramHeader:BetweenWithin) %>%
  mutate(itXth = iter*thin,
         .after = thin) %>%
  mutate(param.name = paste(paramHeader,
                            param,
                            sep = "."),
         .after = uSeed) %>%
  mutate(RepXparam.name = paste(Rep,
                                param.name,
                                sep = "_")) %>%
  select(-paramHeader:-param)

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
  ggplot() +
  aes(x = iter, y = est, group = Model) +
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
