library(ggthemes)
library(viridis)


my.standardization = "stdyx"
# my.standardization = "unstd"
my.param.name = "X.WITH.PHI"
my.param.name = "PHI|X.ON.X&1"

d <- harv %>%
  na.omit() %>%
  # filter(Model == "BinAR",
  #        l2.dist == "Chi2") %>%
  # select(uSeed,
  #        standardization,
  #        param.name,
  #        est,
  #        lower_2.5ci,
  #        upper_2.5ci,
  #        sig) %>%
  filter(
    standardization == my.standardization,
    param.name == my.param.name
  ) %>%
  mutate(l2.X.Model = as.factor(paste(l2.dist, Model))) %>%
  group_by(l2.X.Model) %>%
  arrange(est) %>%
  mutate(ord = order(est)) %>%
  mutate(sign.X.sig = as.factor(sign(est)*sig)) %>%
  mutate(mean.est = mean(est),
         n.datasets = n()) %>%
  group_by(sign.X.sig,
           .add = TRUE) %>%
  mutate(percent = round(100*n()/n.datasets,1))


# d.means <- d %>%
#   group_by(l2.X.Model) %>%
#   summarise(
#     mean.est = mean(est)
#   )



d %>%
  # filter(est < quantile(est, 0.99)) %>%
  group_by(l2.X.Model) %>%
  ggplot() +
  aes(x = ord, color = sign.X.sig, group = l2.X.Model) +
  geom_pointrange(aes(x = ord,
                      y = est,
                      ymin = lower_2.5ci,
                      ymax = upper_2.5ci,
                      fill = sign.X.sig),
                  shape = 19,
                  size = .05,
                  alpha = 0.6,
                  fatten = 25
  ) +
  theme_calc() +
  # scale_color_fivethirtyeight() +
  scale_color_viridis_d(begin = 0.4,
                       end = 1) +
  # geom_smooth(aes(x = ord, y = est),
  #             method = "lm",
  #             formula = y ~ 1, se = FALSE) +
  # geom_hline(
  #   yintercept = mean(d$est),
  #   linetype = "dashed") +
  # stat_summary(aes(y = est), fun = mean, geom="line")
  # # stat_summary(aes(y= est, group=sign.X.sig), fun=mean, geom="line", colour="green")
  geom_hline(aes(yintercept = mean.est),
             linetype = "dashed",
             size = 0.3) +
  geom_text(aes(x = 500,
                y = 0.1 + mean.est,
                # group = sign.X.sig,
                label = paste0("Mean: ",
                              round(mean.est, 4)
                              )
                ),
            color = "black",
            size = 3
            ) +
  # scale_y_continuous(breaks = seq(-1, 1, by = 0.25),
  #                    minor_breaks = seq(-1, 1, by = 0.1)) +
  # geom_hline(d.means, aes(y = mean.est)) +
  ggtitle(paste(my.standardization, my.param.name)) +
  facet_grid(rows = vars(l2.dist),
             cols = vars(Model))


d %>%
  # filter(est < 20) %>%
  ggplot() +
  aes(fill = sign.X.sig,
      group = sign.X.sig) +
  geom_histogram(aes(x = est),
                 bins = 30) +
  theme_minimal() +
  geom_vline(aes(
    xintercept = mean.est),
    linetype = "dashed") +
  theme_calc() +
  # scale_fill_calc() +
  scale_fill_viridis_d(begin = 0.4,
                       end = 1) +
  # geom_label(aes(x = mean(est),
  #                y = 5,
  #                group = sign.X.sig,
  #                label = round(mean(est),4)
  # )
  # ) +
  facet_grid(rows = vars(l2.dist),
             cols = vars(Model))
