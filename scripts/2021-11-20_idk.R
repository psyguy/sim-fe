library(tidyverse)
rm(list = ls())

harv <- readRDS("fit-harvest_all.rds")

plot_harvest <- function(harv = harv,
                         standardization = c("stdyx", "unstnd"),
                         param.name = c("X.WITH.PHI",
                                        "X.WITH.LOGV",
                                        "PHI.WITH.LOGV",
                                        "Means.X",
                                        "Means.PHI",
                                        "Means.LOGV",
                                        "Variances.X",
                                        "Variances.PHI",
                                        "Variances.LOGV",
                                        "PHI|X.ON.X&1",
                                        "LOGV.|.X")
){


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
      standardization == standardization,
      param.name == param.name
    ) %>%
    arrange(est) %>%
    mutate(ord = order(est)) %>%
    mutate(l2.X.Model = as.factor(paste(l2.dist, Model))) %>%
    mutate(sign.X.sig = as.factor(sign(est)*sig))

  d.means <- d %>%
    group_by(l2.X.Model) %>%
    summarise(
      mean.est = mean(est)
    )

  d %>%
    filter(est < quantile(est, 0.99)) %>%
    group_by(l2.X.Model) %>%
    ggplot() +
    aes(x = ord, color = sign.X.sig, group = l2.X.Model) +
    geom_pointrange(aes(x = ord,
                        y = est,
                        ymin = lower_2.5ci,
                        ymax = upper_2.5ci,
                        fill = sign.X.sig),
                    shape = 21,
                    size = .1,
                    alpha = 0.3,
                    fatten = 20
    ) +
    theme_minimal() +
    # geom_smooth(aes(x = ord, y = est),
    #             method = "lm",
    #             formula = y ~ 1, se = FALSE) +
    # geom_hline(
    #   yintercept = mean(d$est),
    #   linetype = "dashed") +
    # stat_summary(aes(y = est), fun = mean, geom="line")
    # # stat_summary(aes(y= est, group=sign.X.sig), fun=mean, geom="line", colour="green")
    geom_hline(aes(yintercept = mean(est))) +
    # geom_hline(d.means, aes(y = mean.est)) +
    ggtitle(paste(standardization, param.name)) +
    facet_grid(rows = vars(l2.dist),
               cols = vars(Model))

}

plot_harvest(harv,
             standardization = "stdyx",
             param.name = "X.WITH.PH")


d %>%
  filter(est < 20) %>%
  ggplot() +
  aes(est) +
  geom_histogram() +
  theme_minimal() +
  geom_vline(aes(
    xintercept = mean(est)),
    linetype = "dashed") +
  # geom_label(aes(x = mean(est),
  #                y = 5,
  #                group = sign.X.sig,
  #                label = round(mean(est),4)
  # )
  # ) +
  facet_grid(rows = vars(l2.dist),
             cols = vars(Model))
# stat_summary(aes(y = est), fun = mean, geom="line")
# # stat_summary(aes(y= est, group=sign.X.sig), fun=mean, geom="line", colour="green")
# geom_hline(aes(y = mean(est)))
# geom_hline(d.means, aes(y = mean.est)) +
