#####################################################
## Here we report the harvested parameter estimates.
#####################################################

library(tidyverse)
library(ggthemes)
library(viridis)
# rm(list = ls())

harv_resid.fixed <- readRDS("fit-harvest_N-100_T-100_BinAR.ChiAR.DAR_resid-fixed.rds")
harv_resid.random <- readRDS("fit-harvest_N-100_T-100_BinAR.ChiAR.DAR_resid-random.rds")

harv_resid.fixed <- harv_resid.fixed %>%
  na.omit() %>%
  mutate(analysis.time = if_else(fit.ElapsedTime<5,
                                fit.ElapsedTime*60,
                                fit.ElapsedTime),
         .after = fit.ElapsedTime
         )
harv_resid.random <- harv_resid.random %>%
  na.omit() %>%
  mutate(analysis.time = if_else(fit.ElapsedTime<5,
                                 fit.ElapsedTime*60,
                                 fit.ElapsedTime),
         .after = fit.ElapsedTime
  )

my.standardization = "stdyx"
# my.standardization = "unstd"
my.param.name = "X.WITH.PHI"
# my.param.name = "PHI|X.ON.X&1"

report_harvest <- function(harv = harv,
                         my.standardization = c("stdyx", "unstnd"),
                         my.param.name = c("X.WITH.PHI",
                                        "X.WITH.LOGV",
                                        "PHI.WITH.LOGV",
                                        "Means.X",
                                        "Means.PHI",
                                        "Means.LOGV",
                                        "Variances.X",
                                        "Variances.PHI",
                                        "Variances.LOGV",
                                        "PHI|X.ON.X&1",
                                        "LOGV.|.X"),
                         what = c("scatter",
                                  "histogram",
                                  "time",
                                  "summary")){


  d <- harv %>%
    na.omit() %>%
    filter(
      standardization == my.standardization,
      param.name == my.param.name
    ) %>%
    mutate(l2.X.Model = as.factor(paste(l2.dist, Model))) %>%
    group_by(l2.X.Model) %>%
    arrange(est) %>%
    mutate(ord = order(est)) %>%
    mutate(sign.X.sig = as.factor(sign(est)*sig)) %>%
    mutate(mean_analysis.time = mean(analysis.time),
           min_analysis.time = min(analysis.time),
           max_analysis.time = max(analysis.time)) %>%
    mutate(mean.est = mean(est),
           n.datasets = n(),
           nonconverged.percent = round(100*(1000-n())/1000,2)) %>%
    group_by(sign.X.sig,
             .add = TRUE) %>%
    mutate(percent = round(100*n()/n.datasets,1),
           .after = sign.X.sig)

  levels(d$sign.X.sig) <- c("Negative", "Zero", "Positive")


  d_summaries <- d %>%
    group_by(l2.X.Model,
             l2.dist,
             Model,
             # N,
             # T,
             mean.est,
             nonconverged.percent,
             mean_analysis.time,
             sign.X.sig) %>%
    summarize(si.count = n()) %>%
    group_by(l2.dist) %>%
    mutate(percent = round(si.count/(1-0.014)/10,
                           2)) %>%
    select(-si.count) %>%
    group_by(l2.X.Model) %>%
    pivot_wider(names_from = sign.X.sig,
                values_from = percent) %>%
    ungroup() %>%
    select(-l2.X.Model) %>%
    relocate(nonconverged.percent,
             mean_analysis.time,
             .after = last_col())



  plot_scatter <- d %>%
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


  plot_histogram <- d %>%
    # filter(est < 20) %>%
    ggplot() +
    aes(fill = sign.X.sig,
        group = sign.X.sig) +
    geom_histogram(aes(x = est),
                   bins = 30) +
    theme_minimal() +
    geom_vline(aes(xintercept = mean.est),
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

  plot_histogram.time <- d %>%
    # filter(est < 20) %>%
    ggplot() +
    aes(fill = sign.X.sig,
        group = sign.X.sig) +
    geom_histogram(aes(x = analysis.time),
                   bins = 30) +
    theme_minimal() +
    # geom_vline(aes(xintercept = mean.est),
    #            linetype = "dashed") +
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
    ggtitle(paste(my.standardization, my.param.name)) +
    facet_grid(rows = vars(l2.dist),
               cols = vars(Model))


  # d_s <- d[!duplicated(d[20]),]


  if(what == "scatter") return(plot_scatter)
  if(what == "histogram") return(plot_histogram)
  if(what == "time") return(plot_histogram.time)
  if(what == "summary") return(d_summaries)

}

report_harvest(harv = harv_resid.fixed,
               my.standardization = "unstd",
               my.param.name = "X.WITH.PHI",
               what = "scatter")

report_harvest(harv = harv_resid.fixed,
               my.standardization = "stdyx",
               my.param.name = "X.WITH.PHI",
               what = "histogram")

report_harvest(harv = harv_resid.random,
               my.standardization = "stdyx",
               my.param.name = "X.WITH.PHI",
               what = "time")

summary_resid.fixed <- report_harvest(harv = harv_resid.fixed,
                                      my.standardization = "unstd",
                                      my.param.name = "X.WITH.PHI",
                                      what = "summary") %>%
  mutate(type = "resid.fixed",
         .before = l2.dist)

summary_resid.random <- report_harvest(harv = harv_resid.random,
                                      my.standardization = "unstd",
                                      my.param.name = "X.WITH.PHI",
                                      what = "summary") %>%
  mutate(type = "resid.random",
         .before = l2.dist)


summary_all <- rbind(summary_resid.fixed,
                     summary_resid.random)


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
