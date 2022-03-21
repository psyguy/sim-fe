#####################################################
## Here we report the harvested parameter estimates.
#####################################################

library(tidyverse)
library(ggthemes)
library(viridis)
library(ggthemes)
library(hrbrthemes)
library(cowplot)
# rm(list = ls())

harv <- readRDS("harvests/2022-03-09_clean-144k")

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
    )

  d <- d %>%
    mutate(l2.X.Model = as.factor(paste(l2.dist, Model))) %>%
    group_by(type,
             l2.X.Model,
             N,
             T) %>%
    arrange(est) %>%
    mutate(ord = order(est)) %>%
    mutate(sign.X.sig = as.factor(sign(est)*sig)) %>%
    # mutate(mean_analysis.time = mean(analysis.time),
    #        min_analysis.time = min(analysis.time),
    #        max_analysis.time = max(analysis.time)) %>%
    mutate(mean.est = mean(abs(est)),
           n.datasets = n(),
           nonconverged.percent = round(100*(1000-n())/1000,2)) %>%
    group_by(sign.X.sig,
             .add = TRUE) %>%
    mutate(percent = round(100*n()/n.datasets,1),
           .after = sign.X.sig)

  levels(d$sign.X.sig) <- c("Negative", "Zero", "Positive")


  d_summaries <- d %>%
    group_by(type,
             l2.X.Model,
             l2.dist,
             Model,
             N,
             T,
             mean.est,
             nonconverged.percent,
             # mean_analysis.time,
             sign.X.sig) %>%
    summarize(si.count = n()) %>%
    group_by(l2.dist,
             l2.X.Model) %>%
    mutate(percent = round(si.count/10,
                           2)) %>%
    select(-si.count) %>%
    group_by(type) %>%
    pivot_wider(names_from = sign.X.sig,
                values_from = percent) %>%
    ungroup() %>%
    select(-l2.X.Model) %>%
    relocate(nonconverged.percent,
             # mean_analysis.time,
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

report_harvest(harv = harv %>% filter(type == "resid.random"),#_resid.fixed,
               my.standardization = "stdyx",
               my.param.name = "X.WITH.PHI",
               what = "scatter")

report_harvest(harv = harv %>% filter(type == "resid.random"),
               my.standardization = "stdyx",
               my.param.name = "X.WITH.PHI",
               what = "histogram")

report_harvest(harv = harv %>% filter(type == "resid.random"),
               my.standardization = "stdyx",
               my.param.name = "X.WITH.PHI",
               what = "time")

summary_resid.fixed <- report_harvest(harv = harv %>%
                                        filter(type == "resid.fixed"),
                                      my.standardization = "stdyx",
                                      my.param.name = "X.WITH.PHI",
                                      what = "summary") %>%
  mutate(type = "resid.fixed",
         .before = l2.dist)

summary_resid.random <- report_harvest(harv = harv %>%
                                         filter(type == "resid.random"),
                                       my.standardization = "stdyx",
                                       my.param.name = "X.WITH.PHI",
                                       what = "summary") %>%
  mutate(type = "resid.random",
         .before = l2.dist)


summary_all <- rbind(summary_resid.fixed,
                     summary_resid.random)


d <- summary_all %>%
  pivot_longer(Negative:Positive,
               names_to = "Correlation",
               values_to = "Percentage") %>%
  mutate(Correlation = factor(Correlation,
                              levels = c("Negative", "Zero", "Positive"),
  ),
  Model = factor(Model,
                 levels = c("NAR",
                            "Chi2AR",
                            "BinAR",
                            "PoDAR")
                 )
  )

# d$l2.dist <- paste0(d$l2.dist, "-distributed means")
# d$type[d$type == "resid.fixed"] <- "Fixed Residuals"
# d$type[d$type == "resid.random"] <- "Random Residuals"

d$N <- as.factor(d$N)
d$T <- as.factor(d$T)

# d[, 1:5] <- d[, 1:5] %>% apply(2, as.factor)


d %>%
  # pull(Percentage)
  ggplot() +
  aes(fill = Correlation,
      y = Percentage,
      # group = Model,
      x = Model) +
  geom_bar(#position = "fill",
    position = "dodge",
    stat = "identity") +
  scale_fill_viridis_d(begin = 0.4,
                       end = 1)+
  theme_calc(base_size = 20) +
  scale_y_continuous(breaks = c(5, 10, 20,
                                90, 95, 100)) +
  labs(title = "Percentage of significant correlations between phi and mean",
       subtitle = "N = 100, T = 100") +
  facet_grid(cols = vars(type),
             rows = vars(l2.dist))

#  summary plots ----------------------------------------------------------


plot_Model.x.Resid <- function(d,
                               Means.dist = c("Gaussian", "Chi2"),
                               which.percent = "Zero",
                               line.width = 1){

  dd <- d %>%
    filter(Correlation == which.percent)
  y.range <- dd$mean.est %>% range()
  dd <- dd %>%
    filter(l2.dist == Means.dist)
  dd$l2.dist <- paste0(dd$l2.dist, "-distributed means")
  dd$type[dd$type == "resid.fixed"] <- "Fixed Residuals"
  dd$type[dd$type == "resid.random"] <- "Random Residuals"

  dd$N <- as.factor(dd$N)
  dd$T <- as.factor(dd$T)

 dd %>%
  ggplot()+
  aes(x = T, y = mean.est,
      group = N,
      color = N) +
  geom_line(size = line.width) + #aes(group = Correlation)) +
  # scale_color_viridis(discrete = TRUE) +
  facet_grid(rows = vars(Model),
             cols = vars(type)) +
  ggtitle(dd$l2.dist) +
  theme_light() +
   # scale_y_continuous(#breaks = c(40, 60, 80, 90, 100),
   #                    limits = y.range,
   #                    minor_breaks = c(5, 10, 90, 95)) +
   scale_color_tableau("Green-Orange-Teal") +
   theme(legend.position="none") +
   # tableau_color_pal('tableau10medium') +
  ylab(paste("Percentage of", which.percent))

}

which.percent <- "Zero"

p.left <- plot_Model.x.Resid(d, "Gaussian", which.percent = which.percent)
p.right <- plot_Model.x.Resid(d, "Chi2", which.percent = which.percent) + ylab(NULL)
  # theme(
  #       axis.title = element_blank(),
  #       axis.text.y = element_blank())

legend_b <- get_legend(
  p.left +
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

p.row <- plot_grid(p.left,
          p.right,
          ncol = 2)

plot_grid(p.row,
          legend_b,
          ncol = 1,
          rel_heights = c(1, 0.05))

