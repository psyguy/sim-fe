#############################################################
## Here we report the harvested parameter estimates,
## calculate Bias/(R)MSE/etc., and plot them side-by-side
#############################################################

library(tidyverse)
library(ggthemes)
library(viridis)
library(ggthemes)
library(hrbrthemes)
library(cowplot)
library(patchwork)
# shelf(tikzDevice)
library(latex2exp)
# library(tictoc)

rm(list = ls())

# Change font in plots ----------------------------------------------------

# https://stackoverflow.com/a/51906008/2275986

library(showtext)

# Check the current search path for fonts
font_paths()
#> [1] "C:\\Windows\\Fonts"

# List available font files in the search path
f <- font_files()

# syntax: font_add(family = "<family_name>", regular = "/path/to/font/file")
font_add("CMU Classical Serif", "cmunci.ttf")
font_add("CMU Serif Upright Italic", "cmunui.ttf")
font_add("CMU Serif", "cmunrm.ttf")

font_add("Merriweather Regular", "Merriweather-Regular.ttf")
font_add("Merriweather Light", "Merriweather Light.ttf")

font_families()

## automatically use showtext for new devices
showtext_auto()


# Reading and summarizing harvested data ----------------------------------


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

  ## To calculate Bias, etc.
  true.val <- 0
  if(my.param.name == "Means.PHI") true.val <- 0.4
  if(my.param.name == "PHI|X.ON.X&1") true.val <- 0.4
  if(my.param.name == "X.WITH.PHI") true.val <- 0
  if(my.param.name == "Variances.PHI") true.val <- 0
  if(my.param.name == "PHI.WITH.LOGV") true.val <- 0

  d <- d %>%
    mutate(l2.X.Model = as.factor(paste(l2.dist, Model))) %>%
    group_by(type,
             l2.X.Model,
             N,
             T) %>%
    arrange(est) %>%
    mutate(ord = order(est)) %>%
    mutate(sign.X.sig = as.factor(sign(est)*sig)) %>%
    mutate(mean.est = mean(est),
           mean.abs.est = mean(abs(est)),
           MAD = mean(abs(est - true.val)),
           MSE = mean((est - true.val)^2),
           RMSE = sqrt(mean((est - true.val)^2)),
           Bias = mean(est) - true.val,
           variance = var(est),
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
             mean.abs.est,
             MAD,
             MSE,
             Bias,
             variance,
             nonconverged.percent
    ) %>%
    group_by(l2.dist,
             l2.X.Model) %>%
    group_by(type) %>%
    pivot_wider(names_from = sign.X.sig,
                values_from = percent) %>%
    ungroup() %>%
    select(-l2.X.Model) %>%
    relocate(nonconverged.percent,
             # mean_analysis.time,
             .after = last_col())



  plot_scatter <- d %>%
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
    scale_color_viridis_d(begin = 0.4,
                          end = 1) +
    geom_hline(aes(yintercept = mean.est),
               linetype = "dashed",
               size = 0.3) +
    geom_text(aes(x = 500,
                  y = 0.1 + mean.est,
                  label = paste0("Mean: ",
                                 round(mean.est, 4)
                  )
    ),
    color = "black",
    size = 3
    ) +
    ggtitle(paste(my.standardization, my.param.name)) +
    facet_grid(rows = vars(l2.dist),
               cols = vars(Model))


  plot_histogram <-
    d %>%
      filter(Model == "PoDAR",
             l2.dist == "Gaussian") %>%
      # group_by(N, T) %>%
      # arrange() %>%
      mutate(title = paste0("N: ", N,
                            ", T: ", T) %>%
               factor(levels = c("N: 100, T: 25",
                                 "N: 100, T: 50",
                                 "N: 100, T: 100",
                                 "N: 50, T: 25",
                                 "N: 50, T: 50",
                                 "N: 50, T: 100",
                                 "N: 25, T: 25",
                                 "N: 25, T: 50",
                                 "N: 25, T: 100"

               ))) %>%
    ggplot() +
    aes(fill = sign.X.sig,
        group = sign.X.sig) +
      # geom_rect(aes(xmin = min(0, mean.est),
      #               xmax = max(0, mean.est),
      #               ymin = -Inf, ymax = Inf),
      #           fill = "gray80") +
    geom_histogram(aes(x = est),
                   bins = 40) +
    # theme_light() +
      theme_minimal() +
      geom_vline(aes(xintercept = 0),
                 linetype = "dashed") +
      # geom_vline(aes(xintercept = Bias),
      #            linetype = "dashed") +
    # theme_calc() +
    scale_fill_viridis_d(#option = "plasma",
                         begin = 0,
                         end = 1
                         ) +
      facet_wrap(vars(title))
    # facet_grid(rows = vars(N),
    #            cols = vars(T))

  plot_histogram.time <- d %>%
    ggplot() +
    aes(fill = sign.X.sig,
        group = sign.X.sig) +
    geom_histogram(aes(x = analysis.time),
                   bins = 30) +
    theme_minimal() +
    theme_calc() +
    scale_fill_viridis_d(begin = 0.4,
                         end = 1) +
    ggtitle(paste(my.standardization, my.param.name)) +
    facet_grid(rows = vars(l2.dist),
               cols = vars(Model))



  if(what == "scatter") return(plot_scatter)
  if(what == "histogram") return(plot_histogram)
  if(what == "time") return(plot_histogram.time)
  if(what == "summary") return(d_summaries)

}


make.summary_all <- function(harv = harv,
                             my.standardization = "stdyx",
                             my.param.name = "X.WITH.PHI"){

  summary_resid.fixed <- report_harvest(harv = harv %>%
                                          filter(type == "resid.fixed"),
                                        my.standardization = my.standardization,
                                        my.param.name = my.param.name,
                                        what = "summary") %>%
    mutate(type = "resid.fixed",
           .before = l2.dist)

  summary_resid.random <- report_harvest(harv = harv %>%
                                           filter(type == "resid.random"),
                                         my.standardization = my.standardization,
                                         my.param.name = my.param.name,
                                         what = "summary") %>%
    mutate(type = "resid.random",
           .before = l2.dist)


  summary_all <- rbind(summary_resid.fixed,
                       summary_resid.random)

  d <- summary_all %>%
    mutate(
      Model = factor(Model,
                     levels = c("NAR",
                                "Chi2AR",
                                "BinAR",
                                "PoDAR")
      )
    )

  return(d)

}

# d <- make.summary_all(harv)


#  summary plots ----------------------------------------------------------


plot_Model.x.Resid <- function(harv,
                               Means.dist = c("Gaussian", "Chi2"),
                               which.measure = "Bias",
                               standardization = "stdyx",
                               param.name = "X.WITH.PHI",
                               line.width = 2,
                               which.percent = NULL){

  d <- make.summary_all(harv,
                        standardization,
                        param.name)

  if(!is.null(which.percent) | which.measure == "Type-1 Error"){
    dd <- d %>%
      # filter(Correlation == which.percent) %>%
      select(type:T, contains(which.percent)) %>%
      na.omit()
    dd[ncol(dd)] <- (100 - dd[ncol(dd)])#/100
    which.measure <- "% Type-1 Error"}

  if(is.null(which.percent)){
    dd <- d %>%
      # filter(Correlation == which.percent) %>%
      select(type:T, which.measure)}

  colnames(dd)[ncol(dd)] <- "value"
  y.range <- dd$value %>% range()

  dd <- dd %>%
    filter(l2.dist == Means.dist)
  dd$l2.dist <- paste0(dd$l2.dist, "-distributed means")
  dd$type[dd$type == "resid.fixed"] <- "Fixed Residuals"
  dd$type[dd$type == "resid.random"] <- "Random Residuals"

  dd$N <- as.factor(dd$N)
  dd$T <- as.factor(dd$T)

  title <- ifelse(Means.dist == "Gaussian",
                  TeX("Gaussian-distributed means $\\phantom{\\chi^2}$"),
                  TeX("$\\chi^2$-distributed means")
                  )

  ddd <- dd %>%
    distinct()

  output.plot <- ddd %>%
    ggplot()+
    aes(x = T, y = value,
        group = N,
        color = N) +
    geom_line(size = line.width) + #aes(group = Correlation)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    # scale_color_viridis(discrete = TRUE) +
    facet_grid(rows = vars(Model),
               cols = vars(type)) +
    ggtitle(dd$l2.dist) +
    theme_light() +
    scale_y_continuous(#breaks = c(40, 60, 80, 90, 100),
                       limits = y.range) +
    scale_color_tableau("Green-Orange-Teal") +
    ylab(which.measure) +
    ggtitle(title) +
    theme(
          plot.title = element_text(size = 40,
                                    family = "CMU Serif"),
          legend.position = "bottom",
          text = element_text(size = 30,
                              family = "Merriweather Regular")
          )
  if(!is.null(which.percent) | which.measure == "Type-1 Error"){
    output.plot <- output.plot +
      scale_y_continuous(breaks = c(1, 5, 10, 20, 40, 60, 80, 90, 100),
        limits = y.range)
  }

  return(output.plot)

}

# modified_log10_trans <- scales::trans_new(
#   "modified_log10_trans",
#   transform = function(x) {
#     if(x>0) o <- log10(x)
#     if(x<0) o <- -log10(-x)},
#   inverse = function(x) {sinh(x)}
# )

asinh_trans <- scales::trans_new(
  "inverse_hyperbolic_sine",
  transform = function(x) {asinh(x)},
  inverse = function(x) {sinh(x)}
)


# which.percent <- "Zero"

# plot.all <- function(d = d,
#                      measure = "MSE"){


# my.standardization = "stdyx"
# my.param.name <- "PHI|X.ON.X&1"
# measure <- "Bias"


my.standardization <- "unstd"
my.param.name <- "X.WITH.PHI"
title <- "Level-2 covariance between $\\phi$ and mean"

my.standardization <- "stdyx"
my.param.name <- "X.WITH.PHI"
title <- "Level-2 correlation between $\\phi$ and mean"

my.standardization <- "unstd"
my.param.name <- "Means.PHI"
title <- "Unstandardized means of $\\phi$"

my.standardization <- "stdyx"
my.param.name <- "Means.PHI"
title <- "Standardized means of $\\phi$"

my.standardization <- "stdyx"
my.param.name <- "PHI|X.ON.X&1"
title <- "Within standardized $\\phi$ averaged over cluster"




# harv %>% filter(param.name == my.param.name,
#                     standardization == my.standardization) %>%
#   pull(BetweenWithin) %>%
#   unique()

p.upper.left <- plot_Model.x.Resid(harv,
                                   "Gaussian",
                                   "Bias",
                                   my.standardization,
                                   my.param.name)  + xlab(NULL)
p.upper.right <- plot_Model.x.Resid(harv,
                                    "Chi2",
                                    "Bias",
                                    my.standardization,
                                    my.param.name) + xlab(NULL) + ylab(NULL)

p.lower.left <- plot_Model.x.Resid(harv,
                                   "Gaussian",
                                   "variance",
                                   my.standardization,
                                   my.param.name)  + ggtitle(NULL)
p.lower.right <- plot_Model.x.Resid(harv,
                                    "Chi2",
                                    "variance",
                                    my.standardization,
                                    my.param.name)  + ggtitle(NULL) + ylab(NULL)


p.patchwork <- (p.upper.left | p.upper.right) / (p.lower.left | p.lower.right)


# For percentages:

my.standardization <- "stdyx"
my.param.name <- "X.WITH.PHI"
title <- "Error in the correlation and covariance between $\\phi$ and mean"


p.upper.left <- plot_Model.x.Resid(harv,
                                   "Gaussian",
                                   "Type-1 Error",
                                   "stdyx",
                                   my.param.name,
                                   which.percent = "Zero")  + xlab(NULL)
p.upper.right <- plot_Model.x.Resid(harv,
                                    "Chi2",
                                    "Type-1 Error",
                                    "stdyx",
                                    my.param.name,
                                    which.percent = "Zero") + xlab(NULL) + ylab(NULL)

p.lower.left <- plot_Model.x.Resid(harv,
                                   "Gaussian",
                                   "Type-1 Error",
                                   "unstd",
                                   my.param.name,
                                   which.percent = "Zero")  + ggtitle(NULL)
p.lower.right <- plot_Model.x.Resid(harv,
                                    "Chi2",
                                    "Type-1 Error",
                                    "unstd",
                                    my.param.name,
                                    which.percent = "Zero")  + ggtitle(NULL) + ylab(NULL)


p.patchwork <- (p.upper.left | p.upper.right) / (p.lower.left | p.lower.right)

#
# p.percent.left <- plot_Model.x.Resid(harv,
#                                    "Gaussian",
#                                    "Type-1 Error",
#                                    my.standardization,
#                                    my.param.name,
#                                    which.percent = "Zero")  + xlab(NULL)
# p.percent.right <- plot_Model.x.Resid(harv,
#                                     "Chi2",
#                                     "Type-1 Error",
#                                     my.standardization,
#                                     my.param.name,
#                                     which.percent = "Zero") + xlab(NULL) + ylab(NULL)
#
# p.patchwork.percent <- (p.percent.left | p.percent.right)


p.final <- p.patchwork + #.percent +
  plot_layout(guides = "collect") +
   # +
  plot_annotation(title = TeX(title),
                  subtitle = " ",
                  theme = theme(plot.title =
                                  element_text(size = 55,
                                               family = "CMU Serif",
                                                hjust = 0.5))
                  ) &
  geom_point(size = 3.5, alpha = 1) &
  geom_line(size = 0.5, alpha = 0.6) &
  # geom_hline(yintercept = c(1, 5), linetype = "dashed") &
  scale_color_viridis_d(direction = -1) &
  # scale_y_continuous(trans = asinh_trans) &
  # coord_trans(y = "log10") &
  theme(legend.position = "bottom")

# (p.percent.four.panel <-( p.final / p.final.cov) +
#   plot_layout(guides = "collect") &
#   theme(legend.position = "bottom")
# )

ggsave(paste0("bias + variance ",
              gsub("\\|", " ", my.param.name),
              " (",
              my.standardization,
              ").pdf"),
       p.final,
       width = 21,
       height = 30,
       units = "in")

# ggsave(paste0("Error correlation and covariance", ".pdf"),
#        p.final,
#        width = 21,
#        height = 30,
#        units = "in")


# Plotting nested histograms  ---------------------------------------------

prep.harvest <- function(harv,
                         # Means.dist = "Gaussian", #c("Gaussian", "Chi2"),
                         # Model = "BinAR",
                         # resid.type = "resid.fixed",
                         my.standardization = "stdyx",
                         my.param.name = "X.WITH.PHI"){


  d <- harv %>%
    na.omit() %>%
    filter(
      # type == resid.type,
      standardization == my.standardization,
      param.name == my.param.name
    ) %>%
    mutate(l2.X.Model = as.factor(paste(l2.dist, Model))) %>%
    group_by(type,
             l2.X.Model,
             N,
             T) %>%
    arrange(est) %>%
    mutate(ord = order(est)) %>%
    mutate(sign.X.sig = as.factor(sign(est)*sig)) %>%
    mutate(mean.est = mean(est),
           n.datasets = n(),
           nonconverged.percent = round(100*(1000-n())/1000,2)) %>%
    group_by(sign.X.sig,
             .add = TRUE) %>%
    mutate(percent = round(100*n()/n.datasets,1),
           .after = sign.X.sig) %>%
    mutate(title = paste0("N: ", N,
                          ", T: ", T) %>%
             factor(levels = c("N: 100, T: 25",
                               "N: 100, T: 50",
                               "N: 100, T: 100",
                               "N: 50, T: 25",
                               "N: 50, T: 50",
                               "N: 50, T: 100",
                               "N: 25, T: 25",
                               "N: 25, T: 50",
                               "N: 25, T: 100"
             )
             )
    )

  levels(d$sign.X.sig) <- c("Negative", "Zero", "Positive")

  return(d)
}

d <- prep.harvest(harv)

p.hist <- function(d){


  x.range <- d$est %>% range()

  plot_histogram <-
    d %>%
    filter() %>%
    # group_by(N, T) %>%
    # arrange() %>%

    ggplot() +
    aes(fill = sign.X.sig,
        group = sign.X.sig) +
    # geom_rect(aes(xmin = min(0, mean.est),
    #               xmax = max(0, mean.est),
    #               ymin = -Inf, ymax = Inf),
    #           fill = "gray80") +
    geom_histogram(aes(x = est),
                   bins = 40) +
    theme_calc() +
    # theme_light() +
    # theme_minimal() +
    geom_vline(aes(xintercept = 0),
               linetype = "dashed") +
    # geom_vline(aes(xintercept = Bias),
    #            linetype = "dashed") +
    # theme_calc() +
    scale_fill_viridis_d(option = "viridis",
                         begin = 0.2,
                         end = 0.8
    ) +
    scale_x_continuous(limits = x.range) +
    facet_wrap(vars(title))

  return(plot_histogram)

}

my.standardization = "stdyx"
my.param.name = "X.WITH.PHI"


plot_columns <- function(d,
                         Means.dist = "Gaussian", #c("Gaussian", "Chi2"),
                         resid.type = "resid.fixed"){

  dd <- d %>%
    filter(l2.dist == Means.dist,
           type == resid.type)

  p.NAR <- dd %>%
    filter(Model == "NAR") %>%
    p.hist()

  p.Chi2AR <- dd %>%
    filter(Model == "Chi2AR") %>%
    p.hist()

  p.BinAR <- dd %>%
    filter(Model == "BinAR") %>%
    p.hist()

  p.PoDAR <- dd %>%
    filter(Model == "PoDAR") %>%
    p.hist()


  patched <- p.NAR / p.Chi2AR / p.BinAR / p.PoDAR

  means <- paste0(Means.dist, "-distributed means")

  resids <- ifelse(resid.type == "resid.fixed",
                   "Fixed Residuals",
                   "Random Residuals")

  patched.titled <- patched +
    plot_layout(guides = "collect") +
    plot_annotation(title = paste(means, "with", resids),
                    subtitle = " ",
                    theme = theme(plot.title =
                                    element_text(size = 25,
                                    family = "Merriweather Regular",
                                    hjust = 0.5))) &
    theme(legend.position = "bottom") &
    ylab(NULL)


  return(patched.titled)

}

tic()

c.Gaussian.fixed <- plot_columns(d,
                                 "Gaussian",
                                 "resid.fixed")

toc()

c.Gaussian.random <- plot_columns(d,
                                  "Gaussian",
                                  "resid.random")

c.Chi2.fixed <- plot_columns(d,
                             "Chi2",
                             "resid.fixed")

c.Chi2.random <- plot_columns(d,
                              "Chi2",
                              "resid.random")

patched.Gaussian <- c.Gaussian.fixed + c.Gaussian.random +
  plot_layout(guides = "collect") +
  plot_annotation(title = TeX("Gaussian-distributed means $\\phantom{\\chi^2}$"),
                  subtitle = " ",
                  theme = theme(plot.title =
                                  element_text(size = 30,
                                               family = "CMU Serif",
                                               hjust = 0.5))
  ) &
  theme(legend.position = "bottom")


patched.Chi2 <- c.Chi2.fixed + c.Chi2.random +
  plot_layout(ncol = 2) +
  plot_annotation(title = TeX("$\\chi^2$-distributed means"),
                  subtitle = " ",
                  theme = theme(plot.title =
                                  element_text(size = 30,
                                               family = "CMU Serif",
                                               hjust = 0.5))
  ) &
  theme(legend.position = "bottom")




patched.four.columns <- patched.Gaussian + patched.Chi2


ggsave(paste0("meeeh5.pdf"),
       patched.four.columns,
       width = 30,
       height = 21,
       units = "in")



  plot_histogram + ggtitle()

plot_histogram / plot_histogram / plot_histogram

