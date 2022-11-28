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
library(latex2exp)
library(ggpubr)
library(RColorBrewer)

library(tikzDevice)

# rm(list = ls())

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

d.abridged <- harv %>%
  na.omit() %>%
  mutate(std_parname = paste(standardization, param.name)) %>%
  filter(std_parname %in% c("stdyx X.WITH.PHI",
                            "unstd X.WITH.PHI",
                            "unstd Means.PHI")) %>%
  select(-standardization) %>%
  mutate(
    parameter = case_when(
      std_parname  == "stdyx X.WITH.PHI" ~ "Correlation",
      std_parname  == "unstd X.WITH.PHI" ~ "Covariance",
      std_parname  == "unstd Means.PHI" ~ "Fixed.Phi",
    ),
    .after = Rep
  ) %>%
  select(uSeed:sig) %>%
  mutate(
    true.value = case_when(
      parameter == "Correlation" ~ 0,
      parameter == "Covariance" ~ 0,
      parameter == "Fixed.Phi" ~ 0.4
    ),
    .after = sig
  )

d.with.performance <- d.abridged %>%

  mutate(l2.X.Model = as.factor(paste(l2.dist, Model)),
         .before = N) %>%
  mutate(
    sign.X.sig = case_when(
      upper_2.5ci < true.value ~ "Negative",
      lower_2.5ci > true.value ~ "Positive",
      TRUE ~ "Zero"
    )
  ) %>%
  group_by(type,
           l2.X.Model,
           N,
           T,
           parameter) %>%
  mutate(
    mean.est = mean(est),
    mean.abs.est = mean(abs(est)),
    MAE = mean(abs(est - true.value)),
    MSE = mean((est - true.value) ^ 2),
    RMSE = sqrt(mean((est - true.value) ^ 2)),
    Bias = mean(est) - true.value,
    Variance = var(est),
    n.converged.datasets = n(),
    nonconverged.percent = round(100 * (1000 - n()) / 1000, 2)
  ) %>%
  group_by(sign.X.sig,
           .add = TRUE) %>%
  mutate(percent = round(100 * n() / n.converged.datasets, 1),
         .after = sign.X.sig) %>%
  relocate(sign.X.sig:nonconverged.percent,
           .after = parameter) %>%
  relocate(est,
           .after = percent)

d_summary <- d.with.performance %>%
  select(type:nonconverged.percent) %>%
  select(-Rep, -est) %>%
  # filter(sign.X.sig == "Zero") %>%
  distinct() %>%
  mutate(`Type-1 error` = percent,
         .after = Variance
  )

# d_summary_zero <- d.with.performance %>%
#   select(type:nonconverged.percent) %>%
#   select(-Rep, -est) %>%
#   filter(sign.X.sig == "Zero") %>%
#   distinct() %>%
#   mutate(`Type-1 error` = 100 - percent,
#          .after = Variance
#   )
#
# d_summary_positive <- d.with.performance %>%
#   select(type:nonconverged.percent) %>%
#   select(-Rep, -est) %>%
#   filter(sign.X.sig == "Positive") %>%
#   distinct() %>%
#   mutate(`Type-1 error` = 100 - percent,
#          .after = Variance
#   )
#
# d_summary_zero <- d.with.performance %>%
#   select(type:nonconverged.percent) %>%
#   select(-Rep, -est) %>%
#   filter(sign.X.sig == "Zero") %>%
#   distinct() %>%
#   mutate(`Type-1 error` = 100 - percent,
#          .after = Variance
#   )

dd <- d_summary[with(d_summary, order(type, l2.X.Model, parameter, N, T)), ]

NAR_MSE <- dd %>%
  filter(Model == "NAR") %>%
  pull(MSE) %>%
  rep(times = 4)

# dd$`Relative efficiency` <- dd$MSE/NAR_MSE

d_important <- dd %>%
  mutate(`Model name` = factor(paste0(Model, "(1)"),
                               levels = c("NAR(1)",
                                          "Chi2AR(1)",
                                          "BinAR(1)",
                                          "PoDAR(1)")
  ),
  `Means distribution` = paste0(l2.dist, "-distributed means"),
  Resid = case_when(type == "resid.fixed" ~ "`Fixed Residual Variance`",
                    type == "resid.random" ~ "`Random Residual Variance`"),
  .after = l2.X.Model)
# %>%
#   relocate(`Relative efficiency`,
#            .after = Variance)

levels(d_important$`Model name`) <- c(`NAR(1)` = "AR(1)",
                                      `Chi2AR(1)` = TeX("$\\chi^2$AR(1)"),
                                      `BinAR(1)` = "BinAR(1)",
                                      `PoDAR(1)` = "PoDAR(1)")

dimp <- d_important %>%
  complete()


#  summary plots ----------------------------------------------------------

plot_Model.x.Resid <- function(d_important,
                               which.parameter,
                               which.measure = "Positive error",
                               Means.dist = c("Gaussian", "Chi2"),
                               line.width = 1,
                               font.scale = 5){

  ddd <- d_important %>%
    filter(parameter == which.parameter)

  if(!grepl("error",
            tolower(which.measure),
            fixed=TRUE)){
    ddd$value <- ddd %>% pull(which.measure)
    ddd <- ddd %>% filter(sign.X.sig == "Zero")
    y.axis <- which.measure
    }

  if(grepl("positive",
           tolower(which.measure),
           fixed=TRUE)){
    which.measure <- "Type-1 error"
    ddd$value <- ddd %>% pull(which.measure)
    ddd <- ddd %>% filter(sign.X.sig == "Positive")
    y.axis <- "Positive error"
  }

  if(grepl("negative",
           tolower(which.measure),
           fixed=TRUE)){
    which.measure <- "Type-1 error"
    ddd$value <- ddd %>% pull(which.measure)
    ddd <- ddd %>% filter(sign.X.sig == "Negative")
    y.axis <- "Negative error"
  }

  if(grepl("total",
           tolower(which.measure),
           fixed=TRUE)){
    which.measure <- "Type-1 error"
    ddd$value <- 100 - (ddd %>% pull(which.measure))
    ddd <- ddd %>% filter(sign.X.sig == "Zero")
    y.axis <- "Total error"
  }


  y.range <- ddd$value %>% range()
  y.range[1] <- min(-0.001, y.range[1])
  y.range[2] <- max(0.001, y.range[2])

  ddd <- ddd  %>%
    filter(l2.dist == Means.dist) %>%
    ungroup() %>%
    complete(Resid, `Model name`, N, T,
             fill = list(value = 0))

  ddd$N <- as.factor(ddd$N)
  ddd$T <- as.factor(ddd$T)

  if(which.measure == "Relative efficiency")
    ddd <- ddd %>% filter(`Model name` != "AR(1)")

  title <- ifelse(Means.dist == "Gaussian",
                  TeX("Gaussian-distributed means $\\phantom{\\chi^2}$"),
                  TeX("$\\chi^2$-distributed means")
  )


  output.plot <- ddd %>%
    ggplot() +
    aes(
      x = T,
      y = value,
      group = N,
      color = N
    ) +
    ## make solid y=0 axis line
    geom_hline(yintercept = 0,
               linetype = "solid",
               color = "black",
               alpha = 1) +
    geom_line(size = rel(1),
              alpha = 0.8,
              lineend = "round") +
    geom_point(size = rel(1.5), alpha = 1) +
    scale_color_manual(values = brewer.pal(name = "PuRd", n = 9)[c(4, 6, 9)]) +
    # scale_color_viridis(
    #   discrete = TRUE,
    #   begin = 0.2,
    #   end = 0.7,
    #   direction = -1,
    #   option = "inferno"
    # ) +
    facet_grid(
      rows = vars(`Model name`),
      cols = vars(Resid),
      labeller = label_parsed
    ) +
    theme_light() +
    theme_pubclean() +
    scale_y_continuous(limits = y.range) +
    ylab(y.axis) +
    ggtitle(title) +
    theme(
      plot.title = element_text(#size = rel(1.5),# 4*font.scale,
        family = "CMU Serif"),
      panel.spacing = unit(0.7, "lines"),
      legend.position = "bottom",
      legend.key = element_rect(colour = NA, fill = NA),
      legend.key.width = unit(10, "mm"),
      # legend.key.height = unit(10, "mm"),
      text = element_text(size = 10,
                          family = "CMU Serif")
    )

  if(which.measure == "Type-1 error" | which.measure == "Relative efficiency"){

    output.plot <- ddd %>%
      ggplot() +
      aes(
        x = T,
        y = value,
        group = N,
        color = N
      ) +
      ## make solid y=0 axis line
      geom_hline(yintercept = 0,
                 linetype = "solid",
                 color = "black",
                 alpha = 1) +
      ## manually add needed grids
      geom_hline(yintercept = c(10, 20, 40, 60),
                 linetype = "dotted",
                 color = "#BEBEBE",
                 alpha = 1) +
      geom_line(size = rel(1),
                alpha = 0.8,
                lineend = "round") +
      geom_point(size = rel(1.5), alpha = 1) +
      scale_color_manual(values = brewer.pal(name = "PuRd", n = 9)[c(4, 6, 9)]) +
      # scale_color_viridis(
      #   discrete = TRUE,
      #   begin = 0.2,
      #   end = 0.7,
      #   direction = -1,
      #   option = "inferno"
      # ) +
      facet_grid(
        rows = vars(`Model name`),
        cols = vars(Resid),
        labeller = label_parsed
      ) +
      theme_light() +
      theme_pubclean() +
      scale_y_continuous(breaks = c(2.5, 10, 20, 40, 60, 80, 90, 100),
                         # looked up myself to assure  + and - ranges are equal
                         limits = c(0, 61)) +
      ylab(y.axis) +
      ggtitle(title) +
      theme(
        plot.title = element_text(#size = rel(1.5),# 4*font.scale,
          family = "CMU Serif"),
        panel.spacing = unit(0.7, "lines"),
        legend.position = "bottom",
        legend.key = element_rect(colour = NA, fill = NA),
        legend.key.width = unit(10, "mm"),
        # legend.key.height = unit(10, "mm"),
        text = element_text(size = 10,
                            family = "CMU Serif"),
        ## remove grid lines
        panel.grid.major.y = element_blank()
      )
       # add thresholds
      # geom_hline(yintercept = 2.5,
      #            linetype = "dotted",
      #            alpha = 0.7) +
    if (y.axis == "Total error")
      output.plot <- output.plot +
        geom_hline(yintercept = 5,
                   linetype = "dashed",
                   alpha = 0.7)

    if (y.axis != "Total error")
      output.plot <- output.plot +
        geom_hline(yintercept = 2.5,
                   linetype = "dashed",
                   alpha = 0.7)

  }

  output.plot <- output.plot +
    geom_hline(yintercept = 0,
               alpha = 0.6)

  # if(which.measure == "Relative efficiency"){
  #   output.plot <- output.plot +
  #     scale_y_continuous(limits = y.range) +
  #     geom_hline(yintercept = 1, linetype = "dashed") +
  #     coord_trans(y = "log10")
  # }

  return(output.plot)

}



# Function to plot quadrants ----------------------------------------------


plot_quadrants <- function(d_important,
                           which.parameter = "Correlation",
                           upper.measure = "Bias",
                           lower.measure = "RMSE",
                           line.width = 2,
                           font.scale = 8){



  p.upper.left <- plot_Model.x.Resid(d_important,
                                     which.parameter,
                                     upper.measure,
                                     "Gaussian",
                                     line.width,
                                     font.scale) + xlab(NULL)

  p.upper.right <- plot_Model.x.Resid(d_important,
                                      which.parameter,
                                      upper.measure,
                                      "Chi2",
                                      line.width,
                                      font.scale) + xlab(NULL) + ylab(NULL)


  p.lower.left <- plot_Model.x.Resid(d_important,
                                     which.parameter,
                                     lower.measure,
                                     "Gaussian",
                                     line.width,
                                     font.scale)  + ggtitle(NULL)

  p.lower.right <- plot_Model.x.Resid(d_important,
                                      which.parameter,
                                      lower.measure,
                                      "Chi2",
                                      line.width,
                                      font.scale)  + ggtitle(NULL) + ylab(NULL)


  p.patchwork <- (p.upper.left | p.upper.right) / (p.lower.left | p.lower.right)

  title <- paste(upper.measure,
                 "and",
                 lower.measure,
                 "in the estimated",
                 tolower(which.parameter)
                 )

  if(which.parameter == "Fixed.Phi") title <- "Level-2 $\\phi$"

  if(grepl("error", tolower(upper.measure), fixed=TRUE)){
    title <- paste("Type-I error rates in the estimated",
                   tolower(which.parameter)
                   )
                   }


  p.final <- p.patchwork +
    plot_layout(guides = "collect") +
    plot_annotation(##
                    ## removed the big title and subtitle  here
                    ##
                    #title = TeX(title),
                    # subtitle = " ",
                    theme = theme(plot.title =
                                    element_text(size = 20,
                                                 family = "CMU Serif",
                                                 hjust = 0.5))
    ) &
    theme(legend.position = "bottom")

  return(p.final)

}


# Making the 4-quadrant plots ---------------------------------------------


for (parameter in c("Correlation", "Covariance")) {
  for (upper_lower in list(c("Bias", "RMSE"),
                           c("Bias", "Variance"),
                           c("Bias", "MAE"),
                           c("Positive error", "Negative error"))) {
    upper <- upper_lower[1]
    lower <- upper_lower[2]

    (p.q <- plot_quadrants(d_important,
                           parameter,
                           upper,
                           lower)) %>%
      ## Saving the plot
      ggsave(
        paste0(parameter,
               " (",
               upper,
               " and ",
               lower,
               ").pdf"),
        .,
        width = 21,
        height = 30,
        units = "cm"
      )
  }
}


# Plotting total Type-I error rate ----------------------------------------

parameter <- "Covariance"
parameter <- "Correlation"

p.error.left <- plot_Model.x.Resid(d_important,
                                   parameter,
                                   "Total error",
                                   "Gaussian",
                                   line.width,
                                   font.scale)


p.error.right <- plot_Model.x.Resid(d_important,
                                    parameter,
                                    "Total error",
                                    "Chi2",
                                    line.width,
                                    font.scale) + ylab(NULL)


p.patchwork <- (p.error.left | p.error.right)


title <- paste("Type-I error rate in the estimated",
               tolower(parameter)
)

p.final <- p.patchwork +
  plot_layout(guides = "collect") +
  plot_annotation(
                  ## removed the big title and subtitle  here
                  ##title = TeX(title),
                  theme = theme(plot.title =
                                  element_text(size = 20,
                                               family = "CMU Serif",
                                               hjust = 0.5))
  ) &
  theme(legend.position = "bottom") &
  scale_y_continuous(breaks = c(5, 10, 20, 40, 60, 80, 90, 100))


ggsave(paste0(parameter,
              " (Type-1 error).pdf"),
       p.final,
       width = 21,
       height = 21.5*30/37 - 1,
       units = "cm")



# Plotting estimation variances (in half-page plots) ---------------------

parameter <- "Covariance"
parameter <- "Correlation"

p.var.left <- plot_Model.x.Resid(d_important,
                                   parameter,
                                   "Variance",
                                   "Gaussian",
                                   line.width,
                                   font.scale)


p.var.right <- plot_Model.x.Resid(d_important,
                                    parameter,
                                    "Variance",
                                    "Chi2",
                                    line.width,
                                    font.scale) + ylab(NULL)


p.patchwork <- (p.var.left | p.var.right)


title <- paste("Estimation variance in the estimated",
               tolower(parameter)
)

p.final <- p.patchwork +
  plot_layout(guides = "collect") +
  plot_annotation(
                  ## removed the big title and subtitle  here
                  ##title = TeX(title),
                  theme = theme(plot.title =
                                  element_text(size = 20,
                                               family = "CMU Serif",
                                               hjust = 0.5))
  ) &
  theme(legend.position = "bottom")
# &
#   scale_y_continuous(limits = c(0, NA))


ggsave(paste0(parameter,
              " (estimation variance).pdf"),
       p.final,
       width = 21,
       height = 21.5*30/37 - 1,
       units = "cm")

