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

# shelf(tikzDevice)

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
  filter(sign.X.sig == "Zero") %>%
  distinct() %>%
  mutate(`Type-1 error` = 100 - percent,
         .after = Variance
  )

dd <- d_summary[with(d_summary, order(type, l2.X.Model, parameter, N, T)), ]

NAR_MSE <- dd %>%
  filter(Model == "NAR") %>%
  pull(MSE) %>%
  rep(times = 4)

dd$`Relative efficiency` <- dd$MSE/NAR_MSE

d_important <- dd %>%
  mutate(`Model name` = factor(paste0(Model, "(1)"),
                        levels = c("NAR(1)",
                                   "Chi2AR(1)",
                                   "BinAR(1)",
                                   "PoDAR(1)")
                        ),
         `Means distribution` = paste0(l2.dist, "-distributed means"),
         Resid = case_when(type == "resid.fixed" ~ "Fixed Residual Variance",
                           type == "resid.random" ~ "Random Residual Variance"),
         .after = l2.X.Model) %>%
  relocate(`Relative efficiency`,
           .after = Variance)


#  summary plots ----------------------------------------------------------

plot_Model.x.Resid <- function(d_important,
                               which.parameter,
                               which.measure = "Bias",
                               Means.dist = c("Gaussian", "Chi2"),
                               line.width = 1,
                               font.scale = 5){

  ddd <- d_important %>%
    filter(parameter == which.parameter)

  ddd$value <- ddd %>% pull(which.measure)

  y.range <- ddd$value %>% range()

  ddd <- ddd %>%
    filter(l2.dist == Means.dist)

  ddd$N <- as.factor(ddd$N)
  ddd$T <- as.factor(ddd$T)

  if(which.measure == "Relative efficiency")
    ddd <- ddd %>% filter(`Model name` != "NAR(1)")

  title <- ifelse(Means.dist == "Gaussian",
                  TeX("Gaussian-distributed means $\\phantom{\\chi^2}$"),
                  TeX("$\\chi^2$-distributed means")
                  )


  output.plot <- ddd %>%
    ggplot() +
    aes(x = T, y = value,
        group = N,
        color = N) +
    geom_line(size = line.width, alpha = 0.6) +
    geom_point(size = 2*line.width, alpha = 1) +
    scale_color_viridis(discrete = TRUE,
                        direction = -1,
                        option = "viridis") +
    facet_grid(rows = vars(`Model name`),
               cols = vars(Resid)) +
    theme_light() +
    scale_y_continuous(limits = y.range) +
    scale_color_viridis_d(direction = -1) +
    ylab(which.measure) +
    ggtitle(title) +
    theme(
          plot.title = element_text(size = 4*font.scale,
                                    family = "CMU Serif"),
          legend.position = "bottom",
          text = element_text(size = 3*font.scale,
                              family = "Merriweather Regular")
          )

  if(which.measure == "Type-1 error"){
    output.plot <- output.plot +
      scale_y_continuous(breaks = c(0, 5, 10, 20, 40, 60, 80, 90, 100),
        limits = c(0,y.range[2])) +
      geom_hline(yintercept = 0, linetype = "solid") +
      geom_hline(yintercept = 5, linetype = "dotted") +
      geom_hline(yintercept = 10, linetype = "dashed")
  } else{
    output.plot <- output.plot +
      geom_hline(yintercept = 0, linetype = "dashed")
  }

  if(which.measure == "Relative efficiency"){
    output.plot <- output.plot +
      scale_y_continuous(limits = y.range) +
      geom_hline(yintercept = 1, linetype = "dashed") +
      coord_trans(y = "log10")
  }

  return(output.plot)

}



# Function to plot quadrants ----------------------------------------------


plot_quadrants <- function(d_important,
                           which.parameter = "Correlation",
                           upper.measure = "Bias",
                           lower.measure = "Variance",
                           line.width = 2,
                           font.scale = 8){


  p.upper.left <- plot_Model.x.Resid(d_important,
                                     which.parameter,
                                     upper.measure,
                                     "Gaussian",
                                     line.width,
                                     font.scale) +
    xlab(NULL)

  p.upper.right <- plot_Model.x.Resid(d_important,
                                      which.parameter,
                                      upper.measure,
                                      "Chi2",
                                      line.width,
                                      font.scale) +
    xlab(NULL) +
    ylab(NULL)


  p.lower.left <- plot_Model.x.Resid(d_important,
                                   which.parameter,
                                   lower.measure,
                                   "Gaussian",
                                   line.width,
                                   font.scale) +
    ggtitle(NULL)

  p.lower.right <- plot_Model.x.Resid(d_important,
                                      which.parameter,
                                      lower.measure,
                                      "Chi2",
                                      line.width,
                                      font.scale) +
    ggtitle(NULL) +
    ylab(NULL)


p.patchwork <- (p.upper.left | p.upper.right) / (p.lower.left | p.lower.right)

title <- paste(which.parameter, "between $\\phi$ and $\\mu$")

if(which.parameter == "Fixed.Phi") title <- "Level-2 $\\phi$"


p.final <- p.patchwork +
  plot_layout(guides = "collect") +
  plot_annotation(title = TeX(title),
                subtitle = " ",
                theme = theme(plot.title =
                                element_text(size = 55,
                                             family = "CMU Serif",
                                             hjust = 0.5))
                ) &
  theme(legend.position = "bottom")

return(p.final)

}


# Making the 4-quadrant plots ---------------------------------------------

## Choose a parameter

parameter <- "Correlation"
parameter <- "Covariance"
parameter <- "Fixed.Phi"


## Choose measures to be plotted in the upper and lower quadrants\

upper <- "Bias"
lower <- "Variance"

upper <- "Bias"
lower <- "MSE"

upper <- "MAE"
lower <- "MSE"

upper <- "Bias"
lower <- "MAE"

upper <- "RMSE"
lower <- "Type-1 error"

upper <- "Type-1 error"
lower <- "Relative efficiency"

## Plotting

p.q <- plot_quadrants(d_important,
                      parameter,
                      upper,
                      lower)

## Saving the plot

ggsave(paste0(parameter,
              " (",
              upper,
              " and ",
              lower,
              ").pdf"),
       p.q,
       width = 21,
       height = 30,
       units = "in")

