#' ---
#' title: "Making plots of the data generating models"
#' author: "Manuel Haqiqatkhah"
#' date: "2022-05-23"
#' output: github_document
#' ---
#'
#'
#'


#+ initialization
library(librarian)
shelf(tidyverse)
shelf(grid)
shelf("daattali/ggExtra")
shelf(ggpubr)
shelf(cowplot)
shelf(latex2exp)
shelf(viridis)
shelf(ggtext)
shelf(patchwork)
shelf(ggthemes)
# rm(list = ls())

source("functions/functions_data-generating-models.R")

# Change font in plots ----------------------------------------------------

# https://stackoverflow.com/a/51906008/2275986

shelf(showtext)

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


# Individual plots --------------------------------------------------------


make_dgm_df <- function(obj.1,
                        obj.2,
                        obj.3 = NULL){

  d.1 <- data.frame(t = 1:length(obj.1$x),
                    value = obj.1$x,
                    obj.id = obj.1$Model.Description.Short)

  d.2 <- data.frame(t = 1:length(obj.2$x),
                    value = obj.2$x,
                    obj.id = obj.2$Model.Description.Short)

  d <- rbind(d.1, d.2)

  if(!is.null(obj.3)){
    d.3 <- data.frame(t = 1:length(obj.3$x),
                      value = obj.3$x,
                      obj.id = obj.3$Model.Description.Short)
    d <- rbind(d, d.3)
  }

  d <- d %>%
    group_by(obj.id) %>%
    mutate(Mean = mean(value))

  return(d)
}

## the following are hard to distinguish in black and white

plot_dgm.profile <- function(d,
                  # y.range = c(0, 100),
                  p.colors = colors.nar.khaki,
                  Model.name = "NAR(1)",
                  burnin_proportion = 0.9){

  ### Preparing the data

  ## Choosing histogram binwidth

  # binwidth_ <- ifelse(grepl("bina", tolower(Model.name), fixed=TRUE),
  #                    0.3,
  #                    1)

  binwidth_ <- 0.5

  ## To make sure the plot title has the same height as Chi2AR model
  Model.name <- paste("$\\phantom{\\chi^2}$",
                      Model.name,
                      "$\\phantom{\\chi^2}$")

  d$obj.id <- d$obj.id %>% factor(unique(d$obj.id))
  d <- d %>%
    filter(t > max(d$t)*burnin_proportion) %>%
    mutate(t = t - burnin_proportion*max(t))
  # d$t <- d$t - length(d$t)


  acf.lag.max <- 10

  d_acf <- d %>%
    group_by(obj.id) %>%
    summarize(lag = stats::acf(value,
                               lag.max = acf.lag.max,
                               plot = FALSE)$lag %>% as.numeric(),
              acf = stats::acf(value,
                               lag.max = acf.lag.max,
                               plot = FALSE)$acf %>% as.numeric()
    )
  empirical.rho <- d_acf %>%
    filter(lag == 1)
  empirical.rho <- empirical.rho[rep(1:nrow(empirical.rho),
                                     times = acf.lag.max*20 + 1), ]

  e.l <- rep(seq(0, acf.lag.max, 0.05),
             each = 3)
  empirical.rho$lag <- e.l
  empirical.rho$acf <- empirical.rho$acf^empirical.rho$lag


  # time series plot
  p_ts <- d %>%
    ggplot(aes(x = t,
               y = value,
               group = obj.id)) +
    geom_line(aes(color = obj.id),
              alpha = 1,
              size = 1) +
    geom_hline(aes(yintercept = Mean),
               size = 0.75,
               linetype = "dashed",
               color = "honeydew3") +
    # geom_point(size = 0.1) +
    # expand_limits(x = range(d$t),
    #               y = range(d$value)) +
    # scale_y_continuous(breaks = (0:max_scale_value))
    ggtitle("Time series") +
    xlab("t") +
    theme(
      axis.title.y = element_blank(),
      # axis.text.y = element_blank(),
      # axis.ticks.y = element_blank(),
      legend.position = "none", # c(0.1,0.95),
      legend.background = element_rect(colour = NA, fill = NA),
      axis.title.x = element_text(size = 17,
                                  family = "Merriweather Regular"),
      axis.text = element_text(size = 20,
                                  family = "Merriweather Regular"),
      # legend.key.size = unit(2, "cm"),
      legend.text.align	= 0,
      legend.title = element_blank(),
      legend.key = element_rect(colour = NA,
                                fill = NA),
      # axis.line.x = element_line(colour = "black"),
      panel.background = element_blank()) +
    scale_colour_manual(values = p.colors,
                        labels = unname(TeX(unique(as.character(d$obj.id))))
    )

  # p_ts
  # +
  #   scale_size_manual(values = c(1.5,0.75),
  #                     labels = unname(TeX(c(obj.1$params.str,
  #                                           obj.2$params.str)))
  #   )


  p_hist <- d %>%
    # mutate(empirical.mean.tex =
    #          paste0("$\\hat \\mu = ",
    #                 round(Mean, 1),
    #                 "$")
    #      ) %>%
    ggplot(aes(x = value,
               # y =..ndensity..,
               group = obj.id,
               fill = obj.id)) +
      geom_histogram(aes(y=..ndensity..),
                     center = 0,
                     binwidth = binwidth_) +
    # geom_histogram(#binwidth = binwidth,
    #                aes(fill = obj.id),
    #                center = 0) +
    # geom_line(aes(color = obj.id),
    #           size = 1) +
    # geom_point(size = 0.1) +
    # expand_limits(x = range(d$t),
    #               y = range(d$value)) +
    # scale_y_continuous(breaks = (0:max_scale_value))
      facet_wrap(~obj.id,
                 nrow = 3) +
      geom_hline(yintercept = 0,
                 size = 0.5) +
      geom_segment(aes(x = Mean,
                       xend = Mean,
                       y = 0,
                       yend = 1),
                 size = 0.75,
                 linetype = "dashed",
                 color = "honeydew3") +
    # geom_text(
    #   # "label",
    #   aes(x = Mean,
    #   y = 0.95,
    #   group = obj.id,
    #   label = round(Mean, 1)
    #   # lapply(empirical.mean.tex,
    #   #                function(x){TeX(x, output = "character")}
    #   #                )
    #   ),
    #   color = "honeydew3",
    # ) +
      theme_tufte() +
    ggtitle("Marginal distribution") +
    xlab("X") +
    scale_y_continuous(limits = c(min(d_acf$acf), 1)) +
    theme(strip.background = element_blank(),
          strip.text = element_blank(),
          axis.title.x = element_text(size = 17,
                                      family = "Merriweather Regular"),
          # axis.title.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          # axis
      # axis.text.y = element_blank(),
      # axis.ticks.y = element_blank(),
      legend.position = "none", # c(0.2,0.90),
      # legend.key.size = unit(2, "cm"),
      legend.text.align	= 0,
      legend.title = element_blank(),
      legend.key = element_rect(colour = NA,
                                fill = NA),
      # axis.line = element_line(colour = "black"),
      panel.background = element_blank()) +
    scale_fill_manual(values = p.colors,
                      unname(TeX(unique(as.character(d$obj.id))))
    )


  ### ACF plots

  d_acf <- d %>%
    group_by(obj.id) %>%
    summarize(lag = stats::acf(value,
                               lag.max = acf.lag.max,
                               plot = FALSE)$lag %>% as.numeric(),
              acf = stats::acf(value,
                               lag.max = acf.lag.max,
                               plot = FALSE)$acf %>% as.numeric(),
              empirical.rho = stats::acf(value,
                               lag.max = acf.lag.max,
                               plot = FALSE)$acf[2] %>% as.numeric())




  p_acf <- d_acf %>%
    # filter(obj.id == "$\\mu = 60,\\; \\phi = 0.4,\\; \\sigma^2_{\\epsilon} = 15$") %>%
    ggplot(aes(x = lag,
               y = acf,
               color = obj.id)) +
    geom_segment(aes(x = lag,
                     xend = lag,
                     y = 0,
                     yend = acf
                     ),
                 size = 1.5,
                 lineend = "butt") +
    # exponential fit
    geom_line(data = empirical.rho,
              aes(x = lag,
                  y = acf),
              linetype = 2,
              color = "honeydew3",
              size = 0.75) +
    facet_wrap(~obj.id,
               nrow = 3) +
    geom_hline(aes(yintercept = 0),
               size = 0.5) +
    ggtitle("Sample ACF") +
    theme_tufte() +
    theme(strip.background = element_blank(),
          strip.text = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = 17,
                                    family = "Merriweather Regular"),
          # axis.ticks.y = element_blank(),
          # axis.text.y = element_blank(),
          # axis.title.y = element_blank(),
          # axis
          # axis.text.y = element_blank(),
          # axis.ticks.y = element_blank(),
          legend.position = "none", # c(0.2,0.90),
          # legend.key.size = unit(2, "cm"),
          legend.text.align	= 0,
          legend.title = element_blank(),
          legend.key = element_rect(colour = NA,
                                    fill = NA)
          ) +
    scale_x_continuous(breaks = seq(0, acf.lag.max, 2)) +
    xlab("Lag") +
    ylab("Autocorrelation function") +
    # ggtitle("Autocorrelation function of X(t)") +
    theme(# axis.title.x=element_blank(),
      #     axis.text.x=element_blank(),
      #     axis.ticks.x=element_blank(),
      # axis.line = element_line(colour = "black"),
      panel.background = element_blank()) +
    scale_color_manual(values = p.colors,
                       unname(TeX(unique(as.character(d$obj.id))))
    )


  # leg <- as_ggplot(cowplot::get_legend(p_hist))

  # leg <- cowplot::get_legend(p_ts +
  #                              guides(color = guide_legend(override.aes = list(size = 7)))+
  #                              theme(legend.position = "bottom",
  #                                    legend.direction="vertical",
  #                                    legend.justification = "left",
  #                                    legend.spacing.x = unit(10, "mm"),
  #                                    legend.spacing.y = unit(10, "mm"),
  #                                    legend.text =
  #                                      element_text(size = 20, vjust = .5, hjust = .3)
  #                                    )
  #                            )
  #
  #   leg <- plot_grid(NULL, leg,
  #                    nrow = 1,
  #                    rel_widths = c(0.1, 10))

  p_profile.main <- p_ts + plot_spacer() + p_hist + plot_spacer() + p_acf +
  plot_layout(widths = c(4, 0.05, 1.5, 0.05, 2)) &
    theme(
      # legend.key.width = unit(2, "line"),
      # legend.text=element_text(size=10),
      # legend.position = "none",
      axis.text = element_text(size = 17,
                               family = "Merriweather Regular"),
      plot.title = element_text(size = 15+5,
                          family = "Merriweather Regular")
    )

  p_out <- p_profile.main
  # +
  # plot_annotation(
  #   # title = TeX(Model.name),
  #   #               subtitle = " ",
  #                   theme = theme(axis.text = element_text(size = 15,
  #                                                            family = "Merriweather Regular"))
  #                     # plot.title =
  #                     #               element_text(size = 25+5,
  #                     #                            family = "CMU Serif",
  #                     #                            hjust = 0.5),
  #                     #             plot.subtitle =
  #                     #               element_text(size = 5,
  #                     #                            family = "CMU Serif",
  #                     #                            hjust = 0.5)
  #                     #             )
  #   )

  # p_out <- plot_grid(NULL,
  #                    p_out,
  #                    NULL,
  #                    leg,
  #                    NULL,
  #                    ncol = 1,
  #                    rel_heights = c(0.1,
  #                                    10,
  #                                    0.1,
  #                                    2,
  #                                    0.6))

  return(p_out)

}


