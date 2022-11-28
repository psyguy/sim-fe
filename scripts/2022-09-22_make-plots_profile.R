
# Packages ----------------------------------------------------------------

# rm(list = ls())

library(tidyverse)
library(GGally)
library(RColorBrewer)
library(patchwork)
library(ggthemes)
library(latex2exp)
library(ggpubr)

library(correlation)


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

p.colors <-
  brewer.pal(name = "YlOrBr", n = 9)[c(5, 7, 9)]


Mode <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}

signif <- function(p.value) {
  symnum(
    p.value,
    corr = FALSE,
    na = FALSE,
    cutpoints = c(0,
                  0.001, 0.01, 0.05, 0.1, 1),
    symbols = c("***", "**", "*", ".", " ")
  )
}

# Pair plots --------------------------------------------------------------


sim.object <- read_rds("../cogito-data-prep/data_Rep-1/sim_uSeed-13293_l2.dist-Chi2_Model-BinAR_N-100_phi-0.4_T-100_sim.Seed-0_Rep-1.rds")


d_sim <- sim.object$sim.Dataset

model <- sim.object$Model
title <- model

# if(model == "PoDAR") binwidth <- 1
# if(model == "Chi2AR"){
#   binwidth <- 1.5
#   title <- "$\\chi^2$AR"
# }


plot_pairplots <- function(d,
                           p.colors = c("#FE9929",
                                        "#CC4C02",
                                        "#662506"),
                           rel_title = 2,
                           rel_lines = 0.5,
                           rel_dots = 3,
                           bins = 30
                           ){

color_hist <- p.colors[2]
color_scatter <- p.colors[1]

d <- d %>%
  group_by(subject) %>%
  summarise(
    Mean = mean(x,
                na.rm = TRUE),
    Mode = Mode(x),
    Variance = var(x,
                   na.rm = TRUE),
    Skewness = moments::skewness(x,
                        na.rm = TRUE)
    ) %>%
  na.omit()

cors <- d %>%
  select(Mean, Variance, Skewness) %>%
  correlation::correlation() %>%
  as.data.frame() %>%
  mutate(
    value = round(r, 3),
    significance = cut(
      p,
      breaks = c(0, 0.001, 0.01, 0.05, 1),
      include.lowest = T,
      labels = c('***', '**', '*', '')
    ),
    string = paste0(value, significance)
  ) %>%
  select(-r:-n_Obs)

## plots

margin.thing <- 5


#################
###  Correlations
#################

p_cor_Mean.Variance <-
  ggplot() +
  geom_text(aes(x = mean(range(d$Variance)),
                y = mean(range(d$Mean)),
                label = paste("Corr:",
                              cors %>%
                                filter(Parameter1 == "Mean",
                                       Parameter2 == "Variance") %>%
                                pull(string)))) +
  theme_tufte() +
  theme(
    text = element_text(size = rel(rel_title)),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  )

p_cor_Mean.Skewness <-
  ggplot() +
  geom_text(aes(x = mean(range(d$Skewness)),
                y = mean(range(d$Mean)),
                label = paste("Corr:",
                              cors %>%
                                filter(Parameter1 == "Mean",
                                       Parameter2 == "Skewness") %>%
                                pull(string)))) +
  theme_tufte() +
  theme(
    text = element_text(size = rel(rel_title)),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  )

p_cor_Variance.Skewness <-
  ggplot() +
  geom_text(aes(x = mean(range(d$Skewness)),
                y = mean(range(d$Variance)),
                label = paste("Corr:",
                              cors %>%
                                filter(Parameter1 == "Variance",
                                       Parameter2 == "Skewness") %>%
                                pull(string)))) +
  theme_tufte() +
  theme(
    text = element_text(size = rel(rel_title)),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  )

#################
### Histograms
#################

p_mean <-
  d %>%
  ggplot(aes(x = Mean)) +
  geom_histogram(fill = color_hist,
                 bins = bins) +
  ggtitle("Mean") +
  theme_tufte() +
  theme(
    plot.title = element_text(
      size = rel(rel_title),
      hjust = 0.5,
      margin = margin(t = margin.thing,
                      b = -margin.thing)
    ),
    axis.title = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank()
  )

p_variance <-
  d %>%
  ggplot(aes(x = Variance)) +
  geom_histogram(fill = color_hist,
                 bins = bins) +
  ggtitle("Variance") +
  theme_tufte() +
  theme(
    plot.title = element_text(
      size = rel(rel_title),
      hjust = 0.5,
      margin = margin(t = margin.thing,
                      b = -margin.thing)
    ),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  )

p_skewness <-
  d %>%
  ggplot(aes(x = Skewness)) +
  geom_histogram(fill = color_hist,
                 bins = bins) +
  ggtitle("Skewness") +
  geom_vline(xintercept = 0,
             alpha = 0.5,
             color = "gray45",
             linetype = "solid",
             size = rel(rel_lines)) +
  geom_vline(xintercept = c(-0.5, 0.5),
             color = "gray45",
             linetype = "dotted",
             size = rel(rel_lines)) +
  geom_vline(xintercept = c(-1, 1),
             color = "gray45",
             linetype = "dashed",
             size = rel(rel_lines)) +
  theme_tufte() +
  # scale_x_continuous(limits = 1.02*range(d$Skewness)) +
  theme(
    plot.title = element_text(
      size = rel(rel_title),
      hjust = 0.5,
      margin = margin(t = margin.thing,
                      b = -margin.thing)
    ),
    axis.title = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  )

##################
### scatter plots
#################

p_scatter_Mean.Variance <- d %>%
  ggplot(aes(x = Mean,
             y = Variance)) +
  geom_point(color = color_scatter,
             shape = "bullet",
             alpha = 0.85,
             size = rel(rel_dots)) +
  theme_tufte() +
  theme(
    axis.title = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank()
  )



p_scatter_Mean.Skewness <- d %>%
  ggplot(aes(x = Mean,
             y = Skewness)) +
  geom_hline(yintercept = 0,
             alpha = 0.5,
             color = "gray45",
             linetype = "solid",
             size = rel(rel_lines)) +
  geom_hline(yintercept = c(-0.5, 0.5),
             color = "gray45",
             linetype = "dotted",
             size = rel(rel_lines)) +
  geom_hline(yintercept = c(-1, 1),
             color = "gray45",
             linetype = "dashed",
             size = rel(rel_lines)) +
  geom_point(color = color_scatter,
             shape = "bullet",
             alpha = 0.7,
             size = rel(rel_dots)) +
  theme_tufte() +
  theme(
    axis.title = element_blank()
  )



p_scatter_Variance.Skewness <- d %>%
  ggplot(aes(x = Variance,
             y = Skewness)) +
  geom_hline(yintercept = 0,
             alpha = 0.5,
             color = "gray45",
             linetype = "solid",
             size = rel(rel_lines)) +
  geom_hline(yintercept = c(-0.5, 0.5),
             color = "gray45",
             linetype = "dotted",
             size = rel(rel_lines)) +
  geom_hline(yintercept = c(-1, 1),
             color = "gray45",
             linetype = "dashed",
             size = rel(rel_lines)) +
  geom_point(color = color_scatter,
             shape = "bullet",
             alpha = 0.7,
             size = rel(rel_dots)) +
  theme_tufte() +
  theme(
    axis.title = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  )

##########################
## Putting plots together
##########################

p_pairplots <-
  (p_mean + p_cor_Mean.Variance + p_cor_Mean.Skewness) /
  (p_scatter_Mean.Variance + p_variance + p_cor_Variance.Skewness) /
  (p_scatter_Mean.Skewness + p_scatter_Variance.Skewness + p_skewness) &
  # plot_layout(guides = "collect") &
  plot_annotation(
                  theme = theme(plot.title = element_text(family = "Merriweather Regular"),
                                axis.text = element_text(size = rel(rel_title)/2)
                  )
                  )

## Returning the 3x3 pairplots
p_pairplots

}

# plot_pairplots(d_sim) + plot_layout(widths = 1, heights = 4.1)

plot_histograms <- function(d,
                            binwidth = 0.5,
                            nrow.facet = 20,
                            p.colors = c("#FE9929",
                                         "#CC4C02",
                                         "#662506")){

  fill_col <- p.colors[2]
  x.range <- d$x %>% range()

  d %>%
    group_by(subject) %>%
    mutate(mm = mean(x)) %>%
    # mutate(sk = moments::skewness(x),
    #        sk_star = case_when(sk < 1 & sk > 0.5  ~ "*",
    #                            sk < 0.5 & sk > -0.5 ~ " ",
    #                            sk > -1 & sk < -0.5 ~ "*",
    #                            TRUE ~ "**")
    #        ) %>%
    arrange(desc(mm)) %>%
    ggplot(aes(x = x,
               y = ..ndensity..)) +
    geom_histogram(
                   binwidth = binwidth,
                   fill = fill_col,
                   center = 0) +
    facet_wrap(~reorder(subject, mm),
               nrow = nrow.facet) +
    # geom_text(aes(label = sk_star),
    #           size = rel(0.1),
    #           x = sum(x.range)/2,
    #           y = 0.9) +
    geom_hline(yintercept = 0,
               size = 0.1) +
    ggtitle(NULL) +
    theme_tufte() +
    theme(strip.background = element_blank(),
          aspect.ratio = 1,
          strip.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank())

}


# title <- ifelse(Means.dist == "Gaussian",
#                 TeX("Gaussian-distributed means $\\phantom{\\chi^2}$"),
#                 TeX("$\\chi^2$-distributed means")
# )

sim.object <- read_rds("../cogito-data-prep/data_Rep-1/sim_uSeed-13293_l2.dist-Chi2_Model-BinAR_N-100_phi-0.4_T-100_sim.Seed-0_Rep-1.rds")

plot_dataset.profile <- function(sim.object,
                                 p.colors = NULL,
                                 title_l2.dist = FALSE){


  palette_nar <- brewer.pal(name = "YlOrBr", n = 9)[c(5, 7, 9)]
  palette_chiar <- brewer.pal(name = "GnBu", n = 9)[c(5, 7, 9)]
  palette_binar <- brewer.pal(name = "YlGn", n = 9)[c(5, 7, 9)]
  palette_podar <- brewer.pal(name = "BuPu", n = 9)[c(5, 7, 9)]


  d_sim <- sim.object$sim.Dataset

  l2.distr <- ifelse(sim.object$l2.dist == "Chi2",
                     "$\\chi^2$",
                     "Gaussian")
  model <- sim.object$Model
  title <- model

  binwidth <- 0.5
  if(model == "NAR"){
    title <- "AR"
    palette_color <- palette_nar
    }
  if(model == "Chi2AR"){
    title <- "$\\chi^2$AR"
    palette_color <- palette_chiar
    }
  if(model == "BinAR") palette_color <- palette_binar
  if(model == "PoDAR") palette_color <- palette_podar

  if(is.null(p.colors)) p.colors <- palette_color

  title <- paste0(title,
                  "(1)",
                  " $\\phantom{\\chi^2}$")

  if(title_l2.dist)
    title <- paste0(l2.distr,
                    "-distributed means",
                    " $\\phantom{\\chi^2}$")


  p_out <-  plot_histograms(d = d_sim,
                            binwidth = binwidth,
                            nrow.facet = 10,
                            p.colors = p.colors) +
    plot_spacer() +
    plot_pairplots(d = d_sim,
                   p.colors = p.colors) +
    plot_layout(widths = c(1, 0.05, 1)) +
    plot_annotation(title = TeX(title),
                    theme = theme(plot.title = element_text(size = rel(2.5),
                                                            family = "CMU Serif"),
                                  plot.subtitle = element_blank()
                                  )
                    )

  p_out %>% wrap_elements()

}



save_dataset_profile <- function(upper, lower, file.name = "meh", title = NULL){


  p_profiles <-
    (plot_dataset.profile(upper, title_l2.dist = TRUE) /
       plot_spacer() /
       plot_dataset.profile(lower, title_l2.dist = TRUE)) +
    plot_layout(heights = c(1, 0.02, 1)) +
    plot_annotation(
      # title = ifelse(is.null(title),
      #                NULL,
      #                TeX(paste(title, "model $\\phantom{\\chi^2}$"))
      #                ),
      theme = theme(
        plot.title = element_text(size = rel(4),
                                  hjust = 0.5,
                                  family = "CMU Serif"),
        plot.subtitle = element_blank()
      )
    )

  ggsave(paste0(file.name,
                ".pdf"),
         p_profiles,
         width = 2 * 15,
         height = 2.02 * 1.1 * 15,
         units = "cm")

}



# p <- plot_dataset.profile(sim.object)
#
# ggsave("meh-34.pdf",
#        c_chiar,
#        width = 2 * 15,
#        height = 1.1 * 15,
#        units = "cm")

# Plot Gaussian DGMs ------------------------------------------------------


# Reading simulated datasets and saving in lists --------------------------


dgm_datasets_gaussian.means <- list(
  nar = "sim_uSeed-13330_l2.dist-Gaussian_Model-NAR",
  chiar = "sim_uSeed-13297_l2.dist-Gaussian_Model-Chi2AR",
  binar = "sim_uSeed-13286_l2.dist-Gaussian_Model-BinAR",
  podar = "sim_uSeed-13319_l2.dist-Gaussian_Model-PoDAR"
) %>%
  lapply(function(x) read_rds(paste0("data_Rep-1/",
                                     x,
                                     "_N-100_phi-0.4_T-100_sim.Seed-0_Rep-1.rds")))


dgm_datasets_chi2.means <- list(
  nar = "sim_uSeed-13337_l2.dist-Chi2_Model-NAR",
  chiar = "sim_uSeed-13304_l2.dist-Chi2_Model-Chi2AR",
  binar = "sim_uSeed-13293_l2.dist-Chi2_Model-BinAR",
  podar = "sim_uSeed-13326_l2.dist-Chi2_Model-PoDAR"
) %>%
  lapply(function(x) read_rds(paste0("data_Rep-1/",
                                     x,
                                     "_N-100_phi-0.4_T-100_sim.Seed-0_Rep-1.rds")))



# Plot profiles per level-2 means -----------------------------------------

p_profiles_gaussian <-
  (
    plot_dataset.profile(dgm_datasets_gaussian.means$nar) /
    plot_spacer() /
    plot_dataset.profile(dgm_datasets_gaussian.means$chiar) /
    plot_spacer() /
    plot_dataset.profile(dgm_datasets_gaussian.means$binar) /
    plot_spacer() /
    plot_dataset.profile(dgm_datasets_gaussian.means$podar)
  ) +
  plot_layout(heights = c(1, 0.01, 1, 0.01, 1, 0.01, 1)) +
  plot_annotation(
    title = TeX("Gaussian-distributed means $\\phantom{\\chi^2}$"),
    theme = theme(
      plot.title = element_text(size = rel(3),
                                family = "CMU Serif"),
      plot.subtitle = element_blank()
    )
  )

ggsave("profiles-datasets-gaussian__.pdf",
       p_profiles_gaussian,
       width = 2 * 15,
       height = 4.3 * 1.1 * 15,
       units = "cm")


p_profiles_chi2 <-
  (
      plot_dataset.profile(dgm_datasets_chi2.means$nar) /
      plot_spacer() /
      plot_dataset.profile(dgm_datasets_chi2.means$chiar) /
      plot_spacer() /
      plot_dataset.profile(dgm_datasets_chi2.means$binar) /
      plot_spacer() /
      plot_dataset.profile(dgm_datasets_chi2.means$podar)
  ) +
  plot_layout(heights = c(1, 0.01, 1, 0.01, 1, 0.01, 1)) +
  plot_annotation(
    title = TeX("$\\chi^2$-distributed means $\\phantom{\\chi^2}$"),
    theme = theme(
      plot.title = element_text(size = rel(3),
                                family = "CMU Serif"),
      plot.subtitle = element_blank()
    )
  )

ggsave("profiles-datasets-chi2.pdf",
       p_profiles_chi2,
       width = 2 * 15,
       height = 4.3 * 1.1 * 15,
       units = "cm")


ggsave("profiles-datasets.pdf",
       wrap_elements(p_profiles_gaussian) +
         plot_spacer() +
         wrap_elements(p_profiles_chi2) +
         plot_layout(widths = c(1, 0.05, 1)),
       width = (2 * 15)*2.05,
       height = 4.3 * 1.1 * 15,
       units = "cm")


# Plot profiles per DGM ---------------------------------------------------


purrr::pwalk(list(dgm_datasets_gaussian.means,
                  dgm_datasets_chi2.means,
                  names(dgm_datasets_gaussian.means)
                  ),
             ~ save_dataset_profile(..1,
                                    ..2,
                                    paste0("profiles-dataset-",..3)
                                    )
             )


# Make cogito pair plots --------------------------------------------------

cogito.files <- list(distress = readRDS("../cogito-data-prep/cogito-na1.rds"),
                     pa = readRDS("../cogito-data-prep/cogito-pa.rds"))

cogito.colors <- list(distress = c("sienna1",
                                   "sienna2",
                                   "sienna3"),
                      pa = c("steelblue2",
                             "steelblue3",
                             "steelblue4")
                      )

purrr::pwalk(list(cogito.files,
                  cogito.colors,
                  names(cogito.files)
                  ),
             ~ ggsave(paste0("cogito_pairplots_",
                             ..3,
                             ".pdf"),
                      plot_pairplots(..1,
                                     ..2,
                                     bins = 70,
                                     rel_dots = 1.5),
                      width = 2/2.05 * 15,
                      height = 2/2.05 * 15,
                      units = "cm")
             )

