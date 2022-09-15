d <- harv %>%
  # filter(l2.dist == "Chi2",
  #        Model == "PoDAR") %>%
  filter(standardization == "unstd",
         param.name == "X.WITH.PHI") %>%
  select(type,
         N,
         T,
         l2.dist,
         Model,
         est,
         sig,
         lower_2.5ci,
         upper_2.5ci
         ) %>%
  group_by(N, T, type, Model, l2.dist) %>%
  mutate(sign.X.sig = as.factor(sign(est)*sig)) %>%
  # mutate(sign.X.sig = factor(sign.X.sig, levels = c("Negative Type-I error",
  #                                                   "Non-significant estimates",
  #                                                   "Positive Type-I error"))) %>%
  mutate(mean.est = mean(est),
         n.datasets = n(),
         nonconverged.percent = round(100*(1000-n())/1000,2)) %>%
  group_by(sign.X.sig,
           .add = TRUE) %>%
  mutate(error.percents = round(100*n()/n.datasets,2)) %>%
  arrange(N, T,
          .by_group = TRUE) %>%
  mutate(ordering = est # + 100*sign(est)*sig
         ) %>%
  arrange(ordering) %>%
  ungroup() %>%
  group_by(type,
           N,
           T,
           l2.dist,
           Model) %>%
  mutate(ord = order(ordering) - n()/2) %>%
  mutate(NN = as.factor(paste("N =", N)),
         TT = as.factor(paste("T =", T)))


levels(d$sign.X.sig) <- c("Significant negative estimates",
                          "Non-significant estimates",
                          "Significant positive estimates")
# levels(d$TT) <- c("Negative Type-I error",
#                           "Non-significant estimates",
#                           "Positive Type-I error")

# nc <- d %>% unique()

# d <- d_podar %>%
#   filter(N == 100,
#          T == 100) %>%
#   group_by(N, T, type) %>%
#   arrange(sign.X.sig) %>%
#   # group_by(sign.X.sig,
#   #          .add = TRUE) %>%
#   mutate(percent = round(100*n()/n.datasets,1),
#          .after = sign.X.sig) %>%
#   mutate(ordering = 100*sign(est)*sig + est) %>%
#   arrange(ordering) %>%
#   mutate(ord = order(ordering)) %>%
#   arrange(NN, TT)


# d_with_erros <- d %>%
#   group_by(T,
#            N,
#            l2.dist,
#            Model)

# plot_scatter <-


plot_caterpillar <- function(d,
                             l2.dist_ = "Chi2",
                             Model_ = "PoDAR",
                             analysis.type = "resid.fixed",
                             legend.key.width = 5) {

  title <- ifelse(analysis.type == "resid.fixed",
                  "fixed residual variance",
                  "random residual variance") %>%
    paste("Modeled with", .)

  d %>%
    filter(l2.dist == l2.dist_,
           Model == Model_) %>%
    # filter(N == 100,
    #        T == 100) %>%
    filter(type == analysis.type) %>%
    # sample_n(100) %>%
    ggplot() +
    aes(x = ord,
        color = sign.X.sig) +
    geom_segment(
      aes(
        x = ord,
        xend = ord,
        y = upper_2.5ci,
        yend = lower_2.5ci
      ),
      alpha = 0.8+0.2,
      size = rel(0.06)
    ) +
    geom_segment(
      aes(
        x = ord,
        xend = ord,
        y = est + 0.01,
        yend = est - 0.01
      ),
      alpha = 0.8+0.2,
      color = "white",
      size = rel(0.06)
    ) +
    # geom_point(
    #   aes(x = ord,
    #       y = est),
    #   alpha = 0.7+0.3,
    #   shape = ".",
    #   color = "white",
    #   size = rel(0.06/3)
    # ) +
    geom_hline(yintercept = 0,
               linetype = "solid",
               size = rel(0.15),
               color = "black",
               alpha = 1) +
    theme_pubclean() +
    # from paletteer::paletteer_d("khroma::sunset")
    guides(colour = guide_legend(override.aes = list(size = rel(1),
                                                     alpha = 1))) +
    scale_color_manual(values =
                         c("#F67E4BFF",
                           "#98CAE1FF",
                           "#364B9AFF")) +
    facet_grid(rows = vars(TT),
               # rows = vars(rev(TT)),
               cols = vars(NN)) +
    ggtitle(title) +
    theme(
      legend.background = element_rect(colour = NA, fill = NA),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.title = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(), # "Score",
      # axis.title.y = "Density",
      axis.text.x = element_blank(),
      legend.key.width = unit(legend.key.width, "mm"),
      axis.text.y = element_text(size = 5),
      text = element_text(family = "CMU Serif",
                          size = 10)
      # plot.title =
      #   element_text(size = 10,
      #                family = "CMU Serif",
      #                hjust = 0)
    ) +
    xlab(NULL) + ylab(NULL)

}


caterpillar_podar_fixed <- plot_caterpillar(d,
                                            l2.dist = "Chi2",
                                            Model = "PoDAR",
                                            analysis.type = "resid.fixed")
caterpillar_podar_random <- plot_caterpillar(d,
                                             l2.dist = "Chi2",
                                             Model = "PoDAR",
                                             analysis.type = "resid.random")



p.patchwork <- caterpillar_podar_fixed / caterpillar_podar_random

p.final <- p.patchwork +
  plot_layout(guides = "collect") +
  plot_annotation(title = TeX("$95\\%$CIs for covariance of PoDAR(1) model with $\\chi^2$-distributed means"),
                  # subtitle = TeX("Point estimates and $95\\%$CIs for covariance"),
                  # subtitle = " ",
                  theme = theme(plot.title =
                                  element_text(size = 15,
                                               family = "CMU Serif",
                                               hjust = 0.5),
                                plot.subtitle =
                                  element_text(size = 12,
                                               family = "CMU Serif",
                                               hjust = 0.5))
  ) &
  theme(legend.position = "bottom")



ggsave("caterpillar-podar.pdf",
       p.final,
       width = 18,
       height = 24,
       units = "cm")
