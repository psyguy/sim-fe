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


plot_caterpillar <- function(d,
                             l2.dist_ = "Chi2",
                             Model_ = "PoDAR",
                             analysis.type = "resid.fixed",
                             parameter = "covariance",
                             legend.key.width = 5,
                             legend.line.width = 10) {

  title <- ifelse(analysis.type == "resid.fixed",
                  "fixed residual variance",
                  "random residual variance") %>%
    paste("Modeled with", .)

  dd <- d %>%
    filter(l2.dist == l2.dist_,
           Model == Model_)
    # filter(N == 100,
    #        T == 100) %>%
  y.range <- c(min(dd$lower_2.5ci), max(dd$upper_2.5ci))

  dd %>%
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
        y = est + min(0.01, 0.05*abs(upper_2.5ci - lower_2.5ci)),
        yend = est - min(0.01, 0.05*abs(upper_2.5ci - lower_2.5ci))
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
               alpha = 0.8) +
    theme_pubclean() +
    # from paletteer::paletteer_d("khroma::sunset")
    guides(colour = guide_legend(override.aes = list(linewidth = rel(1.5),
                                                     alpha = 1))) +
    scale_color_manual(values =
                         c("#F67E4BFF",
                           "#98CAE1FF",
                           "#364B9AFF")) +
    scale_y_continuous(limits = y.range) +
    facet_grid(rows = vars(NN),
               # rows = vars(rev(TT)),
               cols = vars(TT)) +
    ggtitle(title) +
    theme(
      legend.background = element_rect(colour = NA, fill = NA),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.title = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(), # "Score",
      axis.text.x = element_blank(),
      legend.key.width = unit(legend.key.width*2, "mm"),
      axis.text.y = element_text(size = 10),
      text = element_text(family = "CMU Serif",
                          size = 12)
      # plot.title =
      #   element_text(size = 10,
      #                family = "CMU Serif",
      #                hjust = 0)
    ) +
    xlab(NULL) +
    ylab(paste("Estimated", parameter))

}



# Producing the caterpillar plots -----------------------------------------

## Choosing which condition to plot

single_model <- "NAR"
single_model <- "Chi2AR"
single_model <- "BinAR"
single_model <- "PoDAR"

single_model_name <- ifelse(single_model == "Chiar",
                            "$\\chi^2$AR(1)",
                            paste0(single_model, "(1)"))


single_l2.dist <- "Gaussian"
single_l2.dist <- "Chi2"

single_l2.dist_name <- ifelse(single_l2.dist == "Chi2",
                              "$\\chi^2$",
                              single_l2.dist)


model_type <- "resid.fixed"
model_type <- "resid.random"


## Making plots

## Choose whether to have one or two 9-panel caterpillar plot in one sheet

caterpillar_fixed <- plot_caterpillar(d,
                                      l2.dist = single_l2.dist,
                                      Model = single_model,
                                      analysis.type = "resid.fixed")
caterpillar_random <- plot_caterpillar(d,
                                       l2.dist = single_l2.dist,
                                       Model = single_model,
                                       analysis.type = "resid.random")

p.patchwork <- caterpillar_fixed / caterpillar_random


caterpillar_single <- plot_caterpillar(d,
                                       l2.dist = single_l2.dist,
                                       Model = single_model,
                                       analysis.type = model_type)


# p.patchwork <- caterpillar_single


p.final <- p.patchwork +
  plot_layout(guides = "collect") +
  plot_annotation(##
                  ## removed the big title and subtitle  here
                  ##
                  # title = TeX(paste0("$95\\%$CIs for covariance of the ",
                  #                    single_model_name,
                  #                    " model with ",
                  #                    single_l2.dist_name,
                  #                    "-distributed means")),
                  # subtitle = TeX("Point estimates and $95\\%$CIs for covariance"),

                  theme = theme(plot.title =
                                  element_text(size = 12,
                                               family = "CMU Serif",
                                               hjust = 0.5),
                                plot.subtitle =
                                  element_text(size = 12,
                                               family = "CMU Serif",
                                               hjust = 0.5))
  ) &
  theme(legend.position = "bottom")
# &
#   guides(color = guide_legend(override.aes = list(linewidth = rel(2),
#                                                    alpha = 1)))
#
p.final

ggsave(paste0("caterpillar-",
              single_model,
              "-",
              single_l2.dist,
              "-",
              "both",
              # str_replace(model_type, "resid.", ""),
              ".pdf"),
       p.final,
       width = 18,
       height = 18, #24,
       units = "cm")



# -------------------------------------------------------------------------


list_model <- c("NAR",
                "Chi2AR",
                "BinAR",
                "PoDAR")

list_l2.dist <- c("Gaussian",
                  "Chi2")

list_model.type <- c("resid.fixed",
                     "resid.random")

cross2(list_model,
       list_l2.dist) %>%
  plyr::l_ply(function(x){
    single_model <- x[1]
    single_model_name <- ifelse(single_model == "Chiar",
                                "$\\chi^2$AR(1)",
                                paste0(single_model, "(1)"))

    single_l2.dist <- x[2]
    single_l2.dist_name <- ifelse(single_l2.dist == "Chi2",
                                  "$\\chi^2$",
                                  single_l2.dist)

    caterpillar_fixed <- plot_caterpillar(d,
                                          l2.dist = single_l2.dist,
                                          Model = single_model,
                                          analysis.type = "resid.fixed")
    caterpillar_random <- plot_caterpillar(d,
                                           l2.dist = single_l2.dist,
                                           Model = single_model,
                                           analysis.type = "resid.random")

    p.patchwork <- caterpillar_fixed / caterpillar_random

    p.final <- p.patchwork +
      plot_layout(guides = "collect") +
      plot_annotation(
        theme = theme(plot.title =
                        element_text(size = 12,
                                     family = "CMU Serif",
                                     hjust = 0.5),
                      plot.subtitle =
                        element_text(size = 12,
                                     family = "CMU Serif",
                                     hjust = 0.5))
      ) &
      theme(legend.position = "bottom")


    ggsave(paste0("caterpillar-",
                  single_model,
                  "-",
                  single_l2.dist,
                  "-",
                  "both",
                  # str_replace(model_type, "resid.", ""),
                  ".pdf"),
           p.final,
           width = 18,
           height = 18, #24,
           units = "cm")

  })


cross3(list_model,
       list_l2.dist,
       list_model.type) %>%
  plyr::l_ply(function(x){
    single_model <- x[1]
    single_model_name <- ifelse(single_model == "Chiar",
                                "$\\chi^2$AR(1)",
                                paste0(single_model, "(1)"))

    single_l2.dist <- x[2]
    single_l2.dist_name <- ifelse(single_l2.dist == "Chi2",
                                  "$\\chi^2$",
                                  single_l2.dist)

    model_type <- x[3]

    caterpillar_single <- plot_caterpillar(d,
                                           l2.dist = single_l2.dist,
                                           Model = single_model,
                                           analysis.type = model_type)

    p.final <- caterpillar_single +
      plot_layout(guides = "collect") +
      plot_annotation(
        theme = theme(plot.title =
                        element_text(size = 12,
                                     family = "CMU Serif",
                                     hjust = 0.5),
                      plot.subtitle =
                        element_text(size = 12,
                                     family = "CMU Serif",
                                     hjust = 0.5))
      ) &
      theme(legend.position = "bottom")


    ggsave(paste0("caterpillar-",
                  single_model,
                  "-",
                  single_l2.dist,
                  "-",
                  model_type,
                  # str_replace(model_type, "resid.", ""),
                  ".pdf"),
           p.final,
           width = 18,
           height = 11, #24,
           units = "cm")

  })

