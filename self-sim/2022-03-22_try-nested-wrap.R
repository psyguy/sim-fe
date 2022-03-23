### Trying nested wrapping



df <- data.frame(x = c(rnorm(1000,0),
                       rnorm(1000,2),
                       rnorm(1000,3)),
                 ga = rep(c("a1", "a2", "a3"),
                          each = 1000),
                 gb = rep(c("b1", "b2", "b3"),
                          each = 1000),
                 gc = rep(c("c1", "c2", "c3"),
                          each = 1000),
                 lab = rep(c("x", "y", "z"),
                           time = 1000)
                 )
# %>%
#   pivot_longer(lab)

plot <- df %>%
  # filter(lab == "z") %>%
  ggplot() +
  aes(fill = lab,
      group = lab) +
  geom_histogram(aes(x = x),
                 bins = 30) +
  theme_minimal() +
  # geom_vline(aes(xintercept = mean.est),
  #            linetype = "dashed") +
  theme_calc() +
  scale_fill_viridis_d(option = "plasma",
                       begin = 0.4,
                       end = 1) +
  facet_grid(rows = vars(ga),
             cols = vars(gb))

plot +
  plot_layout(guides = "collect") &
  facet_wrap(vars(gc))
  # +
  # plot_annotation(title = TeX(title),
  #                 subtitle = " ",
  #                 theme = theme(plot.title =
  #                                 element_text(size = 55,
  #                                              family = "CMU Serif",
  #                                              hjust = 0.5))
  # ) &
  # geom_point(size = 3.5, alpha = 1) &
  # geom_line(size = 0.5, alpha = 0.6) &
  # scale_color_viridis_d(direction = -1) &
  theme(legend.position = "bottom")


patchwork::wrap_plots(plot, ncol = 3)
