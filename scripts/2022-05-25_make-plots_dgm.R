##################################################################
## Here we make profile plots of sample simulated DGMs, including
## the time series plot, marginal distributions, and ACF plot
##################################################################

source("functions/functions_dgm-plots.R")


# Making color palettes ---------------------------------------------------


colors.nar.khaki <- c("khaki2",
                      "khaki3",
                      "khaki4")

colors.chiar.lightblue <- c("lightblue2",
                            "lightblue3",
                            "lightblue4")

colors.binar.darkolivegreen <- c("darkolivegreen2",
                                 "darkolivegreen3",
                                 "darkolivegreen4")

colors.podar.orchid <- c("orchid2",
                         "orchid3",
                         "orchid4")
## sample colors
# p.colors <- c("greenyellow",
#               "lightskyblue1",
#               "mediumorchid4")
#
# p.colors <- c("steelblue2",
#               "steelblue3",
#               "steelblue4")

# Simulating DGMs ---------------------------------------------------------


ts.length <- 500

d.3.nar <- make_dgm_df(dgm_nar(phi = 0.4,
                                var.resid = 20,
                                T = ts.length,
                                Mean = 85,
                                seed = 1),
                        dgm_nar(phi = 0.8,
                                var.resid = 20,
                                T = ts.length,
                                Mean = 55,
                                seed = 2+2),
                        dgm_nar(phi = 0.4,
                                var.resid = 47,
                                T = ts.length,
                                Mean = 20,
                                seed = 3+2)
                       )

profile_nar <- plot_dgm.profile(d.3.nar,
                                  colors.nar.khaki,
                                  "$AR(1)$")

  ggsave("Profile NAR.pdf",
       profile_nar,
       height = 10*2.2,
       width = 21*2,
       units = "cm")




d.3.chiar <- make_dgm_df(dgm_generator(Model = "ChiAR",
                                       phi = 0.4,
                                       nu = 25,
                                       T = ts.length,
                                       # Mean = 70,
                                       seed = 1),
                         dgm_generator(Model = "ChiAR",
                                       phi = 0.7,
                                       nu = 5,
                                       T = ts.length,
                                       # Mean = 50,
                                       seed = 2),
                         dgm_generator(Model = "ChiAR",
                                       phi = 0.4,
                                       nu = 1,
                                       T = ts.length,
                                       # Mean = 10,
                                       seed = 3)
                         )


profile_chiar <- plot_dgm.profile(d.3.chiar,
                                  colors.chiar.lightblue,
                                  "$\\chi^2AR(1)$")

ggsave("Profile Chi2AR.pdf",
       profile_chiar,
       height = 10*2.7,
       width = 21*2,
       units = "cm")



d.3.binar <- make_dgm_df(dgm_generator(Model = "BinAR",
                                       k = 7,
                                       alpha  = 0.85,
                                       phi = 0.45,
                                       # Mean = 5.5,
                                       T = ts.length,
                                       seed = 1+3),
                         dgm_generator(Model = "BinAR",
                                       k = 7,
                                       alpha = 0.85,
                                       phi = 0.7,
                                       # Mean = 3.5,
                                       T = ts.length,
                                       # Mean = 50,
                                       seed = 2),
                         dgm_generator(Model = "BinAR",
                                       k = 7,
                                       alpha = 0.5,
                                       phi = 0.45,
                                       # Mean = 0.6,
                                       T = ts.length,
                                       # Mean = 10,
                                       seed = 3+6)
                         )

profile_binar <- plot_dgm.profile(d.3.binar,
                                  colors.binar.darkolivegreen,
                                  "$BinAR(1)$")

ggsave("Profile BinAR.pdf",
       profile_binar,
       height = 10*2,
       width = 21*2,
       units = "cm")





d.3.podar <- make_dgm_df(dgm_generator(Model = "PoDAR",
                                       tau = 0.4,
                                       Mean = 40,
                                       T = ts.length,
                                       seed = 1+26),
                         dgm_generator(Model = "PoDAR",
                                       tau = 0.8,
                                       Mean = 10,
                                       T = ts.length,
                                       seed = 2+16),
                         dgm_generator(Model = "PoDAR",
                                       tau = 0.4,
                                       Mean = 1.,
                                       T = ts.length,
                                       seed = 3+13)
                         )


profile_podar <- plot_dgm.profile(d.3.podar,
                                  colors.podar.orchid,
                                  "$\\PoDAR(1)$")

ggsave("Profile PoDAR.pdf",
       profile_podar,
       height = 10*2,
       width = 21*2,
       units = "cm")


ggsave("Profile four DGMs.pdf",
       profile_nar /
       profile_chiar /
       profile_binar /
       profile_podar,
       height = 10*2*4,
       width = 21*2,
       units = "cm")



ggsave("Profile alternative DGMs.pdf",
       profile_chiar /
       profile_binar /
       profile_podar,
       height = 10*2*3,
       width = 21*2,
       units = "cm")

