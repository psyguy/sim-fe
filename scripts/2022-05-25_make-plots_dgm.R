##################################################################
## Here we make profile plots of sample simulated DGMs, including
## the time series plot, marginal distributions, and ACF plot
##################################################################

source("functions/functions_dgm-plots.R")
library(RColorBrewer)

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


ts.length <- 500*10

d.3.nar <- make_dgm_df(dgm_nar(phi = 0.4,
                                var.resid = 20,
                                T = ts.length,
                                Mean = 85,
                                seed = 1+9),
                        dgm_nar(phi = 0.8,
                                var.resid = 20,
                                T = ts.length,
                                Mean = 55,
                                seed = 2+4),
                        dgm_nar(phi = 0.4,
                                var.resid = 47,
                                T = ts.length,
                                Mean = 20,
                                seed = 3+2)
                       )


profile_nar <- plot_dgm.profile(d.3.nar,
                                brewer.pal(name = "YlOrBr", n = 9)[c(5, 7, 9)],
                                "$AR(1)$")


ggsave(
  "Profile NAR.pdf",
  profile_nar,
  height = 10*1.8,
  width = 21 * 2,
  units = "cm"
)




d.3.chiar <- make_dgm_df(dgm_generator(Model = "ChiAR",
                                       phi = 0.4,
                                       nu = 25,
                                       T = ts.length,
                                       # Mean = 70,
                                       seed = 1+1),
                         dgm_generator(Model = "ChiAR",
                                       phi = 0.7,
                                       nu = 5,
                                       T = ts.length,
                                       # Mean = 50,
                                       seed = 2+5),
                         dgm_generator(Model = "ChiAR",
                                       phi = 0.4,
                                       nu = 1,
                                       T = ts.length,
                                       # Mean = 10,
                                       seed = 3+1)
                         )


profile_chiar <- plot_dgm.profile(d.3.chiar,
                                  brewer.pal(name = "GnBu", n = 9)[c(5, 7, 9)],
                                  "$\\chi^2AR(1)$")

ggsave("Profile Chi2AR.pdf",
       profile_chiar,
       height = 10*1.8,
       width = 21*2,
       units = "cm")



d.3.binar <- make_dgm_df(dgm_generator(Model = "BinAR",
                                       k = 7,
                                       alpha  = 0.85,
                                       phi = 0.45,
                                       # Mean = 5.5,
                                       T = ts.length,
                                       seed = 1+5),
                         dgm_generator(Model = "BinAR",
                                       k = 7,
                                       alpha = 0.85,
                                       phi = 0.7,
                                       # Mean = 3.5,
                                       T = ts.length,
                                       # Mean = 50,
                                       seed = 2+6),
                         dgm_generator(Model = "BinAR",
                                       k = 7,
                                       alpha = 0.5,
                                       phi = 0.45,
                                       # Mean = 0.6,
                                       T = ts.length,
                                       # Mean = 10,
                                       seed = 3)
                         )

profile_binar <- plot_dgm.profile(d.3.binar,
                                  brewer.pal(name = "YlGn", n = 9)[c(5, 7, 9)],
                                  "$BinAR(1)$")

ggsave("Profile BinAR.pdf",
       profile_binar,
       height = 10*1.8,
       width = 21*2,
       units = "cm")





d.3.podar <- make_dgm_df(dgm_generator(Model = "PoDAR",
                                       tau = 0.4,
                                       Mean = 40,
                                       T = ts.length,
                                       seed = 1+4),
                         dgm_generator(Model = "PoDAR",
                                       tau = 0.8,
                                       Mean = 10,
                                       T = ts.length,
                                       seed = 2+4),
                         dgm_generator(Model = "PoDAR",
                                       tau = 0.4,
                                       Mean = 1.,
                                       T = ts.length,
                                       seed = 3+2)
                         )


profile_podar <- plot_dgm.profile(d.3.podar,
                                  brewer.pal(name = "BuPu", n = 9)[c(5, 7, 9)],
                                  "$\\PoDAR(1)$")

ggsave("Profile PoDAR.pdf",
       profile_podar,
       height = 10*1.8,
       width = 21*2,
       units = "cm")


ggsave("Profile four DGMs.pdf",
       profile_nar /
       profile_chiar /
       profile_binar /
       profile_podar,
       height = 10*1.8*4,
       width = 21*2,
       units = "cm")



ggsave("Profile alternative DGMs.pdf",
       profile_chiar /
       profile_binar /
       profile_podar,
       height = 10*1.8*3,
       width = 21*2,
       units = "cm")

