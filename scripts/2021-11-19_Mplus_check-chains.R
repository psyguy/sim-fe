source("functions/functions_Mplus.R")

d_Gaus_BinAR <- readRDS("Mplus_check-chains/sim_l2.dist-Gaussian_Model-BinAR_N-100_phi-0.4_T-100_Rep-1_uSeed-13286.rds")
d_Gaus_Chi2AR <- readRDS("Mplus_check-chains/sim_l2.dist-Gaussian_Model-Chi2AR_N-100_phi-0.4_T-1000_Rep-1_uSeed-120397.rds")

f <- list()

Sys.time()

f$Gaus_BinAR_nPROC.1 <- run_MplusAutomation(d_Gaus_BinAR,
                                            PROCESSORS = 1,
                                            out.folder = "Mplus_check-chains/",
                                            file.name = "f_Gaus_BinAR_nPROC-1")

Sys.time()

f$Gaus_Chi2AR_nPROC.1 <- run_MplusAutomation(d_Gaus_Chi2AR,
                                            PROCESSORS = 1,
                                            out.folder = "Mplus_check-chains/",
                                            file.name = "f_Gaus_Chi2AR_nPROC-1")

Sys.time()

f$Gaus_BinAR_nPROC.2 <- run_MplusAutomation(d_Gaus_BinAR,
                                            PROCESSORS = 2,
                                            out.folder = "Mplus_check-chains/",
                                            file.name = "f_Gaus_BinAR_nPROC-2")

Sys.time()

f$Gaus_Chi2AR_nPROC.2 <- run_MplusAutomation(d_Gaus_Chi2AR,
                                             PROCESSORS = 2,
                                             out.folder = "Mplus_check-chains/",
                                             file.name = "f_Gaus_Chi2AR_nPROC-2")

Sys.time()

f$Gaus_BinAR_nPROC.4 <- run_MplusAutomation(d_Gaus_BinAR,
                                            PROCESSORS = 4,
                                            out.folder = "Mplus_check-chains/",
                                            file.name = "f_Gaus_BinAR_nPROC-4")

Sys.time()

f$Gaus_Chi2AR_nPROC.4 <- run_MplusAutomation(d_Gaus_Chi2AR,
                                             PROCESSORS = 4,
                                             out.folder = "Mplus_check-chains",
                                             file.name = "f_Gaus_Chi2AR_nPROC-4")

Sys.time()

f$Gaus_BinAR_nPROC.8 <- run_MplusAutomation(d_Gaus_BinAR,
                                            PROCESSORS = 8,
                                            out.folder = "Mplus_check-chains/",
                                            file.name = "f_Gaus_BinAR_nPROC-8")
Sys.time()

f$Gaus_Chi2AR_nPROC.8 <- run_MplusAutomation(d_Gaus_Chi2AR,
                                             PROCESSORS = 8,
                                             out.folder = "Mplus_check-chains/",
                                             file.name = "f_Gaus_Chi2AR_nPROC-8")
Sys.time()

