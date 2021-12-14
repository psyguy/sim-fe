## @knitr methods

library(tidyverse)
library(MplusAutomation)

run_MplusAutomation <- function(df,
                                PROCESSORS = 2,
                                CHAINS = 2,
                                THIN = 2,
                                BITERATIONS.min = 2000,
                                BITERATIONS.max =
                                  BITERATIONS.min*BITERATION.minmax.factor,
                                BITERATION.minmax.factor = 2.5,
                                out.folder = "Mplus-files/",
                                file.prefix = "20211104"
){


  file.suffix <- sapply(out.folder,
                        function(dir){
                          length(list.files(dir,pattern='inp'))
                          }) %>%
    as.numeric()

  inp.name <- paste0(out.folder,
                     file.prefix,
                     # "_biterMin",
                     "_it-",
                     BITERATIONS.min,
                     # "_biterMax-",
                     # BITERATIONS.max,
                     "_th-",
                     THIN,
                     "_no-",
                     file.suffix+1)

  prepareMplusData(df,
                   # paste0(mpa_dataset$title, ".dat")
                   paste0(inp.name, ".dat")
  )

  model.ar1 <- mplusObject(
    TITLE = inp.name, #"meh",#mpa_dataset$title,
    rdata = df,
    # filename = "data.dat",
    usevariables = c("subject", "t", "x"),
    VARIABLE = "
	CLUSTER = subject;
	LAGGED = x(1);
	TINTERVAL = t(1);
  ",
    ANALYSIS = glue("TYPE = TWOLEVEL RANDOM;
	ESTIMATOR = BAYES;
	PROCESSORS = {PROCESSORS};
  CHAINS = {CHAINS};
  THIN = {THIN};
	BITERATIONS = {BITERATIONS.max}({BITERATIONS.min});"),
    MODEL = "
  %WITHIN%
	phi | x ON x&1;
	logv | x;
	%BETWEEN%
	x phi logv WITH x phi logv;",
    OUTPUT = "TECH1 TECH2 TECH3 TECH8 FSCOMPARISON STANDARDIZED STDYX STDY;",
    PLOT = "TYPE = PLOT3;
	FACTORS = ALL (500);")


  st <- Sys.time()
  print(paste(inp.name, "started at", st))
  fit.ar1 <- mplusModeler(model.ar1,
                          check = FALSE,
                          modelout = paste0(inp.name, ".inp"),
                          hashfilename = FALSE,
                          run = 1L)
  print(paste(inp.name, "took", Sys.time() - st))

  return(fit.ar1)

}

## iteration = 1000
mpa_it.1k_th.01 <- new_method("mpa_it.1k_th.01",
                              "Mplus analysis with BITERATIONS.min = 1000 and thin = 1",
                              method = function(model, draw){
                                run_MplusAutomation(df = draw,
                                                    BITERATIONS.min = 1000,
                                                    THIN = 1,
                                                    file.prefix =
                                                      paste0(model$Model,
                                                             "_N_", model$N,
                                                             "_T_", model$T)
                                )
                              })

mpa_it.1k_th.02 <- new_method("mpa_it.1k_th.02",
                              "Mplus analysis with BITERATIONS.min = 1000 and thin = 2",
                              method = function(model, draw){
                                run_MplusAutomation(df = draw,
                                                    BITERATIONS.min = 1000,
                                                    THIN = 2,
                                                    file.prefix =
                                                      paste0(model$Model,
                                                             "_N_", model$N,
                                                             "_T_", model$T)
                                )
                              })

mpa_it.1k_th.10 <- new_method("mpa_it.1k_th.10",
                              "Mplus analysis with BITERATIONS.min = 1000 and thin = 10",
                              method = function(model, draw){
                                run_MplusAutomation(df = draw,
                                                    BITERATIONS.min = 1000,
                                                    THIN = 10,
                                                    file.prefix =
                                                      paste0(model$Model,
                                                             "_N_", model$N,
                                                             "_T_", model$T)
                                )
                              })

mpa_it.1k_th.20 <- new_method("mpa_it.1k_th.20",
                              "Mplus analysis with BITERATIONS.min = 1000 and thin = 20",
                              method = function(model, draw){
                                run_MplusAutomation(df = draw,
                                                    BITERATIONS.min = 1000,
                                                    THIN = 20,
                                                    file.prefix =
                                                      paste0(model$Model,
                                                             "_N_", model$N,
                                                             "_T_", model$T)
                                )
                              })

## iteration = 2000

mpa_it.2k_th.01 <- new_method("mpa_it.2k_th.01",
                              "Mplus analysis with BITERATIONS.min = 2000 and thin = 1",
                              method = function(model, draw){
                                run_MplusAutomation(df = draw,
                                                    BITERATIONS.min = 2000,
                                                    THIN = 1,
                                                    file.prefix =
                                                      paste0(model$Model,
                                                             "_N_", model$N,
                                                             "_T_", model$T)
                                )
                              })

mpa_it.2k_th.02 <- new_method("mpa_it.2k_th.02",
                              "Mplus analysis with BITERATIONS.min = 2000 and thin = 2",
                              method = function(model, draw){
                                run_MplusAutomation(df = draw,
                                                    BITERATIONS.min = 2000,
                                                    THIN = 2,
                                                    file.prefix =
                                                      paste0(model$Model,
                                                             "_N_", model$N,
                                                             "_T_", model$T)
                                )
                              })

mpa_it.2k_th.10 <- new_method("mpa_it.2k_th.10",
                              "Mplus analysis with BITERATIONS.min = 2000 and thin = 10",
                              method = function(model, draw){
                                run_MplusAutomation(df = draw,
                                                    BITERATIONS.min = 2000,
                                                    THIN = 10,
                                                    file.prefix =
                                                      paste0(model$Model,
                                                             "_N_", model$N,
                                                             "_T_", model$T)
                                )
                              })

mpa_it.2k_th.20 <- new_method("mpa_it.2k_th.20",
                              "Mplus analysis with BITERATIONS.min = 2000 and thin = 20",
                              method = function(model, draw){
                                run_MplusAutomation(df = draw,
                                                    BITERATIONS.min = 2000,
                                                    THIN = 20,
                                                    file.prefix =
                                                      paste0(model$Model,
                                                             "_N_", model$N,
                                                             "_T_", model$T)
                                )
                              })

## iteration = 5000
mpa_it.5k_th.01 <- new_method("mpa_it.5k_th.01",
                              "Mplus analysis with BITERATIONS.min = 5000 and thin = 1",
                              method = function(model, draw){
                                run_MplusAutomation(df = draw,
                                                    BITERATIONS.min = 5000,
                                                    THIN = 1,
                                                    file.prefix =
                                                      paste0(model$Model,
                                                             "_N_", model$N,
                                                             "_T_", model$T)
                                )
                              })

mpa_it.5k_th.02 <- new_method("mpa_it.5k_th.02",
                              "Mplus analysis with BITERATIONS.min = 5000 and thin = 2",
                              method = function(model, draw){
                                run_MplusAutomation(df = draw,
                                                    BITERATIONS.min = 5000,
                                                    THIN = 2,
                                                    file.prefix =
                                                      paste0(model$Model,
                                                             "_N_", model$N,
                                                             "_T_", model$T)
                                )
                              })

mpa_it.5k_th.10 <- new_method("mpa_it.5k_th.10",
                              "Mplus analysis with BITERATIONS.min = 5000 and thin = 10",
                              method = function(model, draw){
                                run_MplusAutomation(df = draw,
                                                    BITERATIONS.min = 5000,
                                                    THIN = 10,
                                                    file.prefix =
                                                      paste0(model$Model,
                                                             "_N_", model$N,
                                                             "_T_", model$T)
                                )
                              })

mpa_it.5k_th.20 <- new_method("mpa_it.5k_th.20",
                              "Mplus analysis with BITERATIONS.min = 5000 and thin = 20",
                              method = function(model, draw){
                                run_MplusAutomation(df = draw,
                                                    BITERATIONS.min = 5000,
                                                    THIN = 20,
                                                    file.prefix =
                                                      paste0(model$Model,
                                                             "_N_", model$N,
                                                             "_T_", model$T)
                                )
                              })

