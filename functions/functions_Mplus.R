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
                                file.name = Sys.time()
){

  inp.name <- paste0(out.folder,
                     file.name)

  MplusAutomation::prepareMplusData(df,
                   paste0(inp.name, ".dat")
                   )

  model.ar1 <- MplusAutomation::mplusObject(
    TITLE = inp.name,
    rdata = df,
    usevariables = c("subject", "t", "x"),

    VARIABLE = "
    CLUSTER = subject;
    LAGGED = x(1);
    TINTERVAL = t(1);
  ",

    ANALYSIS = glue::glue("TYPE = TWOLEVEL RANDOM;
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


  # st <- Sys.time()
  # print(paste(inp.name, "started at", st))
  fit.ar1 <- MplusAutomation::mplusModeler(model.ar1,
                          check = FALSE,
                          modelout = paste0(inp.name, ".inp"),
                          hashfilename = FALSE,
                          run = 1L)
  # print(paste(inp.name, "took", Sys.time() - st))

  return(fit.ar1)

}

