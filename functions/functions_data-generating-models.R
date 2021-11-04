#' ---
#' title: "Data generating models"
#' author: "Manuel Haqiqatkhah"
#' date: "2021-10-22"
#' output: github_document
#' ---
#'
#' (Copied from v.2021-10-22 of `simulation_generic-dgm.R` from a previous.)
#'
#' We need a generic function that can generate desired properties via
#' a unified interface.
#' We should be able to
#'
#' 1. Choose a data generation mechanism
#'   - model name
#'   - maximum value on scale
#' 2. Choose desired theoretical properties:
#'   - mean
#'   - variance (perhaps non necessary)
#'   - skewness
#'   - autocorrelation effect
#'   - length of the time series
#' 3. Get empirical/implied estimates for the theoretical parameters
#'   - model-specific parameters
#'   - estimated mean, variance, skewness, and ACF(1)
#'   - deviation of the estimated parameters from the theoretical values
#'


#+ initialization
library(librarian)
shelf(tidyverse)
shelf(latex2exp)
shelf(moments)
shelf(cowplot)
shelf(grid)

# library(tidyverse)
# library(latex2exp)
# library(moments)
# library(cowplot)

# rm(list = ls())


#' We define functions to implement DGMs with original parameterization
#'

#+ chiAR
dgm_chiar <- function(...){

  pa <- list(...)

  if(is.list(pa$pa)) pa <- pa$pa

  ## setting default seed if not given
  if(is.null(pa$phi)) pa$phi <- 0.4
  if(is.null(pa$nu)) pa$nu <- 3
  if(is.null(pa$c)) pa$c <- 0
  if(is.null(pa$k)) pa$k <- 100
  if(is.null(pa$T)) pa$T <- 100
  if(is.null(pa$seed)) pa$seed <- 0

  ## drawing the first sample x_1
  set.seed(pa$seed)
  x = rep(NA, pa$T)
  x[1] <- rchisq(n = 1,
                 df = pa$Mean)

  ## making the rest of th time series
  for (t in 2:pa$T){

    x[t] <- pa$c + pa$phi*x[t-1] + rchisq(n = 1,
                                          df = pa$nu)
  }

  ## quick output of raw time series without book-keeping variables/parameters
  if(!is.null(pa$only.ts) & pa$only.ts==TRUE) return(x)

  Empirical.Parameters = list(Mean = mean(x),
                              Variance = var(x),
                              Skewness = moments::skewness(x),
                              AR = acf(x, lag.max = 1, plot = FALSE)$acf[2]
  )

  ## making a LaTeX-ready list description of the model
  Model.Description <- paste0("$\\chi^2AR(1):", # \\; with",
                              "\\; \\mu = ",
                              round(pa$Mean,2),
                              "(",
                              round(Empirical.Parameters$Mean,2),
                              ")",
                              ",\\; \\gamma = ",
                              round(pa$Skewness,2),
                              "(",
                              round(Empirical.Parameters$Skewness,2),
                              ")",
                              ",\\; \\phi = ",
                              round(pa$phi,2),
                              "(",
                              round(Empirical.Parameters$AR,2),
                              ")",
                              ",\\; \\nu = ",
                              round(pa$nu,2),
                              ",\\; c = ",
                              round(pa$c,3),
                              ",\\; T = ",
                              pa$T,
                              "$")

  ## making the output object
  output <- list(x = x,
                 Model.Description = Model.Description,
                 Model.Parameters = pa,
                 Empirical.Parameters = Empirical.Parameters)

  return(output)
}


#+ markovChain
dgm_markov.chain <- function(trans.mat, N, seed = 0) {

  transita <- function(state,
                       trans.mat) {
    sample(rownames(trans.mat),
           1,
           prob = trans.mat[,state])
  }

  p.marginal <- Re(eigen(trans.mat)$vectors[, 1])
  p.marginal <- p.marginal / sum(p.marginal)

  sim <- character(N)
  set.seed(seed)
  sim[1] <- sample(rownames(trans.mat),
                   1,
                   prob = p.marginal)
  for (i in 2:N) {
    sim[i] <- transita(sim[i-1],
                       trans.mat)
  }
  return(sim)
}



#+ twopart
dgm_zeroinflated <- function(...){

  pa <- list(...)

  if(is.list(pa$pa)) pa <- pa$pa

  ## setting default seed if not given
  if(is.null(pa$phi)) pa$phi <- 0.4
  if(is.null(pa$p.remain.off)) pa$p.remain.off <- 0.3
  if(is.null(pa$p.remain.on)) pa$p.remain.on <- 0.8
  if(is.null(pa$mu.on)) pa$mu.on <- 30
  if(is.null(pa$var.on)) pa$var.on <- 5
  if(is.null(pa$c)) pa$c <- pa$c <- pa$mu.on * (1 - pa$phi)
  if(is.null(pa$k)) pa$k <- 100
  if(is.null(pa$T)) pa$T <- 100
  if(is.null(pa$seed)) pa$seed <- 0


  #C from mean formula
  # pa$c <- pa$mu.on * (1 - pa$phi)

  s00 <- pa$p.remain.off
  s01 <- 1- s00
  s11 <- pa$p.remain.on
  s10 <- 1 - s11

  trans.mat <- matrix(c(s00, s01, s10, s11),
                      nrow = 2)

  colnames(trans.mat) <-
    rownames(trans.mat) <-
    c("off", "on")

  states <- dgm_markov.chain(trans.mat, pa$T, pa$seed)


  pa$Mean <- sum(states=="on")*pa$mu.on/pa$T

  ## drawing the first sample x_1
  set.seed(pa$seed)
  x = rep(NA, pa$T)
  x[1] <- ifelse(states[1] == "off",
                 0,
                 rnorm(1,
                       pa$mu.on,
                       sqrt(pa$var.on / (1 - pa$phi ^ 2))
                 )
  )
  ## making the rest of th time series
  for (t in 2:pa$T){

    x[t] <- ifelse(states[t]=="off",
                   0,
                   pa$c + pa$phi*x[t-1] + rnorm(1,
                                                0,
                                                sqrt(pa$var.on)
                   )
    )
  }

  ## quick output of raw time series without book-keeping variables/parameters
  if(!is.null(pa$only.ts) & pa$only.ts==TRUE) return(x)

  Empirical.Parameters = list(Mean = mean(x),
                              Variance = var(x),
                              Skewness = moments::skewness(x),
                              AR = acf(x, lag.max = 1, plot = FALSE)$acf[2]
  )

  ## making a LaTeX-ready list description of the model
  Model.Description <- paste0("$ZI-AR(1):", # \\; with",
                              "\\; \\mu = ",
                              round(pa$Mean,2),
                              "(",
                              round(Empirical.Parameters$Mean,2),
                              ")",
                              ",\\; \\gamma = ",
                              # round(pa$Skewness,2),
                              "(",
                              round(Empirical.Parameters$Skewness,2),
                              ")",
                              ",\\; \\phi = ",
                              round(pa$phi,2),
                              "(",
                              round(Empirical.Parameters$AR,2),
                              ")",
                              # ",\\; \\nu = ",
                              # round(pa$nu,2),
                              ",\\; c = ",
                              round(pa$c,3),
                              ",\\; T = ",
                              pa$T,
                              "$")

  ## making the output object
  output <- list(x = x,
                 Model.Description = Model.Description,
                 Model.Parameters = pa,
                 Empirical.Parameters = Empirical.Parameters)

  return(output)
}

#+ BinAR1
dgm_binar <- function(...){

  pa <- list(...)

  if(is.list(pa$pa)) pa <- pa$pa

  ## setting default seed if not given
  if(is.null(pa$alpha)) pa$alpha <- 0.5
  if(is.null(pa$beta)) pa$beta <- 0.4
  if(is.null(pa$k)) pa$k <- 6
  if(is.null(pa$T)) pa$T <- 100
  if(is.null(pa$seed)) pa$seed <- 0

  ## making other parameters
  pa$rho <- pa$alpha - pa$beta
  pa$theta <- pa$beta/(1-pa$rho)


  ## drawing the first sample x_1
  set.seed(pa$seed)
  x = rep(NA, pa$T)
  x[1] <- rbinom(n = 1,
                 size = pa$k ,
                 prob = pa$theta)

  ## making the rest of th time series
  for (t in 2:pa$T){
    S_t <- rbinom(n = 1,
                  size = x[t-1],
                  prob = pa$alpha)
    R_t <- rbinom(n = 1,
                  size = pa$k - x[t-1],
                  prob = pa$beta)
    x[t] <- S_t + R_t
  }

  ## quick output of raw time series without book-keeping variables/parameters
  if(!is.null(pa$only.ts) & pa$only.ts==TRUE) return(x)

  Empirical.Parameters = list(Mean = mean(x),
                              Variance = var(x),
                              Skewness = moments::skewness(x),
                              AR = acf(x, lag.max = 1, plot = FALSE)$acf[2]
  )

  ## making a LaTeX-ready list description of the model
  Model.Description <- paste0("$BinAR(1):",
                              "\\; \\mu = ",
                              round(pa$Mean,2),
                              "(",
                              round(Empirical.Parameters$Mean,2),
                              ")",
                              ",\\; \\gamma = ",
                              round(pa$Skewness,2),
                              "(",
                              round(Empirical.Parameters$Skewness,2),
                              ")",
                              ",\\; \\rho = ",
                              round(pa$rho,3),
                              "(",
                              round(Empirical.Parameters$AR,2),
                              ")",
                              ",\\; \\alpha = ",
                              round(pa$alpha,2),
                              ",\\; \\beta = ",
                              round(pa$beta,2),
                              ",\\; \\theta = ",
                              round(pa$theta,3),
                              ",\\; T = ",
                              pa$T,
                              "$")

  ## making the output object
  output <- list(x = x,
                 Model.Description = Model.Description,
                 Model.Parameters = pa,
                 Empirical.Parameters = Empirical.Parameters
  )

  return(output)
}

#+ DAR1
dgm_dar <- function(...){

  pa <- list(...)

  if(is.list(pa$pa)) pa <- pa$pa

  ## setting default seed if not given
  if(is.null(pa$tau)) pa$tau <- 0.7
  if(is.null(pa$theta)) pa$theta <- 0.5
  if(is.null(pa$k)) pa$k <- 6
  if(is.null(pa$T)) pa$T <- 100
  if(is.null(pa$seed)) pa$seed <- 0


  ## drawing the first sample x_1
  set.seed(pa$seed)
  x = rep(NA, pa$T)
  x[1] <- rbinom(n = 1,
                 size = pa$k ,
                 prob = pa$theta)

  ## making the rest of th time series
  for (t in 2:pa$T){
    V_t <- rbinom(n = 1,
                  size = 1,
                  prob = pa$tau)
    Z_t <- rbinom(n = 1,
                  size = pa$k,
                  prob = pa$theta)
    x[t] <- V_t*x[t-1] + (1-V_t)*Z_t
  }

  ## quick output of raw time series without book-keeping variables/parameters
  if(!is.null(pa$only.ts) & pa$only.ts==TRUE) return(x)

  Empirical.Parameters = list(Mean = mean(x),
                              Variance = var(x),
                              Skewness = moments::skewness(x),
                              AR = acf(x, lag.max = 1, plot = FALSE)$acf[2]
  )

  ## making a LaTeX-ready list description of the model
  Model.Description <- paste0("$DAR(1):",
                              "\\; \\mu = ",
                              round(pa$Mean,2),
                              "(",
                              round(Empirical.Parameters$Mean,2),
                              ")",
                              ",\\; \\gamma = ",
                              round(pa$Skewness,2),
                              "(",
                              round(Empirical.Parameters$Skewness,2),
                              ")",
                              "\\; \\tau = ",
                              round(pa$tau,3),
                              "(",
                              round(Empirical.Parameters$AR,2),
                              ")",
                              ",\\; \\theta = ",
                              round(pa$theta,3),
                              ",\\; T = ",
                              pa$T,
                              "$")


  ## making the output object
  output <- list(x = x,
                 Model.Description = Model.Description,
                 Model.Parameters = pa,
                 Empirical.Parameters = Empirical.Parameters
  )

  return(output)
}

#' Now it is time to make a generic time series generator that,
#' given desired mean and skewness, calculates corresponding
#' model parameters and generates time series with `dgm_` functions.
#'
#' Before that, we need another function that can calculate model parameters
#' based on what it is given
#'

#+ dgmParameterCalculator

dgm_parameterizer <- function(...){

  pa <- list(...)

  if(is.list(pa$pa)) pa <- pa$pa

  if(is.null(pa$Model)) pa$Model <- "ChiAR(1)"
  if(is.null(pa$phi)) pa$phi <- 0.2

  ## %%%%%%%%%%%%
  ## for ChiAR(1)
  ## %%%%%%%%%%%%

  if(tolower(pa$Model) == "chiar(1)" | tolower(pa$Model) == "chiar" |
     tolower(pa$Model) == "chi2ar(1)" | tolower(pa$Model) == "chi2ar"){

    if(is.null(pa$k)) pa$k <- 100

    ## Calculating model parameters
    ## if mean and skewness are given
    if (!is.null(pa$Mean) & !is.null(pa$Skewness)) {
      # from the skewness formula
      pa$nu <-
        8 * (1 - pa$phi ^ 2) ^ 3 / ((pa$Skewness ^ 2) * (1 - pa$phi ^
                                                           3) ^ 2)
      # then from the mean formula
      pa$c <- pa$Mean * (1 - pa$phi) - pa$nu
      # returning the parameter list
      return(pa)
    }
    ## if mean and skewness are not given at the same time, we use c
    ## and one other parameter
    else{

      # set intercept to zero, if already not defined
      if(is.null(pa$c)) pa$c <- 0

      ## if mean is given
      if (!is.null(pa$Mean)) {
        # then from the mean formula
        pa$nu <- pa$Mean * (1 - pa$phi) - pa$c
        # from the skewness formula
        pa$Skewness <-
          2 * (1 - pa$phi ^ 2) ^ 1.5 / (sqrt(pa$nu / 2) * (1 - pa$phi ^
                                                             3))
        # returning the parameter list
        return(pa)
      }

      ## if skewness is given
      if (!is.null(pa$Skewness)) {
        # from the skewness formula
        pa$nu <-
          8 * (1 - pa$phi ^ 2) ^ 3 / ((pa$Skewness ^ 2) * (1 - pa$phi ^
                                                             3) ^ 2)
        # then from the mean formula
        pa$Mean <- (pa$c + pa$nu) / (1 - pa$phi)
        # returning the parameter list
        return(pa)
      }

      ## if nu is given
      if(!is.null(pa$nu)){
        # from the mean formula
        pa$Mean <- (pa$c + pa$nu)/(1-pa$phi)
        # from the skewness formula
        pa$Skewness <-
          2 * (1 - pa$phi ^ 2) ^ 1.5 / (sqrt(pa$nu / 2) * (1 - pa$phi ^
                                                             3))
        # returning the parameter list
        return(pa)
      }


    }


  }


  ## %%%%%%%%%%%%
  ## for BinAR(1)
  ## %%%%%%%%%%%%

  if (tolower(pa$Model) == "binar(1)" | tolower(pa$Model) == "binar") {

    if (is.null(pa$k))
      pa$k <- 10

    ## Calculating model parameters

    ## if mean is given
    if (!is.null(pa$Mean)) {
      # from skewness formula: m = k*theta
      pa$theta <- pa$Mean / pa$k
      ## we then calculate skewness based on theta
      pa$Skewness <- (1 - 2 * pa$theta) / sqrt(pa$k * pa$theta * (1 - pa$theta))
      ## we then calculate beta based on theta and phi
      pa$beta <- pa$theta * (1 - pa$phi)
      # then we calculate alpha
      pa$alpha <- pa$phi + pa$beta
      # finally we calculate k*beta, which is equivalent to c
      pa$c <- pa$k * pa$beta
      # returning the parameter list
      return(pa)
    }

    ## if skewness is given
    if (!is.null(pa$Skewness)) {
      # from skewness formula: skewness = (1-2*theta)/sqrt(k*theta*(1-theta))
      # it is easier to write with ks = k*(skewness^2)
      ks <- pa$k*pa$Skewness^2
      pa$theta <- (ks + 4 -
                     sqrt(ks * (ks + 4))) / (2 * ks + 8)
      # The above formula only gives theta < 0.5, thus for negative skewness
      # we must use 1-theta instead
      if(pa$Skewness < 0) pa$theta <- 1 - pa$theta
      # we then calculate mean based on theta
      pa$Mean <- pa$k * pa$theta
      # we then calculate beta based on theta and phi
      pa$beta <- pa$theta * (1 - pa$phi)
      # then we calculate alpha
      pa$alpha <- pa$phi + pa$beta
      # finally we calculate k*beta, which is equivalent to c
      pa$c <- pa$k * pa$beta
      # returning the parameter list
      return(pa)
    }

    ## if theta is given
    if (!is.null(pa$theta)) {
      # from theta formula
      pa$beta <- pa$theta * (1 - pa$phi)
      # from beta formula
      pa$alpha <- pa$phi + pa$beta
      # we then calculate mean based on theta
      pa$Mean <- pa$k * pa$theta
      # we then calculate skewness based on theta
      pa$Skewness <- (1 - 2 * pa$theta) / sqrt(pa$k * pa$theta * (1 - pa$theta))
      # finally we calculate k*beta, which is equivalent to c
      pa$c <- pa$k * pa$beta
      # returning the parameter list
      return(pa)
    }

    ## if alpha is given
    if (!is.null(pa$alpha)) {
      # from beta formula
      pa$beta <- pa$alpha - pa$phi
      # from theta formula
      pa$theta <- pa$beta / (1 - pa$phi)
      # we then calculate mean based on theta
      pa$Mean <- pa$k * pa$theta
      # we then calculate skewness based on theta
      pa$Skewness <- (1 - 2 * pa$theta) / sqrt(pa$k * pa$theta * (1 - pa$theta))
      # finally we calculate k*beta, which is equivalent to c
      pa$c <- pa$k * pa$beta
      # returning the parameter list
      return(pa)
    }

    ## if beta is given
    if (!is.null(pa$beta)) {
      # from beta formula
      pa$alpha <- pa$phi + pa$beta
      # from theta formula
      pa$theta <- pa$beta / (1 - pa$phi)
      # we then calculate mean based on theta
      pa$Mean <- pa$k * pa$theta
      # we then calculate skewness based on theta
      pa$Skewness <- (1 - 2 * pa$theta) / sqrt(pa$k * pa$theta * (1 - pa$theta))
      # finally we calculate k*beta, which is equivalent to c
      pa$c <- pa$k * pa$beta
      # returning the parameter list
      return(pa)
    }

    ## if intercept given
    if (!is.null(pa$c)) {
      # from intercept formula c = k*beta
      pa$beta <- pa$c / pa$k
      # from beta formula
      pa$alpha <- pa$phi + pa$beta
      # from theta formula
      pa$theta <- pa$beta / (1 - pa$phi)
      # we then calculate mean based on theta
      pa$Mean <- pa$k * pa$theta
      # we then calculate skewness based on theta
      pa$Skewness <- (1 - 2 * pa$theta) / sqrt(pa$k * pa$theta * (1 - pa$theta))
      # returning the parameter list
      return(pa)
    }

  }

  ## %%%%%%%%%%%%
  ## for DAR(1)
  ## %%%%%%%%%%%%

  if (tolower(pa$Model) == "dar(1)" | tolower(pa$Model) == "dar") {

    if (is.null(pa$k))
      pa$k <- 10

    # phi and tau are the same, then if tau is defined, it overrules phi
    if(!is.null(pa$tau)) pa$phi <- pa$tau
    # and if tau is not defined, then tau will get the value of phi
    if(is.null(pa$tau)) pa$tau <- pa$phi
    # DAR(1) intercept is zero
    pa$c <- 0

    ## Calculating model parameters

    ## if mean is given
    if (!is.null(pa$Mean)) {
      # from skewness formula: m = k*theta
      pa$theta <- pa$Mean / pa$k
      ## we then calculate skewness based on theta
      pa$Skewness <- (1 - 2 * pa$theta) / sqrt(pa$k * pa$theta * (1 - pa$theta))
      # returning the parameter list
      return(pa)
    }

    ## if skewness is given
    if (!is.null(pa$Skewness)) {
      # from skewness formula: skewness = (1-2*theta)/sqrt(k*theta*(1-theta))
      # it is easier to write with ks = k*(skewness^2)
      ks <- pa$k*pa$Skewness^2
      pa$theta <- (ks + 4 -
                     sqrt(ks * (ks + 4))) / (2 * ks + 8)
      # The above formula only gives theta < 0.5, thus for negative skewness
      # we must use 1-theta instead
      if(pa$Skewness < 0) pa$theta <- 1 - pa$theta
      # we then calculate mean based on theta
      pa$Mean <- pa$k * pa$theta
      # returning the parameter list
      return(pa)
    }

    ## if theta is given
    if (!is.null(pa$theta)) {
      # we calculate mean based on theta
      pa$Mean <- pa$k * pa$theta
      # we then calculate skewness based on theta
      pa$Skewness <- (1 - 2 * pa$theta) / sqrt(pa$k * pa$theta * (1 - pa$theta))
      # returning the parameter list
      return(pa)
    }
  }

}

#+ dgmGenerator
dgm_generator <- function(...){

  pa <- list(...)

  if(is.list(pa$pa)) pa <- pa$pa

  ## setting default seed if not given
  if(is.null(pa$Model)) pa$Model <- "ChiAR(1)"
  if(is.null(pa$phi)) pa$phi <- 0.2
  # if no model parameter is given, then mean is set to a default 5
  if (is.null(pa$Mean) &
      is.null(pa$Skewness) &
      is.null(pa$c) &
      is.null(pa$nu) &
      is.null(pa$alpha) &
      is.null(pa$beta) &
      is.null(pa$theta)
  ) pa$Mean <- 5
  # if(is.null(pa$Mean)) pa$Variance <- 10
  # if(is.null(pa$Skewness)) pa$Skewness <- 3
  if(is.null(pa$T)) pa$T <- 100
  if(is.null(pa$seed)) pa$seed <- 0

  if(is.null(pa$only.ts)) pa$only.ts <- FALSE

  ## If you set `pa$only.ts` parameter as TRUE, the dgm_ functions produce only
  ## the raw time series (a single numeric vector) which is way lighter and way
  ## faster:

  # pa$only.ts <- TRUE


  ## calculating model parameters
  pa <- dgm_parameterizer(pa = pa)

  ### making models

  ## %%%%%%%%%%%%
  ## ChiAR(1)
  ## %%%%%%%%%%%%
  if(tolower(pa$Model) == "chiar(1)" | tolower(pa$Model) == "chiar" |
     tolower(pa$Model) == "chi2ar(1)" | tolower(pa$Model) == "chi2ar"){
    # default maximum scale value
    if(is.null(pa$k)) pa$k <- 100
    ## %% Generating the data
    o <- dgm_chiar(pa = pa)
  }

  ## %%%%%%%%%%%%
  ## BinAR(1)
  ## %%%%%%%%%%%%
  if(tolower(pa$Model) == "binar(1)" | tolower(pa$Model) == "binar"){
    # default maximum scale value
    if(is.null(pa$k)) pa$k <- 10
    ## %% Generating the data
    o <- dgm_binar(pa = pa)
  }

  ## %%%%%%%%%%%%
  ## DAR(1)
  ## %%%%%%%%%%%%
  if(tolower(pa$Model) == "dar(1)" | tolower(pa$Model) == "dar"){
    # default maximum scale value
    if(is.null(pa$k)) pa$k <- 10
    ## %% Generating the data
    o <- dgm_dar(pa = pa)
  }

  ## Also allow a data frame output
  # if(!is.null(p$as.dataframe) & p$as.dataframe){
  #
  # }

  return(o)
}


#' We also need a function to transform `dgm` objects into data frames,
#' which we'll need to simulate populations.
#'

#+ dgmAsDataFrame
## this function makes a long dataframe for each dgm object
dgm_as.dataframe <- function(dgm_object, subject = 0){
  ob <- dgm_object

  ## for only.ts = TRUE the dgm_object is a numeric vector, so this'll get short
  if(is.numeric(ob)){
    df <- data.frame(subject = subject,
                     t = 1:length(ob),
                     x = ob)
    return(df)
  }

  pa <- ob$Model.Parameters

  ## output as dataframe
  df <- data.frame(subject = subject,
                   Model = pa$Model,
                   seed = pa$seed,
                   theoretical_Mean = pa$Mean,
                   theoretical_Skewness = pa$Skewness,
                   theoretical_AR = pa$phi,
                   empirical_Mean = ob$Empirical.Parameters$Mean,
                   empirical_Skewness = ob$Empirical.Parameters$Skewness,
                   empirical_AR = ob$Empirical.Parameters$AR) %>%
    # rounding values
    mutate(across(contains("_"), ~ round(.x, 3))) %>%
    # replicating rows T times
    slice(rep(1:n(), each = pa$T)) %>%
    # adding time and value of time series
    mutate(t = 1:pa$T,
           x = ob$x,
           .after = subject)
  return(df)
}


#' And a function to make a sample population of `dgm`s given a vector of means:

#+ dgmMakePopulation

dgm_make.population <- function(Model = "ChiAR(1)",
                                Means = rnorm(100, 5, 3),
                                T = 100,
                                phi = 0.4,
                                seeds = NULL
){
  # pa <- list(...)
  # print(pa)
  # print(str(pa))
  N <- length(Means)
  df <- data.frame(subject = rep(1:N, each = T),
                   t = rep(1:T, times = N),
                   x = rep(NA, N*T))
  if(is.null(seeds)) seeds <- Means
  if(length(seeds)<=1) seeds <- seeds + Means

  for(s in 1:N){
    x <- dgm_generator(
      Model = Model,
      only.ts = TRUE,
      T = T,
      phi = phi,
      Mean = Means[s],
      # Skewness = Skewness,
      seed = seeds[s])

    df$x[((s-1)*T + 1):(s*T)] <- x

  }

  return(df)
}

#' Now we need to write plotting functions(s)
#'

#+ dgmPlot

## %%%%%%%%%%%%%%%%
## plotting Autocorrelation function

dgm_plot.acf <- function(dgm.obj,
                         lag.max = 10,
                         add.title = FALSE,
                         p.color = c("dodgerblue3"),
                         size = 1.5){

  l <- 0:lag.max
  d <- data.frame(
    l = l,
    a1 = stats::acf(dgm.obj$x,
                    lag.max = lag.max,
                    plot = FALSE)$acf
  )

  l.e <- seq(0, lag.max, 0.05)
  d.e <- data.frame(l.e = l.e,
                    e1 = dgm.obj$Model.Parameters$phi ^ l.e
  )

  plot.acf <- ggplot(d, aes(x = l, y = a1)) +
    geom_segment(aes(
      x = l,
      xend = l,
      y = 0,
      yend = a1
    ),
    size = size[1],
    color = p.color[1]) +
    # exponential fit
    geom_line(
      data = d.e,
      aes(x = l.e, y = e1),
      linetype = "dashed",
      color = "yellowgreen",
      size = 1
    ) +

    geom_hline(aes(yintercept = 0),
               size = 0.5) +
    # annotate(
    #   "label",
    #   x = 1.75,
    #   y = d$a1[2],
    #   label = unname(TeX(paste0(
    #     "$\\rho = ", round(d$a1[2], 2), "$"
    #   ))),
    #   color = p.color[1]
    # ) +

    theme(legend.position = "none") +
    xlab("Lag") +
    ylab("Autocorrelation function") +
    # ggtitle("Autocorrelation function of X(t)") +
    theme(# axis.title.x=element_blank(),
      #     axis.text.x=element_blank(),
      #     axis.ticks.x=element_blank(),
      axis.line = element_line(colour = "black"),
      panel.background = element_blank())

  if(add.title)
    plot.acf <- plot.acf + ggtitle(TeX(dgm.obj$Model.Description))


  return(plot.acf)
}


## %%%%%%%%%%%%%%%%
## plotting density histogram

dgm_plot.hist <- function(dgm.obj,
                          add.title = FALSE,
                          p.color = c("lightcyan4"),
                          alpha = 0.9){

  vals <- dgm.obj$x

  out.of.bound <- sum(vals<0) + sum(vals>dgm.obj$Model.Parameters$k)
  out.of.bound <- round(100*out.of.bound/dgm.obj$Model.Parameters$T,0)

  color_mean <- c("deepskyblue1","yellowgreen")
  breaks <- seq(0,1,0.1)*dgm.obj$Model.Parameters$k
  binwidth <- ifelse(is.null(dgm.obj$Model.Parameters$theta),
                     # 1+3.322*log10(dgm.obj$Model.Parameters$T),
                     dgm.obj$Model.Parameters$T^(1/3)*2, # Rice's rule
                     # 3.49*sqrt(var(dgm.obj$x))*dgm.obj$Model.Parameters$T^(-1/3),
                     dgm.obj$Model.Parameters$k)


  d <- data.frame(t=1:dgm.obj$Model.Parameters$T,
                  vals = vals)

  plot.hist <- ggplot(d, aes(x = vals)) +
    geom_histogram(aes(y = ..density..),
                   binwidth = 1,
                   fill = p.color[1],
                   alpha = alpha) +

    geom_vline(aes(xintercept = dgm.obj$Model.Parameters$Mean),
               colour="yellowgreen",
               linetype="dotted",
               size = 1) +
    geom_vline(aes(xintercept = dgm.obj$Empirical.Parameters$Mean),
               colour="firebrick",
               linetype="dotdash",
               size = 1) +

    # annotate("label",
    #          x = dgm.obj$Model.Parameters$Mean,
    #          y = 0.1,
    #          label = TeX("$\\mu_{(implied)}$"),
    #          color = color_mean[1]) +
    annotate("label",
             x = dgm.obj$Empirical.Parameters$Mean,
             y = Inf,#0.01,
             vjust = 1,
             label = TeX("$\\mu_{empirical}$"),
             color = "firebrick") +

    xlab(TeX("$X_t$")) +
    ylab("Density") +

    # xlim(-0.1*dgm.obj$Model.Parameters$k,
    #  1.1*dgm.obj$Model.Parameters$k) +
    scale_x_continuous(limits = c(-0.1*dgm.obj$Model.Parameters$k,
                                  1.1*dgm.obj$Model.Parameters$k),
                       breaks = breaks,
                       expand = c(0, 0)) +
    scale_y_continuous(expand = expansion(mult = c(0, .1))) +
    theme(
      # axis.title.y = element_blank(),
      # axis.text.y = element_blank(),
      # axis.ticks.y = element_blank(),
      axis.line = element_line(colour = "black"),
      panel.background = element_blank()
    )

  if(out.of.bound)
    plot.hist <- plot.hist +
    annotate("label",
             x = Inf,# 0.8*dgm.obj$Model.Parameters$k,
             y = Inf,#0.01,
             vjust = 1,
             hjust = 1,
             label = paste0(out.of.bound,
                            "% of values out of bound"),
             color = "slateblue3")

  if(add.title)
    plot.hist <- plot.hist + ggtitle(TeX(dgm.obj$Model.Description))

  return(plot.hist)
}

dgm_plot.timeseries <- function(dgm.obj,
                                limit.t = 200,
                                add.title = FALSE,
                                p.color = c("slateblue3"),
                                size = 1.5){

  limit.t <- min(limit.t, dgm.obj$Model.Parameters$T)
  breaks.t <- seq(0,1,0.1)*limit.t

  ts <- data.frame(t = 1:dgm.obj$Model.Parameters$T,
                   value = dgm.obj$x)[1:limit.t,]

  # time series plot
  plot.timeseries <- ts %>%
    ggplot(aes(x = t, y = value)) +
    geom_line(color = p.color[1]) +

    scale_x_continuous(breaks = breaks.t,
                       expand = c(0, limit.t*0.05)) +
    scale_y_continuous(breaks = seq(0,1,0.1)*dgm.obj$Model.Parameters$k,
                       expand = expansion(mult = c(0, .1))) +

    geom_hline(aes(yintercept = dgm.obj$Empirical.Parameters$Mean),
               colour="firebrick",
               linetype="dotdash",
               size = 1) +

    # annotate("label",
    #          x = dgm.obj$Model.Parameters$Mean,
    #          y = 0.1,
    #          label = TeX("$\\mu_{(implied)}$"),
    #          color = color_mean[1]) +
    annotate("label",
             x = 0.2*limit.t,
             y = dgm.obj$Empirical.Parameters$Mean,
             label = TeX("$\\mu_{empirical}$"),
             color = "firebrick") +
    xlab("time") +
    ylab(TeX("$X_t$")) +

    theme(
      # axis.title.y = element_blank(),
      # axis.text.y = element_blank(),
      # axis.ticks.y = element_blank(),
      axis.line = element_line(colour = "black"),
      panel.background = element_blank()
    )

  if(add.title)
    plot.timeseries <- plot.timeseries + ggtitle(TeX(dgm.obj$Model.Description))


  return(plot.timeseries)
}

## %%%%%%%%%%%%%%%
## plotting side-by-side
## %%%%%%%%%%%%%%%

dgm_plot.profile <- function(dgm.obj,
                             p.color = "lavenderblush4",
                             lag.max = 20,
                             limit.t = 300){

  p_right <- plot_grid(dgm_plot.timeseries(dgm.obj,
                                           limit.t = limit.t,
                                           p.color = p.color),
                       dgm_plot.acf(dgm.obj,
                                    lag.max = lag.max,
                                    p.color = p.color),
                       align = "v",
                       axis = "r",
                       nrow = 2,
                       # labels = "AUTO",
                       rel_heights = c(1, 1))

  p_profile <- plot_grid(dgm_plot.hist(dgm.obj,
                                       p.color = p.color),
                         p_right,
                         # align = "h", axis = "b",
                         # labels = "AUTO",
                         rel_widths = c(2, 1))

  title <- ggdraw() +
    draw_label(
      TeX(dgm.obj$Model.Description),
      # fontface = 'bold',
      x = 0,
      hjust = 0
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 7)
    )


  output <- plot_grid(
    title, p_profile,
    ncol = 1,
    # rel_heights values control vertical title margins
    rel_heights = c(0.1, 1)
  )

  return(output)

}


## %%%%%%%%%%%%
## samples
## %%%%%%%%%%%%
#
# t1 <- dgm_generator(Mean = 50,
#                     Skewness = .2,
#                     phi = 0.2999,
#                     T = 100000,
#                     seed = 1)
#
# t2 <- dgm_generator(Mean = 44.5,
#                     Skewness = 0.4,
#                     phi = 0.1,
#                     T = 100,
#                     seed = 1)
#
# t3 <- dgm_generator(Mean = 0.3,
#                     Skewness = 0.04,
#                     phi = 0.1,
#                     T = 500,
#                     seed = 1,
#                     Model = "BinAR")
#
# t3 <- dgm_generator(Mean = 0.3,
#                     Skewness = 0.04,
#                     phi = 0.81,
#                     T = 100,
#                     seed = 1,
#                     Model = "DAR")
#
# dgm_plot.timeseries(t1,add.title = TRUE)
# dgm_plot.hist(t2)
# dgm_plot.acf(t3)
#
# dgm_plot.profile(t2, p.color = "lavenderblush4")

