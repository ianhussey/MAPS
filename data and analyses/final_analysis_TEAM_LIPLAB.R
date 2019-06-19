TEAM_NAME = "team_liplab"
VERSION = "R version 3.5.2 (2018-12-20)"

load_data <- function(path){
  
  # IMPORTS 
  library(tidyverse)
  
  # SELECT DATA
  data <- read.csv(path) %>%
    dplyr::select(comp_week,
                  comp_wend,
                  dep_score,
                  dep_thoughts,
                  has_dep_diag,
                  dep_band_15)
  
  return(data)
}


computer_use_001 <- function(data){
  
  # IMPORTS
  library(tidyverse)
  library(forcats)
  
  # SET ORDERED LEVELS
  data <- data %>%
    dplyr::mutate(comp_use_1 = forcats::fct_relevel(comp_week,
                                                    "Not at all",
                                                    "Less than 1 hour",
                                                    "1-2 hours",
                                                    "3 or more hours"),
                  comp_use_1 = as.ordered(comp_use_1),
                  comp_use_2 = forcats::fct_relevel(comp_wend,
                                                    "Not at all",
                                                    "Less than 1 hour",
                                                    "1-2 hours",
                                                    "3 or more hours"),
                  comp_use_2 = as.ordered(comp_use_2))
  
  return(data)
}


depression_001 <- function(data){
  
  # IMPORTS
  library(tidyverse)
  
  # SET ORDERED LEVELS
  data <- data %>%
    dplyr::mutate(depression = as.ordered(dep_score))
  
  return(data)
}


specify_model <- function(data){
  
  # IMPORTS
  library(tidyverse)
  library(brms)  # nb requires brms 2.8+
  library(parallel)
  
  # MODEL 1: depression ~ comp_use_1
  mod_1 <- brm(formula      = depression ~ comp_use_1,
               data         = data,
               family       = cumulative("logit"),
               #prior        = c(set_prior("normal(0, 1)")),
               sample_prior = TRUE,
               iter         = 2000,
               chains       = 4,
               control      = list(adapt_delta = 0.95),
               cores        = detectCores(),
               file         = "TEAM_LIPLAB_mod_1")
  
  # ADD WAIC 
  mod_1 <- add_criterion(mod_1, "waic")
  
  # EXTRACT WAIC VALUE
  n_waic_1 <- mod_1$waic$estimates %>%
    as.data.frame() %>%
    rownames_to_column(var = "coefficient") %>%
    filter(coefficient == "waic") %>%
    pull(Estimate) %>%
    as.numeric()
  
  # CONVERT LOG ODDS TO ODDS RATIOS 
  n_odds_ratios_1 <- posterior_summary(mod_1) %>%
    exp() %>%  
    as.data.frame() %>%
    rownames_to_column(var = "effect") %>%
    filter(effect == "b_comp_use_1.L")  # extract effect for linear trend
  
  or_1 <- n_odds_ratios_1 %>%
    pull(Estimate) %>%
    as.numeric()
  
  n_ci_1_lower <- n_odds_ratios_1 %>%
    pull(Q2.5) %>%
    as.numeric()
  
  n_ci_1_upper <- n_odds_ratios_1 %>%
    pull(Q97.5) %>%
    as.numeric()
  
  ci_1 <- c(n_ci_1_lower, n_ci_1_upper)
  
  
  # MODEL 2: depression ~ comp_use_2
  mod_2 <- brm(formula      = depression ~ comp_use_2,
               data         = data,
               family       = cumulative("logit"),
               #prior        = c(set_prior("normal(0, 1)")),
               sample_prior = TRUE,
               iter         = 2000,
               chains       = 4,
               control      = list(adapt_delta = 0.95),
               cores        = detectCores(),
               file         = "TEAM_LIPLAB_mod_2")
  
  # ADD WAIC 
  mod_2 <- add_criterion(mod_2, "waic")
  
  # EXTRACT WAIC VALUE
  n_waic_2 <- mod_2$waic$estimates %>%
    as.data.frame() %>%
    rownames_to_column(var = "coefficient") %>%
    filter(coefficient == "waic") %>%
    pull(Estimate) %>%
    as.numeric()
  
  # CONVERT LOG ODDS TO ODDS RATIOS 
  n_odds_ratios_2 <- posterior_summary(mod_2) %>%
    exp() %>%  # convert log odds to odds ratios
    as.data.frame() %>%
    rownames_to_column(var = "effect") %>%
    filter(effect == "b_comp_use_2.L")  # extract effect for linear trend
  
  or_2 <- n_odds_ratios_2 %>%
    pull(Estimate) %>%
    as.numeric()
  
  n_ci_2_lower <- n_odds_ratios_2 %>%
    pull(Q2.5) %>%
    as.numeric()
  
  n_ci_2_upper <- n_odds_ratios_2 %>%
    pull(Q97.5) %>%
    as.numeric()
  
  ci_2 <- c(n_ci_2_lower, n_ci_2_upper)
  
  # RETURN RESULTS LIST
  return(list(mod   = list(mod_1 = mod_1, 
                           mod_2 = mod_2), 
              or_1  = or_1,
              ci_1  = ci_1,
              p_1   = NA,
              or_2  = or_2,
              ci_2  = ci_2,
              p_2   = NA,
              AIC   = NA,
              WAIC  = list(WAIC_1 = n_waic_1,
                           WAIC_2 = n_waic_2))) 
  
}

data    <- load_data("maps-synthetic-data-v1.1.csv")
data    <- computer_use_001(data)
data    <- depression_001(data)
results <- specify_model(data)

# # save and print results
# save(results, file = "results.RData")
# 
# # print results
# results$or_1
# results$ci_1
# results$or_2
# results$ci_2
# results$WAIC


