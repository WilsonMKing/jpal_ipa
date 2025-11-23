################################################################################
########################### Function: Run Regressions ##########################
################################################################################

### Function: Run Regressions
run_ipa_regressions <- function(outcome, data = freqs_panel, specification, controls = TRUE){
  
  ### OLS Event Study Specification
  if(specification == "ols-event-study"){
    if(controls == TRUE){
      form <- as.formula(paste0(outcome, " ~ i(time_to_ipa, ever_ipa, ref = -1) + log(population) + log(gdp) | as.factor(country) + as.factor(year) + as.factor(regime) + as.factor(conflict) + as.factor(independent) + as.factor(igc_treated)"))
      regression <- fixest::feols(form, cluster = ~country, data = data, se = "cluster")
    } else {
      form <- as.formula(paste0(outcome, " ~ i(time_to_ipa, ever_ipa, ref = -1) | as.factor(country) + as.factor(year)"))
      regression <- fixest::feols(form, cluster = ~country, data = data, se = "cluster")
    }
  }
  
  ### Sun & Abraham Event Study Specification
  if(specification == "sa-event-study"){
    if(controls == TRUE){
      form <- as.formula(paste0(outcome, " ~ sunab(ipa, time_to_ipa) + log(population) + log(gdp) | as.factor(country) + as.factor(year) + as.factor(regime) + as.factor(conflict) + as.factor(independent) + as.factor(igc_treated)"))
      regression <- fixest::feols(form, cluster = ~country, data = data, se = "cluster")
    } else {
      form <- as.formula(paste0(outcome, " ~ sunab(ipa, time_to_ipa) | as.factor(country) + as.factor(year)"))
      regression <- fixest::feols(form, cluster = ~country, data = data, se = "cluster")
    }
  }
  
  ### OLS Binary Difference-in-Differences Specification
  if(specification == "ols-binary-did"){
    if(controls == TRUE){
      form <- as.formula(paste0(outcome, " ~ ipa_treated + log(population) + log(gdp) | as.factor(country) + as.factor(year) + as.factor(regime) + as.factor(conflict) + as.factor(independent) + as.factor(igc_treated) | 0 | country"))
      regression <- lfe::felm(form, data = data)
    } else {
      form <- as.formula(paste0(outcome, " ~ ipa_treated | as.factor(country) + as.factor(year) | 0 | country"))
      regression <- lfe::felm(form, data = data)
    }
  }
  
  ### OLS Binary Difference-in-Differences Specification w/ Region Fixed Effects
  if(specification == "ols-binary-did-region-fes"){
    if(controls == TRUE){
      form <- as.formula(paste0(outcome, " ~ ipa_treated + log(population) + log(gdp) + as.factor(continent)*as.factor(year) | as.factor(country) + as.factor(year) + as.factor(regime) + as.factor(conflict) + as.factor(igc_treated) + as.factor(independent) | 0 | country"))
      regression <- lfe::felm(form, data = data)
    } else {
      form <- as.formula(paste0(outcome, " ~ ipa_treated | as.factor(country) + as.factor(year) + as.factor(continent)*as.factor(year) | 0 | country"))
      regression <- lfe::felm(form, data = data)
    }
  }
  
  ### OLS Binary Difference-in-Differences Specification w/ Spillovers
  if(specification == "ols-binary-did-spillovers"){
    if(controls == TRUE){
      form <- as.formula(paste0(outcome, " ~ ipa_treated + bordering_ipa_treated + log(population) + log(gdp) | as.factor(country) + as.factor(year) + as.factor(regime) + as.factor(conflict) + as.factor(igc_treated) + as.factor(independent) | 0 | country"))
      regression <- lfe::felm(form, data = data)
    } else {
      form <- as.formula(paste0(outcome, " ~ ipa_treated + bordering_ipa_treated | as.factor(country) + as.factor(year) | 0 | country"))
      regression <- lfe::felm(form, data = data)
    }
  }
  
  ### Poisson QML Binary Difference-in-Differences Specification
  if(specification == "poisson-binary-did"){
    if(controls == TRUE){
      form <- as.formula(paste0(outcome, " ~ ipa_treated + log(population) + log(gdp) | as.factor(country) + as.factor(year) + as.factor(regime) + as.factor(conflict) + as.factor(independent) + as.factor(igc_treated)"))
      regression <- feglm(form, data = data, cluster = ~country, fixef.rm = "none", family = "quasipoisson")
    } else {
      form <- as.formula(paste0(outcome, " ~ ipa_treated | as.factor(country) + as.factor(year)"))
      regression <- feglm(form, data = data, cluster = ~country, fixef.rm = "none", family = "quasipoisson")
    }
  }
  
  return(regression)
  
}

### Function: Run Regressions
run_jpal_regressions <- function(outcome, data = freqs_panel, specification, controls = TRUE){
  
  ### OLS Event Study Specification
  if(specification == "ols-event-study"){
    if(controls == TRUE){
      form <- as.formula(paste0(outcome, " ~ i(time_to_jpal, ever_jpal, ref = -1) + log(population) + log(gdp) | as.factor(country) + as.factor(year) + as.factor(regime) + as.factor(conflict) + as.factor(independent) + as.factor(igc_treated)"))
      regression <- fixest::feols(form, cluster = ~country, data = data, se = "cluster")
    } else {
      form <- as.formula(paste0(outcome, " ~ i(time_to_jpal, ever_jpal, ref = -1) | as.factor(country) + as.factor(year)"))
      regression <- fixest::feols(form, cluster = ~country, data = data, se = "cluster")
    }
  }
  
  ### Sun & Abraham Event Study Specification
  if(specification == "sa-event-study"){
    if(controls == TRUE){
      form <- as.formula(paste0(outcome, " ~ sunab(jpal, time_to_jpal) + log(population) + log(gdp) | as.factor(country) + as.factor(year) + as.factor(regime) + as.factor(conflict) + as.factor(independent) + as.factor(igc_treated)"))
      regression <- fixest::feols(form, cluster = ~country, data = data, se = "cluster")
    } else {
      form <- as.formula(paste0(outcome, " ~ sunab(jpal, time_to_jpal) | as.factor(country) + as.factor(year)"))
      regression <- fixest::feols(form, cluster = ~country, data = data, se = "cluster")
    }
  }
  
  ### OLS Binary Difference-in-Differences Specification
  if(specification == "ols-binary-did"){
    if(controls == TRUE){
      form <- as.formula(paste0(outcome, " ~ jpal_treated + log(population) + log(gdp) | as.factor(country) + as.factor(year) + as.factor(regime) + as.factor(conflict) + as.factor(independent)  + as.factor(igc_treated) | 0 | country"))
      regression <- lfe::felm(form, data = data)
    } else {
      form <- as.formula(paste0(outcome, " ~ jpal_treated | as.factor(country) + as.factor(year) | 0 | country"))
      regression <- lfe::felm(form, data = data)
    }
  }
  
  ### OLS Binary Difference-in-Differences Specification w/ Region Fixed Effects
  if(specification == "ols-binary-did-region-fes"){
    if(controls == TRUE){
      form <- as.formula(paste0(outcome, " ~ jpal_treated + log(population) + log(gdp) + as.factor(continent)*as.factor(year) | as.factor(country) + as.factor(year) + as.factor(regime) + as.factor(conflict) + as.factor(igc_treated) + as.factor(independent) | 0 | country"))
      regression <- lfe::felm(form, data = data)
    } else {
      form <- as.formula(paste0(outcome, " ~ jpal_treated | as.factor(country) + as.factor(year) + as.factor(continent)*as.factor(year) | 0 | country"))
      regression <- lfe::felm(form, data = data)
    }
  }
  
  ### OLS Binary Difference-in-Differences Specification w/ Spillovers
  if(specification == "ols-binary-did-spillovers"){
    if(controls == TRUE){
      form <- as.formula(paste0(outcome, " ~ jpal_treated + bordering_jpal_treated + log(population) + log(gdp) | as.factor(country) + as.factor(year) + as.factor(regime) + as.factor(conflict) + as.factor(igc_treated) + as.factor(independent) | 0 | country"))
      regression <- lfe::felm(form, data = data)
    } else {
      form <- as.formula(paste0(outcome, " ~ jpal_treated + bordering_jpal_treated | as.factor(country) + as.factor(year) | 0 | country"))
      regression <- lfe::felm(form, data = data)
    }
  }
  
  ### OLS Binary Difference-in-Differences Specification
  if(specification == "ols-binary-did-both"){
    if(controls == TRUE){
      form <- as.formula(paste0(outcome, " ~ jpal_treated + ipa_treated + log(population) + log(gdp) | as.factor(country) + as.factor(year) + as.factor(regime) + as.factor(conflict) + as.factor(independent)  + as.factor(igc_treated) | 0 | country"))
      regression <- lfe::felm(form, data = data)
    } else {
      form <- as.formula(paste0(outcome, " ~ jpal_treated + ipa_treated | as.factor(country) + as.factor(year) | 0 | country"))
      regression <- lfe::felm(form, data = data)
    }
  }
    
    ### OLS Binary Difference-in-Differences Specification
    if(specification == "ols-binary-did-both-region-fes"){
      if(controls == TRUE){
        form <- as.formula(paste0(outcome, " ~ jpal_treated + ipa_treated + log(population) + log(gdp) + as.factor(continent)*as.factor(year) | as.factor(country) + as.factor(year) + as.factor(regime) + as.factor(conflict) + as.factor(independent)  + as.factor(igc_treated) | 0 | country"))
        regression <- lfe::felm(form, data = data)
      } else {
        form <- as.formula(paste0(outcome, " ~ jpal_treated + ipa_treated + as.factor(continent)*as.factor(year) | as.factor(country) + as.factor(year) | 0 | country"))
        regression <- lfe::felm(form, data = data)
      }
    }
  
  ### OLS Binary Difference-in-Differences Specification
  if(specification == "ols-binary-did-ext"){
    if(controls == TRUE){
      form <- as.formula(paste0(outcome, " ~ jpal_ext_treated + log(population) + log(gdp) | as.factor(country) + as.factor(year) + as.factor(regime) + as.factor(conflict) + as.factor(independent)  + as.factor(igc_treated) | 0 | country"))
      regression <- lfe::felm(form, data = data)
    } else {
      form <- as.formula(paste0(outcome, " ~ jpal_ext_treated | as.factor(country) + as.factor(year) | 0 | country"))
      regression <- lfe::felm(form, data = data)
    }
  }
  
  ### Poisson QML Binary Difference-in-Differences Specification
  if(specification == "poisson-binary-did"){
    if(controls == TRUE){
      form <- as.formula(paste0(outcome, " ~ jpal_treated + log(population) + log(gdp) | as.factor(country) + as.factor(year) + as.factor(regime) + as.factor(conflict) + as.factor(independent) + as.factor(igc_treated)"))
      regression <- feglm(form, data = data, cluster = ~country, fixef.rm = "none", family = "quasipoisson")
    } else {
      form <- as.formula(paste0(outcome, " ~ jpal_treated | as.factor(country) + as.factor(year)"))
      regression <- feglm(form, data = data, cluster = ~country, fixef.rm = "none", family = "quasipoisson")
    }
  }
  
  return(regression)
  
}



