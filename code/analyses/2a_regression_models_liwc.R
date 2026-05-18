############################
#### LIWC / DICTIONARY × AFFECT ANALYSES
#### MAIN PAPER + SUPPLEMENT
############################

############################
#### 0) PREPARATION
############################

required_packages <- c(
  "dplyr",
  "tidyr",
  "purrr",
  "tibble",
  "ggplot2",
  "lme4",
  "broom",
  "broom.mixed",
  "lmtest",
  "sandwich",
  "scales",
  "stringr",
  "forcats",
  "patchwork",
  "grid"
)

invisible(lapply(required_packages, library, character.only = TRUE))

dir.create("results", recursive = TRUE, showWarnings = FALSE)
dir.create("figures", recursive = TRUE, showWarnings = FALSE)

source("code/analyses/helper/plot_theme.R")
base_theme <- theme_custom(base_size = 12)

set.seed(123)

z_scale <- function(x) {
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(NA_real_, length(x)))
  as.numeric(scale(x))
}

safe_sd <- function(x) {
  sd(x, na.rm = TRUE)
}

p_from_est_se <- function(estimate, se) {
  ifelse(
    is.na(estimate) | is.na(se) | se <= 0,
    NA_real_,
    2 * pnorm(abs(estimate / se), lower.tail = FALSE)
  )
}

nice_context <- function(x) {
  dplyr::case_when(
    x == "private" ~ "Private",
    x == "public"  ~ "Public",
    TRUE           ~ as.character(x)
  )
}

nice_outcome_label <- function(x) {
  dplyr::case_when(
    x == "pa_trait_z"          ~ "Trait PA",
    x == "na_trait_z"          ~ "Trait NA",
    x == "daily_valence_z"     ~ "Daily valence",
    x == "momentary_valence_z" ~ "Momentary valence",
    TRUE                       ~ x
  )
}

############################
#### 1) FEATURE DEFINITIONS
############################

# Theory-guided dictionary features for the main paper.


theory_feature_labels <- c(
  "liwc_posemo" = "Pos. emotion",
  "liwc_negemo" = "Neg. emotion",
  "liwc_i"      = "I",
  "liwc_we"     = "We"
)

theory_features_requested <- names(theory_feature_labels)

theory_features_requested <- names(theory_feature_labels)

############################
#### 2) LOAD DATA
############################

keyboard_data_trait <- readRDS("data/results/keyboard_data_trait_final.rds") %>%
  as.data.frame() %>%
  filter(scope %in% c("private", "public")) %>%
  mutate(
    context = factor(scope, levels = c("private", "public"))
  )

keyboard_data_day <- readRDS("data/results/keyboard_data_day_final.rds") %>%
  as.data.frame() %>%
  filter(scope %in% c("private", "public")) %>%
  mutate(
    context = factor(scope, levels = c("private", "public")),
    date = as.Date(date),
    user_day_id = interaction(user_id, date, drop = TRUE)
  )

keyboard_data_moment <- readRDS("data/results/keyboard_data_ema_final.rds") %>%
  as.data.frame() %>%
  filter(scope %in% c("private", "public")) %>%
  mutate(
    context = factor(scope, levels = c("private", "public"))
  )

if (!all(c("user_id", "scope", "pa_panas", "na_panas") %in% names(keyboard_data_trait))) {
  stop("Trait data must contain user_id, scope, pa_panas, and na_panas.")
}

if (!all(c("user_id", "scope", "daily_valence") %in% names(keyboard_data_day))) {
  stop("Daily data must contain user_id, scope, and daily_valence.")
}

if (!all(c("user_id", "scope", "valence") %in% names(keyboard_data_moment))) {
  stop("Momentary data must contain user_id, scope, and valence.")
}

############################
#### 3) FEATURE AVAILABILITY
############################

theory_features <- theory_features_requested[
  theory_features_requested %in% names(keyboard_data_trait)
]

if (length(theory_features) == 0) {
  stop("None of the requested theory-guided dictionary features are present in the trait data.")
}

missing_theory_features <- setdiff(theory_features_requested, theory_features)

if (length(missing_theory_features) > 0) {
  message("Theory-guided features missing and omitted: ")
  print(missing_theory_features)
}

theory_feature_lookup <- tibble(
  feature = theory_features,
  label = unname(theory_feature_labels[theory_features])
)

write.csv(
  theory_feature_lookup,
  "results/dictionary_theory_feature_lookup.csv",
  row.names = FALSE
)

# Exploratory all-LIWC features:
# Use LIWC share features, excluding session-derived summaries.
# This is for appendix/supplement only.

trait_liwc_features <- names(keyboard_data_trait) %>%
  stringr::str_subset("^liwc_") %>%
  .[!stringr::str_detect(., "_session_")] %>%
  .[sapply(keyboard_data_trait[.], is.numeric)]

day_liwc_features <- names(keyboard_data_day) %>%
  stringr::str_subset("^liwc_") %>%
  .[!stringr::str_detect(., "_session_")] %>%
  .[sapply(keyboard_data_day[.], is.numeric)]

moment_liwc_features <- names(keyboard_data_moment) %>%
  stringr::str_subset("^liwc_") %>%
  .[!stringr::str_detect(., "_session_")] %>%
  .[sapply(keyboard_data_moment[.], is.numeric)]

all_liwc_features <- Reduce(
  intersect,
  list(trait_liwc_features, day_liwc_features, moment_liwc_features)
)

if (length(all_liwc_features) == 0) {
  stop("No shared LIWC features found across trait, daily, and momentary datasets.")
}

message("Shared LIWC features across trait/day/momentary: ", length(all_liwc_features))

liwc_feature_lookup <- tibble(
  feature = all_liwc_features,
  label = all_liwc_features
)

write.csv(
  liwc_feature_lookup,
  "results/supp_all_liwc_feature_lookup.csv",
  row.names = FALSE
)

############################
#### 4) PREPARE ANALYSIS DATA
############################

# Trait outcomes are standardized once at participant level, then joined back to context rows.
# This avoids double-weighting participants who contribute both private and public rows.

trait_outcomes <- keyboard_data_trait %>%
  distinct(user_id, pa_panas, na_panas) %>%
  filter(!is.na(pa_panas), !is.na(na_panas)) %>%
  mutate(
    pa_trait_z = z_scale(pa_panas),
    na_trait_z = z_scale(na_panas)
  ) %>%
  select(user_id, pa_panas, na_panas, pa_trait_z, na_trait_z)

keyboard_data_trait_z <- keyboard_data_trait %>%
  select(user_id, scope, context, all_of(unique(c(theory_features, all_liwc_features)))) %>%
  left_join(trait_outcomes, by = "user_id") %>%
  mutate(
    across(
      all_of(unique(c(theory_features, all_liwc_features))),
      z_scale,
      .names = "{.col}_z"
    )
  )

keyboard_data_day_z <- keyboard_data_day %>%
  mutate(
    daily_valence_z = z_scale(daily_valence)
  ) %>%
  mutate(
    across(
      all_of(unique(c(intersect(theory_features, names(.)), all_liwc_features))),
      z_scale,
      .names = "{.col}_z"
    )
  )

keyboard_data_moment_z <- keyboard_data_moment %>%
  mutate(
    momentary_valence_z = z_scale(valence)
  ) %>%
  mutate(
    across(
      all_of(unique(c(intersect(theory_features, names(.)), all_liwc_features))),
      z_scale,
      .names = "{.col}_z"
    )
  )

############################
#### 5) DESCRIPTIVES / FEATURE DIAGNOSTICS
############################

dictionary_feature_descriptives_trait <- keyboard_data_trait %>%
  select(user_id, context, all_of(unique(c(theory_features, all_liwc_features)))) %>%
  pivot_longer(
    cols = all_of(unique(c(theory_features, all_liwc_features))),
    names_to = "feature",
    values_to = "value"
  ) %>%
  group_by(feature, context) %>%
  summarise(
    n_rows = n(),
    n_users = n_distinct(user_id),
    n_nonmissing = sum(!is.na(value)),
    n_nonzero = sum(!is.na(value) & value != 0),
    prop_nonzero = n_nonzero / n_rows,
    mean_raw = mean(value, na.rm = TRUE),
    sd_raw = sd(value, na.rm = TRUE),
    median_raw = median(value, na.rm = TRUE),
    q25_raw = quantile(value, 0.25, na.rm = TRUE),
    q75_raw = quantile(value, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(context = as.character(context))

write.csv(
  dictionary_feature_descriptives_trait,
  "results/supp_dictionary_feature_descriptives_trait_by_context.csv",
  row.names = FALSE
)

############################
#### 6) MODEL HELPERS: TRAIT
############################

coef_by_context_lm <- function(model, term, vcov_mat, conf = 0.95) {
  b <- coef(model)
  all_names <- names(b)
  
  int_term_1 <- paste0(term, ":contextpublic")
  int_term_2 <- paste0("contextpublic:", term)
  
  int_term <- if (int_term_1 %in% all_names) {
    int_term_1
  } else if (int_term_2 %in% all_names) {
    int_term_2
  } else {
    NA_character_
  }
  
  if (!term %in% all_names) {
    return(tibble(
      context = c("private", "public"),
      estimate = NA_real_,
      se = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      p.value = NA_real_
    ))
  }
  
  z <- qnorm(1 - (1 - conf) / 2)
  
  est_private <- unname(b[term])
  se_private <- sqrt(unname(vcov_mat[term, term]))
  
  if (!is.na(int_term) && int_term %in% rownames(vcov_mat)) {
    est_public <- unname(b[term] + b[int_term])
    
    var_public <- unname(
      vcov_mat[term, term] +
        vcov_mat[int_term, int_term] +
        2 * vcov_mat[term, int_term]
    )
    
    se_public <- sqrt(pmax(var_public, 0))
  } else {
    est_public <- NA_real_
    se_public <- NA_real_
  }
  
  tibble(
    context = c("private", "public"),
    estimate = c(est_private, est_public),
    se = c(se_private, se_public),
    conf.low = c(
      est_private - z * se_private,
      est_public - z * se_public
    ),
    conf.high = c(
      est_private + z * se_private,
      est_public + z * se_public
    ),
    p.value = p_from_est_se(
      estimate = c(est_private, est_public),
      se = c(se_private, se_public)
    )
  )
}

fit_trait_dictionary_model <- function(data, outcome, feature) {
  feat_z <- paste0(feature, "_z")
  
  if (!feat_z %in% names(data)) {
    return(NULL)
  }
  
  dat <- data %>%
    filter(
      !is.na(.data[[outcome]]),
      !is.na(.data[[feat_z]]),
      !is.na(context),
      !is.na(user_id)
    ) %>%
    mutate(
      context = factor(context, levels = c("private", "public"))
    )
  
  if (
    nrow(dat) < 30 ||
    n_distinct(dat$user_id) < 20 ||
    n_distinct(dat$context) < 2 ||
    is.na(safe_sd(dat[[outcome]])) ||
    is.na(safe_sd(dat[[feat_z]])) ||
    safe_sd(dat[[outcome]]) == 0 ||
    safe_sd(dat[[feat_z]]) == 0
  ) {
    return(NULL)
  }
  
  fml <- as.formula(paste0(outcome, " ~ ", feat_z, " * context"))
  mod <- lm(fml, data = dat)
  
  V <- tryCatch(
    sandwich::vcovCL(mod, cluster = dat$user_id, type = "HC1"),
    error = function(e) sandwich::vcovHC(mod, type = "HC3")
  )
  
  list(
    model = mod,
    vcov = V,
    data = dat,
    feature_z = feat_z
  )
}

extract_trait_context_results <- function(fit, outcome, feature) {
  if (is.null(fit)) {
    return(tibble(
      outcome = outcome,
      outcome_label = nice_outcome_label(outcome),
      feature = feature,
      context = c("private", "public"),
      estimate = NA_real_,
      se = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      p.value = NA_real_,
      n_rows = NA_integer_,
      n_users = NA_integer_
    ))
  }
  
  coef_by_context_lm(
    model = fit$model,
    term = fit$feature_z,
    vcov_mat = fit$vcov
  ) %>%
    mutate(
      outcome = outcome,
      outcome_label = nice_outcome_label(outcome),
      feature = feature,
      n_rows = nrow(fit$data),
      n_users = n_distinct(fit$data$user_id),
      .before = 1
    )
}

extract_trait_interaction_results <- function(fit, outcome, feature) {
  if (is.null(fit)) {
    return(tibble(
      outcome = outcome,
      outcome_label = nice_outcome_label(outcome),
      feature = feature,
      term = "dictionary_z:contextpublic",
      estimate = NA_real_,
      se = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      p.value = NA_real_,
      n_rows = NA_integer_,
      n_users = NA_integer_
    ))
  }
  
  ct <- lmtest::coeftest(fit$model, vcov. = fit$vcov)
  td <- broom::tidy(ct)
  
  term_1 <- paste0(fit$feature_z, ":contextpublic")
  term_2 <- paste0("contextpublic:", fit$feature_z)
  
  out <- td %>%
    filter(term %in% c(term_1, term_2)) %>%
    slice(1)
  
  if (nrow(out) == 0) {
    return(tibble(
      outcome = outcome,
      outcome_label = nice_outcome_label(outcome),
      feature = feature,
      term = "dictionary_z:contextpublic",
      estimate = NA_real_,
      se = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      p.value = NA_real_,
      n_rows = nrow(fit$data),
      n_users = n_distinct(fit$data$user_id)
    ))
  }
  
  out %>%
    transmute(
      outcome = outcome,
      outcome_label = nice_outcome_label(outcome),
      feature = feature,
      term = "dictionary_z:contextpublic",
      estimate = estimate,
      se = std.error,
      conf.low = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error,
      p.value = p.value,
      n_rows = nrow(fit$data),
      n_users = n_distinct(fit$data$user_id)
    )
}

############################
#### 7) MODEL HELPERS: DAILY / MOMENTARY MIXED MODELS
############################

fit_mixed_with_fallback <- function(formulas, data) {
  for (i in seq_along(formulas)) {
    mod <- tryCatch(
      suppressWarnings(
        lme4::lmer(
          formula = formulas[[i]],
          data = data,
          REML = FALSE,
          control = lme4::lmerControl(
            optimizer = "bobyqa",
            optCtrl = list(maxfun = 2e5)
          )
        )
      ),
      error = function(e) NULL
    )
    
    if (!is.null(mod)) return(mod)
  }
  
  NULL
}

coef_by_context_lmer <- function(model, term, conf = 0.95) {
  if (is.null(model)) {
    return(tibble(
      context = c("private", "public"),
      estimate = NA_real_,
      se = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      p.value = NA_real_
    ))
  }
  
  b <- lme4::fixef(model)
  V <- as.matrix(vcov(model))
  all_names <- names(b)
  
  int_term_1 <- paste0(term, ":contextpublic")
  int_term_2 <- paste0("contextpublic:", term)
  
  int_term <- if (int_term_1 %in% all_names) {
    int_term_1
  } else if (int_term_2 %in% all_names) {
    int_term_2
  } else {
    NA_character_
  }
  
  if (!term %in% all_names) {
    return(tibble(
      context = c("private", "public"),
      estimate = NA_real_,
      se = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      p.value = NA_real_
    ))
  }
  
  z <- qnorm(1 - (1 - conf) / 2)
  
  est_private <- unname(b[term])
  se_private <- sqrt(unname(V[term, term]))
  
  if (!is.na(int_term) && int_term %in% rownames(V)) {
    est_public <- unname(b[term] + b[int_term])
    
    var_public <- unname(
      V[term, term] +
        V[int_term, int_term] +
        2 * V[term, int_term]
    )
    
    se_public <- sqrt(pmax(var_public, 0))
  } else {
    est_public <- NA_real_
    se_public <- NA_real_
  }
  
  tibble(
    context = c("private", "public"),
    estimate = c(est_private, est_public),
    se = c(se_private, se_public),
    conf.low = c(
      est_private - z * se_private,
      est_public - z * se_public
    ),
    conf.high = c(
      est_private + z * se_private,
      est_public + z * se_public
    ),
    p.value = p_from_est_se(
      estimate = c(est_private, est_public),
      se = c(se_private, se_public)
    )
  )
}

fit_state_dictionary_model <- function(data, outcome, feature) {
  feat_z <- paste0(feature, "_z")
  
  if (!feat_z %in% names(data)) {
    return(NULL)
  }
  
  dat <- data %>%
    filter(
      !is.na(.data[[outcome]]),
      !is.na(.data[[feat_z]]),
      !is.na(context),
      !is.na(user_id)
    ) %>%
    mutate(
      context = factor(context, levels = c("private", "public"))
    )
  
  if (
    nrow(dat) < 30 ||
    n_distinct(dat$user_id) < 10 ||
    n_distinct(dat$context) < 2 ||
    is.na(safe_sd(dat[[outcome]])) ||
    is.na(safe_sd(dat[[feat_z]])) ||
    safe_sd(dat[[outcome]]) == 0 ||
    safe_sd(dat[[feat_z]]) == 0
  ) {
    return(NULL)
  }
  
  fml_1 <- as.formula(
    paste0(outcome, " ~ ", feat_z, " * context + (1 | user_id)")
  )
  
  mod <- fit_mixed_with_fallback(
    formulas = list(fml_1),
    data = dat
  )
  
  if (is.null(mod)) return(NULL)
  
  list(
    model = mod,
    data = dat,
    feature_z = feat_z
  )
}

extract_state_context_results <- function(fit, outcome, feature) {
  if (is.null(fit)) {
    return(tibble(
      outcome = outcome,
      outcome_label = nice_outcome_label(outcome),
      feature = feature,
      context = c("private", "public"),
      estimate = NA_real_,
      se = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      p.value = NA_real_,
      n_rows = NA_integer_,
      n_users = NA_integer_,
      singular = NA
    ))
  }
  
  coef_by_context_lmer(
    model = fit$model,
    term = fit$feature_z
  ) %>%
    mutate(
      outcome = outcome,
      outcome_label = nice_outcome_label(outcome),
      feature = feature,
      n_rows = nrow(fit$data),
      n_users = n_distinct(fit$data$user_id),
      singular = lme4::isSingular(fit$model, tol = 1e-4),
      .before = 1
    )
}

extract_state_interaction_results <- function(fit, outcome, feature) {
  if (is.null(fit)) {
    return(tibble(
      outcome = outcome,
      outcome_label = nice_outcome_label(outcome),
      feature = feature,
      term = "dictionary_z:contextpublic",
      estimate = NA_real_,
      se = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      p.value = NA_real_,
      n_rows = NA_integer_,
      n_users = NA_integer_,
      singular = NA
    ))
  }
  
  td <- broom.mixed::tidy(
    fit$model,
    effects = "fixed",
    conf.int = FALSE
  )
  
  term_1 <- paste0(fit$feature_z, ":contextpublic")
  term_2 <- paste0("contextpublic:", fit$feature_z)
  
  out <- td %>%
    filter(term %in% c(term_1, term_2)) %>%
    slice(1)
  
  if (nrow(out) == 0) {
    return(tibble(
      outcome = outcome,
      outcome_label = nice_outcome_label(outcome),
      feature = feature,
      term = "dictionary_z:contextpublic",
      estimate = NA_real_,
      se = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      p.value = NA_real_,
      n_rows = nrow(fit$data),
      n_users = n_distinct(fit$data$user_id),
      singular = lme4::isSingular(fit$model, tol = 1e-4)
    ))
  }
  
  out %>%
    transmute(
      outcome = outcome,
      outcome_label = nice_outcome_label(outcome),
      feature = feature,
      term = "dictionary_z:contextpublic",
      estimate = estimate,
      se = std.error,
      conf.low = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error,
      p.value = 2 * pnorm(abs(estimate / std.error), lower.tail = FALSE),
      n_rows = nrow(fit$data),
      n_users = n_distinct(fit$data$user_id),
      singular = lme4::isSingular(fit$model, tol = 1e-4)
    )
}

############################
#### 8) MAIN PAPER:
#### THEORY-GUIDED MODELS ACROSS ALL TIMESCALES
############################

trait_outcomes_main <- c("pa_trait_z", "na_trait_z")
daily_outcomes_main <- c("daily_valence_z")
momentary_outcomes_main <- c("momentary_valence_z")

# Ensure theory features are available in each dataset
theory_features_trait <- theory_features[
  paste0(theory_features, "_z") %in% names(keyboard_data_trait_z)
]

theory_features_day <- theory_features[
  paste0(theory_features, "_z") %in% names(keyboard_data_day_z)
]

theory_features_moment <- theory_features[
  paste0(theory_features, "_z") %in% names(keyboard_data_moment_z)
]

message("Theory features in trait data: ", paste(theory_features_trait, collapse = ", "))
message("Theory features in daily data: ", paste(theory_features_day, collapse = ", "))
message("Theory features in momentary data: ", paste(theory_features_moment, collapse = ", "))

# Trait models: OLS with cluster-robust SEs
theory_trait_context_results <- bind_rows(
  lapply(trait_outcomes_main, function(outcome_i) {
    bind_rows(lapply(theory_features_trait, function(feature_i) {
      fit_i <- fit_trait_dictionary_model(
        data = keyboard_data_trait_z,
        outcome = outcome_i,
        feature = feature_i
      )
      
      extract_trait_context_results(
        fit = fit_i,
        outcome = outcome_i,
        feature = feature_i
      )
    }))
  })
)

theory_trait_interaction_results <- bind_rows(
  lapply(trait_outcomes_main, function(outcome_i) {
    bind_rows(lapply(theory_features_trait, function(feature_i) {
      fit_i <- fit_trait_dictionary_model(
        data = keyboard_data_trait_z,
        outcome = outcome_i,
        feature = feature_i
      )
      
      extract_trait_interaction_results(
        fit = fit_i,
        outcome = outcome_i,
        feature = feature_i
      )
    }))
  })
)

# Daily models: mixed models with participant random intercepts
theory_daily_context_results <- bind_rows(
  lapply(daily_outcomes_main, function(outcome_i) {
    bind_rows(lapply(theory_features_day, function(feature_i) {
      fit_i <- fit_state_dictionary_model(
        data = keyboard_data_day_z,
        outcome = outcome_i,
        feature = feature_i
      )
      
      extract_state_context_results(
        fit = fit_i,
        outcome = outcome_i,
        feature = feature_i
      )
    }))
  })
)

theory_daily_interaction_results <- bind_rows(
  lapply(daily_outcomes_main, function(outcome_i) {
    bind_rows(lapply(theory_features_day, function(feature_i) {
      fit_i <- fit_state_dictionary_model(
        data = keyboard_data_day_z,
        outcome = outcome_i,
        feature = feature_i
      )
      
      extract_state_interaction_results(
        fit = fit_i,
        outcome = outcome_i,
        feature = feature_i
      )
    }))
  })
)

# Momentary models: mixed models with participant random intercepts
theory_momentary_context_results <- bind_rows(
  lapply(momentary_outcomes_main, function(outcome_i) {
    bind_rows(lapply(theory_features_moment, function(feature_i) {
      fit_i <- fit_state_dictionary_model(
        data = keyboard_data_moment_z,
        outcome = outcome_i,
        feature = feature_i
      )
      
      extract_state_context_results(
        fit = fit_i,
        outcome = outcome_i,
        feature = feature_i
      )
    }))
  })
)

theory_momentary_interaction_results <- bind_rows(
  lapply(momentary_outcomes_main, function(outcome_i) {
    bind_rows(lapply(theory_features_moment, function(feature_i) {
      fit_i <- fit_state_dictionary_model(
        data = keyboard_data_moment_z,
        outcome = outcome_i,
        feature = feature_i
      )
      
      extract_state_interaction_results(
        fit = fit_i,
        outcome = outcome_i,
        feature = feature_i
      )
    }))
  })
)

# Combine context-specific results
theory_context_results_all <- bind_rows(
  theory_trait_context_results %>%
    mutate(timescale = "Trait"),
  theory_daily_context_results %>%
    mutate(timescale = "Daily"),
  theory_momentary_context_results %>%
    mutate(timescale = "Momentary")
) %>%
  left_join(theory_feature_lookup, by = "feature") %>%
  mutate(
    context_label = nice_context(context),
    outcome_label = nice_outcome_label(outcome),
    outcome_label = factor(
      outcome_label,
      levels = c(
        "Trait NA",
        "Trait PA",
        "Daily valence",
        "Momentary valence"
      )
    ),
    label = factor(
      label,
      levels = unname(theory_feature_labels[theory_features])
    )
  ) %>%
  group_by(outcome, context) %>%
  mutate(p_fdr = p.adjust(p.value, method = "BH")) %>%
  ungroup() %>%
  arrange(outcome_label, feature, context)

# Combine interaction results
theory_interaction_results_all <- bind_rows(
  theory_trait_interaction_results %>%
    mutate(timescale = "Trait"),
  theory_daily_interaction_results %>%
    mutate(timescale = "Daily"),
  theory_momentary_interaction_results %>%
    mutate(timescale = "Momentary")
) %>%
  left_join(theory_feature_lookup, by = "feature") %>%
  mutate(
    outcome_label = nice_outcome_label(outcome),
    outcome_label = factor(
      outcome_label,
      levels = c(
        "Trait NA",
        "Trait PA",
        "Daily valence",
        "Momentary valence"
      )
    )
  ) %>%
  group_by(outcome) %>%
  mutate(p_fdr = p.adjust(p.value, method = "BH")) %>%
  ungroup() %>%
  arrange(outcome_label, p.value)

write.csv(
  theory_context_results_all,
  "results/main_dictionary_theory_all_timescales_context_specific_associations.csv",
  row.names = FALSE
)

write.csv(
  theory_interaction_results_all,
  "results/main_dictionary_theory_all_timescales_context_interactions.csv",
  row.names = FALSE
)

############################
#### 9) MAIN PAPER FIGURE:
#### THEORY-GUIDED ASSOCIATIONS ACROSS ALL TIMESCALES
############################

df_all_plot <- theory_context_results_all %>%
  filter(!is.na(estimate), !is.na(conf.low), !is.na(conf.high)) %>%
  mutate(
    context = factor(context, levels = c("private", "public")),
    context_label = factor(nice_context(context), levels = c("Private", "Public")),
    outcome_label = factor(
      outcome_label,
      levels = c(
        "Trait NA",
        "Trait PA",
        "Daily valence",
        "Momentary valence"
      )
    ),
    label = factor(
      label,
      levels = unname(theory_feature_labels[theory_features])
    ),
    sig_ci = if_else(
      conf.low > 0 | conf.high < 0,
      "CI excludes 0",
      "CI includes 0"
    )
  )

# Set plot order manually for psychological readability
feature_order_main <- c(
  "Pos. emotion",
  "Neg. emotion",
  "I",
  "We"
)

feature_order_main <- feature_order_main[
  feature_order_main %in% unique(as.character(df_all_plot$label))
]

df_all_plot <- df_all_plot %>%
  mutate(
    label = factor(label, levels = feature_order_main)
  )

# Optional: create wide data for private-public differences,
# useful for checking context gaps or supplement tables.
df_all_context_wide <- df_all_plot %>%
  select(outcome, outcome_label, label, context, estimate) %>%
  tidyr::pivot_wider(
    names_from = context,
    values_from = estimate
  ) %>%
  mutate(
    private_minus_public = private - public
  )

write.csv(
  df_all_context_wide,
  "results/figure_dictionary_theory_all_timescales_private_public_differences.csv",
  row.names = FALSE
)

context_cols <- c(
  "Private" = "#E69F00",
  "Public"  = "#56B4E9"
)

context_shapes <- c(
  "Private" = 16,
  "Public"  = 17
)

pd_context <- position_dodge(width = 0.18)

fig_dictionary_all_timescales <- ggplot(
  df_all_plot,
  aes(
    x = label,
    y = estimate,
    color = context_label,
    shape = context_label,
    group = context_label
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray75",
    linewidth = 0.45
  ) +
  
  geom_linerange(
    aes(
      ymin = conf.low,
      ymax = conf.high
    ),
    linewidth = 0.65,
    alpha = 0.35,
    position = pd_context
  ) +
  
  geom_point(
    size = 2.8,
    alpha = 0.95,
    position = pd_context
  ) +
  
  facet_wrap(
    ~ outcome_label,
    ncol = 1,
    labeller = as_labeller(c(
      "Trait NA" = "A) Trait negative affect",
      "Trait PA" = "B) Trait positive affect",
      "Daily valence" = "C) Daily valence",
      "Momentary valence" = "D) Momentary valence"
    ))
  ) +
  
  scale_color_manual(
    values = context_cols,
    name = "Context"
  ) +
  scale_shape_manual(
    values = context_shapes,
    name = "Context"
  ) +
  
  scale_y_continuous(
    limits = c(-0.30, 0.30),
    breaks = seq(-0.30, 0.30, by = 0.1),
    labels = scales::label_number(accuracy = 0.1, trim = TRUE),
    oob = scales::squish
  ) +
  
  labs(
    x = NULL,
    y = "Standardized regression coefficient"
  ) +
  
  base_theme +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title = element_text(size = 10.5, face = "bold"),
    legend.text = element_text(size = 10),
    legend.margin = margin(b = -4),
    
    strip.background = element_rect(
      fill = "gray96",
      color = "gray60",
      linewidth = 0.7
    ),
    strip.text = element_text(
      face = "bold",
      size = 10.5
    ),
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1,
      size = 9
    ),
    axis.text.y = element_text(size = 9.2),
    axis.title.y = element_text(size = 10.5),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing.y = unit(1.0, "lines"),
    
    plot.margin = margin(8, 10, 6, 8)
  ) +
  guides(
    color = guide_legend(
      order = 1,
      override.aes = list(size = 3.2, alpha = 1)
    ),
    shape = guide_legend(
      order = 1,
      override.aes = list(size = 3.2, alpha = 1)
    )
  )

fig_dictionary_all_timescales

ggsave(
  filename = "figures/figure_4_dictionary_theory_all_timescales_vertical.png",
  plot = fig_dictionary_all_timescales,
  width = 6,
  height = 10,
  dpi = 300
)


write.csv(
  df_all_plot,
  "results/figure_4_dictionary_theory_all_timescales_plot_data.csv",
  row.names = FALSE
)


############################
#### 10) SUPPLEMENT:
#### THEORY-GUIDED DAILY AND MOMENTARY MODELS
############################

theory_features_day <- intersect(theory_features, names(keyboard_data_day))
theory_features_moment <- intersect(theory_features, names(keyboard_data_moment))

daily_theory_context_results <- bind_rows(
  lapply(theory_features_day, function(feature_i) {
    fit_i <- fit_state_dictionary_model(
      data = keyboard_data_day_z,
      outcome = "daily_valence_z",
      feature = feature_i
    )
    
    extract_state_context_results(
      fit = fit_i,
      outcome = "daily_valence_z",
      feature = feature_i
    )
  })
) %>%
  left_join(theory_feature_lookup, by = "feature") %>%
  group_by(context) %>%
  mutate(p_fdr = p.adjust(p.value, method = "BH")) %>%
  ungroup()

daily_theory_interaction_results <- bind_rows(
  lapply(theory_features_day, function(feature_i) {
    fit_i <- fit_state_dictionary_model(
      data = keyboard_data_day_z,
      outcome = "daily_valence_z",
      feature = feature_i
    )
    
    extract_state_interaction_results(
      fit = fit_i,
      outcome = "daily_valence_z",
      feature = feature_i
    )
  })
) %>%
  left_join(theory_feature_lookup, by = "feature") %>%
  mutate(p_fdr = p.adjust(p.value, method = "BH"))

momentary_theory_context_results <- bind_rows(
  lapply(theory_features_moment, function(feature_i) {
    fit_i <- fit_state_dictionary_model(
      data = keyboard_data_moment_z,
      outcome = "momentary_valence_z",
      feature = feature_i
    )
    
    extract_state_context_results(
      fit = fit_i,
      outcome = "momentary_valence_z",
      feature = feature_i
    )
  })
) %>%
  left_join(theory_feature_lookup, by = "feature") %>%
  group_by(context) %>%
  mutate(p_fdr = p.adjust(p.value, method = "BH")) %>%
  ungroup()

momentary_theory_interaction_results <- bind_rows(
  lapply(theory_features_moment, function(feature_i) {
    fit_i <- fit_state_dictionary_model(
      data = keyboard_data_moment_z,
      outcome = "momentary_valence_z",
      feature = feature_i
    )
    
    extract_state_interaction_results(
      fit = fit_i,
      outcome = "momentary_valence_z",
      feature = feature_i
    )
  })
) %>%
  left_join(theory_feature_lookup, by = "feature") %>%
  mutate(p_fdr = p.adjust(p.value, method = "BH"))

write.csv(
  daily_theory_context_results,
  "results/supp_dictionary_theory_daily_context_specific_associations.csv",
  row.names = FALSE
)

write.csv(
  daily_theory_interaction_results,
  "results/supp_dictionary_theory_daily_context_interactions.csv",
  row.names = FALSE
)

write.csv(
  momentary_theory_context_results,
  "results/supp_dictionary_theory_momentary_context_specific_associations.csv",
  row.names = FALSE
)

write.csv(
  momentary_theory_interaction_results,
  "results/supp_dictionary_theory_momentary_context_interactions.csv",
  row.names = FALSE
)



############################
#### 11) SUPPLEMENT FIGURE:
#### THEORY-GUIDED DAILY + MOMENTARY
############################

state_theory_plot_df <- bind_rows(
  daily_theory_context_results,
  momentary_theory_context_results
) %>%
  filter(!is.na(estimate), !is.na(conf.low), !is.na(conf.high)) %>%
  mutate(
    context = factor(context, levels = c("private", "public")),
    context_label = factor(nice_context(context), levels = c("Private", "Public")),
    outcome_label = factor(
      outcome_label,
      levels = c("Daily valence", "Momentary valence")
    ),
    label = factor(label, levels = rev(unname(theory_feature_labels[theory_features]))),
    sig_ci = if_else(conf.low > 0 | conf.high < 0, "CI excludes 0", "CI includes 0")
  )

if (nrow(state_theory_plot_df) > 0) {
  axis_lim_state <- max(
    abs(c(state_theory_plot_df$conf.low, state_theory_plot_df$conf.high)),
    na.rm = TRUE
  )
  
  axis_lim_state <- max(0.30, ceiling(axis_lim_state * 10) / 10)
  axis_lim_state <- min(axis_lim_state, 0.60)
  
  fig_dictionary_state_supp <- ggplot(
    state_theory_plot_df,
    aes(
      x = estimate,
      y = label,
      xmin = conf.low,
      xmax = conf.high,
      shape = context_label,
      alpha = sig_ci
    )
  ) +
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      color = "gray65",
      linewidth = 0.45
    ) +
    geom_errorbarh(
      position = position_dodge(width = 0.65),
      height = 0,
      linewidth = 0.75,
      color = "gray35"
    ) +
    geom_point(
      position = position_dodge(width = 0.65),
      size = 3.1,
      color = "black"
    ) +
    facet_wrap(~ outcome_label, ncol = 1) +
    scale_shape_manual(
      values = c("Private" = 16, "Public" = 17),
      name = "Context"
    ) +
    scale_alpha_manual(
      values = c(
        "CI excludes 0" = 1.0,
        "CI includes 0" = 0.45
      ),
      guide = "none"
    ) +
    scale_x_continuous(
      limits = c(-axis_lim_state, axis_lim_state),
      breaks = seq(-axis_lim_state, axis_lim_state, by = 0.1),
      labels = scales::label_number(accuracy = 0.1, trim = TRUE)
    ) +
    labs(
      x = "Standardized coefficient (\u03b2) with 95% CI",
      y = NULL
    ) +
    base_theme +
    theme(
      legend.position = "top",
      strip.text = element_text(face = "bold"),
      plot.margin = margin(8, 12, 8, 8)
    )
  
  fig_dictionary_state_supp
  
  ggsave(
    filename = "figures/supp_dictionary_theory_daily_momentary_associations.png",
    plot = fig_dictionary_state_supp,
    width = 7.2,
    height = 6.8,
    dpi = 300
  )
  
  ggsave(
    filename = "figures/supp_dictionary_theory_daily_momentary_associations.pdf",
    plot = fig_dictionary_state_supp,
    width = 7.2,
    height = 6.8
  )
}





############################
#### 12) SUPPLEMENT:
#### EXPLORATORY ALL-LIWC TRAIT ANALYSES
############################

all_liwc_trait_context_results <- bind_rows(
  lapply(trait_outcomes_main, function(outcome_i) {
    bind_rows(lapply(all_liwc_features, function(feature_i) {
      fit_i <- fit_trait_dictionary_model(
        data = keyboard_data_trait_z,
        outcome = outcome_i,
        feature = feature_i
      )
      
      extract_trait_context_results(
        fit = fit_i,
        outcome = outcome_i,
        feature = feature_i
      )
    }))
  })
) %>%
  left_join(liwc_feature_lookup, by = "feature") %>%
  group_by(outcome, context) %>%
  mutate(p_fdr = p.adjust(p.value, method = "BH")) %>%
  ungroup() %>%
  arrange(outcome_label, context, p.value)

all_liwc_trait_interaction_results <- bind_rows(
  lapply(trait_outcomes_main, function(outcome_i) {
    bind_rows(lapply(all_liwc_features, function(feature_i) {
      fit_i <- fit_trait_dictionary_model(
        data = keyboard_data_trait_z,
        outcome = outcome_i,
        feature = feature_i
      )
      
      extract_trait_interaction_results(
        fit = fit_i,
        outcome = outcome_i,
        feature = feature_i
      )
    }))
  })
) %>%
  left_join(liwc_feature_lookup, by = "feature") %>%
  group_by(outcome) %>%
  mutate(p_fdr = p.adjust(p.value, method = "BH")) %>%
  ungroup() %>%
  arrange(outcome_label, p.value)

write.csv(
  all_liwc_trait_context_results,
  "results/supp_all_liwc_trait_context_specific_associations.csv",
  row.names = FALSE
)

write.csv(
  all_liwc_trait_interaction_results,
  "results/supp_all_liwc_trait_context_interactions.csv",
  row.names = FALSE
)

############################
#### 13) SUPPLEMENT:
#### EXPLORATORY ALL-LIWC DAILY AND MOMENTARY ANALYSES
############################

all_liwc_daily_context_results <- bind_rows(
  lapply(all_liwc_features, function(feature_i) {
    fit_i <- fit_state_dictionary_model(
      data = keyboard_data_day_z,
      outcome = "daily_valence_z",
      feature = feature_i
    )
    
    extract_state_context_results(
      fit = fit_i,
      outcome = "daily_valence_z",
      feature = feature_i
    )
  })
) %>%
  left_join(liwc_feature_lookup, by = "feature") %>%
  group_by(context) %>%
  mutate(p_fdr = p.adjust(p.value, method = "BH")) %>%
  ungroup() %>%
  arrange(context, p.value)

all_liwc_daily_interaction_results <- bind_rows(
  lapply(all_liwc_features, function(feature_i) {
    fit_i <- fit_state_dictionary_model(
      data = keyboard_data_day_z,
      outcome = "daily_valence_z",
      feature = feature_i
    )
    
    extract_state_interaction_results(
      fit = fit_i,
      outcome = "daily_valence_z",
      feature = feature_i
    )
  })
) %>%
  left_join(liwc_feature_lookup, by = "feature") %>%
  mutate(p_fdr = p.adjust(p.value, method = "BH")) %>%
  arrange(p.value)

all_liwc_momentary_context_results <- bind_rows(
  lapply(all_liwc_features, function(feature_i) {
    fit_i <- fit_state_dictionary_model(
      data = keyboard_data_moment_z,
      outcome = "momentary_valence_z",
      feature = feature_i
    )
    
    extract_state_context_results(
      fit = fit_i,
      outcome = "momentary_valence_z",
      feature = feature_i
    )
  })
) %>%
  left_join(liwc_feature_lookup, by = "feature") %>%
  group_by(context) %>%
  mutate(p_fdr = p.adjust(p.value, method = "BH")) %>%
  ungroup() %>%
  arrange(context, p.value)

all_liwc_momentary_interaction_results <- bind_rows(
  lapply(all_liwc_features, function(feature_i) {
    fit_i <- fit_state_dictionary_model(
      data = keyboard_data_moment_z,
      outcome = "momentary_valence_z",
      feature = feature_i
    )
    
    extract_state_interaction_results(
      fit = fit_i,
      outcome = "momentary_valence_z",
      feature = feature_i
    )
  })
) %>%
  left_join(liwc_feature_lookup, by = "feature") %>%
  mutate(p_fdr = p.adjust(p.value, method = "BH")) %>%
  arrange(p.value)

write.csv(
  all_liwc_daily_context_results,
  "results/supp_all_liwc_daily_context_specific_associations.csv",
  row.names = FALSE
)

write.csv(
  all_liwc_daily_interaction_results,
  "results/supp_all_liwc_daily_context_interactions.csv",
  row.names = FALSE
)

write.csv(
  all_liwc_momentary_context_results,
  "results/supp_all_liwc_momentary_context_specific_associations.csv",
  row.names = FALSE
)

write.csv(
  all_liwc_momentary_interaction_results,
  "results/supp_all_liwc_momentary_context_interactions.csv",
  row.names = FALSE
)

############################
#### 14) SUPPLEMENT FIGURE:
#### TOP EXPLORATORY LIWC TRAIT ASSOCIATIONS
############################

# Select top LIWC categories by largest absolute context-specific coefficient
# across Trait PA and Trait NA. This is exploratory and should be framed as such.

top_n_liwc <- 10

top_liwc_trait_features <- all_liwc_trait_context_results %>%
  filter(!is.na(estimate)) %>%
  group_by(feature, label) %>%
  summarise(
    rank_score = max(abs(estimate), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  slice_max(order_by = rank_score, n = top_n_liwc, with_ties = FALSE)

top_liwc_trait_plot_df <- all_liwc_trait_context_results %>%
  semi_join(top_liwc_trait_features, by = c("feature", "label")) %>%
  filter(!is.na(estimate), !is.na(conf.low), !is.na(conf.high)) %>%
  mutate(
    context_label = factor(nice_context(context), levels = c("Private", "Public")),
    outcome_label = factor(outcome_label, levels = c("Trait PA", "Trait NA")),
    label = forcats::fct_reorder(label, abs(estimate), .fun = max, .desc = FALSE),
    sig_ci = if_else(conf.low > 0 | conf.high < 0, "CI excludes 0", "CI includes 0")
  )

write.csv(
  top_liwc_trait_plot_df,
  "results/supp_top_all_liwc_trait_associations_plot_data.csv",
  row.names = FALSE
)

if (nrow(top_liwc_trait_plot_df) > 0) {
  axis_lim_top_liwc <- max(
    abs(c(top_liwc_trait_plot_df$conf.low, top_liwc_trait_plot_df$conf.high)),
    na.rm = TRUE
  )
  
  axis_lim_top_liwc <- max(0.30, ceiling(axis_lim_top_liwc * 10) / 10)
  axis_lim_top_liwc <- min(axis_lim_top_liwc, 0.80)
  
  fig_supp_top_liwc_trait <- ggplot(
    top_liwc_trait_plot_df,
    aes(
      x = estimate,
      y = label,
      xmin = conf.low,
      xmax = conf.high,
      shape = context_label,
      alpha = sig_ci
    )
  ) +
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      color = "gray65",
      linewidth = 0.45
    ) +
    geom_errorbarh(
      position = position_dodge(width = 0.65),
      height = 0,
      linewidth = 0.75,
      color = "gray35"
    ) +
    geom_point(
      position = position_dodge(width = 0.65),
      size = 3.0,
      color = "black"
    ) +
    facet_wrap(~ outcome_label, ncol = 1) +
    scale_shape_manual(
      values = c("Private" = 16, "Public" = 17),
      name = "Context"
    ) +
    scale_alpha_manual(
      values = c(
        "CI excludes 0" = 1.0,
        "CI includes 0" = 0.45
      ),
      guide = "none"
    ) +
    scale_x_continuous(
      limits = c(-axis_lim_top_liwc, axis_lim_top_liwc),
      breaks = seq(-axis_lim_top_liwc, axis_lim_top_liwc, by = 0.1),
      labels = scales::label_number(accuracy = 0.1, trim = TRUE)
    ) +
    labs(
      x = "Standardized coefficient (\u03b2) with 95% CI",
      y = NULL
    ) +
    base_theme +
    theme(
      legend.position = "top",
      strip.text = element_text(face = "bold"),
      plot.margin = margin(8, 12, 8, 8)
    )
  
  fig_supp_top_liwc_trait
  
  ggsave(
    filename = "figures/supp_top_all_liwc_trait_associations.png",
    plot = fig_supp_top_liwc_trait,
    width = 7.4,
    height = 7.8,
    dpi = 300
  )
  
  ggsave(
    filename = "figures/supp_top_all_liwc_trait_associations.pdf",
    plot = fig_supp_top_liwc_trait,
    width = 7.4,
    height = 7.8
  )
}

############################
#### 15) SUPPLEMENT FIGURE:
#### DISTRIBUTION OF |BETA| ACROSS ALL LIWC FEATURES
############################

# This figure gives a compact descriptive map of exploratory LIWC signal
# across timescales and contexts. Use only in supplement if helpful.

all_liwc_abs_trait <- all_liwc_trait_context_results %>%
  filter(!is.na(estimate)) %>%
  mutate(
    timescale = "Trait",
    abs_estimate = abs(estimate)
  ) %>%
  group_by(feature, context, timescale) %>%
  summarise(
    abs_estimate = mean(abs_estimate, na.rm = TRUE),
    .groups = "drop"
  )

all_liwc_abs_daily <- all_liwc_daily_context_results %>%
  filter(!is.na(estimate)) %>%
  mutate(
    timescale = "Daily",
    abs_estimate = abs(estimate)
  ) %>%
  select(feature, context, timescale, abs_estimate)

all_liwc_abs_momentary <- all_liwc_momentary_context_results %>%
  filter(!is.na(estimate)) %>%
  mutate(
    timescale = "Momentary",
    abs_estimate = abs(estimate)
  ) %>%
  select(feature, context, timescale, abs_estimate)

all_liwc_abs_summary <- bind_rows(
  all_liwc_abs_trait,
  all_liwc_abs_daily,
  all_liwc_abs_momentary
) %>%
  mutate(
    timescale = factor(timescale, levels = c("Trait", "Daily", "Momentary")),
    context_label = factor(nice_context(context), levels = c("Private", "Public"))
  )

all_liwc_abs_means <- all_liwc_abs_summary %>%
  group_by(timescale, context_label) %>%
  summarise(
    mean_abs = mean(abs_estimate, na.rm = TRUE),
    median_abs = median(abs_estimate, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(
  all_liwc_abs_summary,
  "results/supp_all_liwc_abs_beta_by_feature_timescale_context.csv",
  row.names = FALSE
)

write.csv(
  all_liwc_abs_means,
  "results/supp_all_liwc_abs_beta_means_by_timescale_context.csv",
  row.names = FALSE
)

if (nrow(all_liwc_abs_summary) > 0) {
  fig_supp_all_liwc_abs <- ggplot(
    all_liwc_abs_summary,
    aes(
      x = context_label,
      y = abs_estimate,
      fill = context_label,
      color = context_label
    )
  ) +
    geom_boxplot(
      width = 0.55,
      alpha = 0.18,
      outlier.shape = NA,
      linewidth = 0.7
    ) +
    geom_jitter(
      width = 0.08,
      size = 1.1,
      alpha = 0.30
    ) +
    geom_point(
      data = all_liwc_abs_means,
      aes(
        x = context_label,
        y = mean_abs
      ),
      inherit.aes = FALSE,
      shape = 18,
      size = 3.6,
      color = "black"
    ) +
    geom_text(
      data = all_liwc_abs_means,
      aes(
        x = context_label,
        y = mean_abs,
        label = sprintf("%.3f", mean_abs)
      ),
      inherit.aes = FALSE,
      vjust = -0.9,
      size = 3.2,
      color = "black"
    ) +
    scale_fill_manual(
      values = c("Private" = "gray65", "Public" = "gray35"),
      guide = "none"
    ) +
    scale_color_manual(
      values = c("Private" = "gray65", "Public" = "gray35"),
      guide = "none"
    ) +
    facet_wrap(~ timescale, nrow = 1) +
    labs(
      x = NULL,
      y = "Absolute standardized coefficient (|β|)"
    ) +
    base_theme +
    theme(
      strip.background = element_rect(
        fill = "gray97",
        color = "gray40",
        linewidth = 0.6
      ),
      strip.text = element_text(face = "bold", size = 11.2)
    )
  
  fig_supp_all_liwc_abs
  
  ggsave(
    filename = "figures/supp_all_liwc_abs_beta_distribution.png",
    plot = fig_supp_all_liwc_abs,
    width = 7.5,
    height = 4.2,
    dpi = 300
  )
  
}

############################
#### 16) ROUNDED TABLES FOR MANUSCRIPT / SUPPLEMENT
############################

main_theory_trait_table_rounded <- theory_trait_context_results %>%
  transmute(
    outcome = outcome_label,
    feature_label = label,
    feature = feature,
    context = nice_context(context),
    beta = round(estimate, 3),
    se = round(se, 3),
    ci_low = round(conf.low, 3),
    ci_high = round(conf.high, 3),
    p = signif(p.value, 3),
    p_fdr = signif(p_fdr, 3),
    n_rows = n_rows,
    n_users = n_users
  ) %>%
  arrange(outcome, feature, context)

main_theory_trait_interaction_table_rounded <- theory_trait_interaction_results %>%
  transmute(
    outcome = outcome_label,
    feature_label = label,
    feature = feature,
    beta_interaction_public_minus_private = round(estimate, 3),
    se = round(se, 3),
    ci_low = round(conf.low, 3),
    ci_high = round(conf.high, 3),
    p = signif(p.value, 3),
    p_fdr = signif(p_fdr, 3),
    n_rows = n_rows,
    n_users = n_users
  ) %>%
  arrange(outcome, feature)

supp_theory_state_context_table_rounded <- bind_rows(
  daily_theory_context_results,
  momentary_theory_context_results
) %>%
  transmute(
    outcome = outcome_label,
    feature_label = label,
    feature = feature,
    context = nice_context(context),
    beta = round(estimate, 3),
    se = round(se, 3),
    ci_low = round(conf.low, 3),
    ci_high = round(conf.high, 3),
    p = signif(p.value, 3),
    p_fdr = signif(p_fdr, 3),
    n_rows = n_rows,
    n_users = n_users,
    singular = singular
  ) %>%
  arrange(outcome, feature, context)

supp_all_liwc_trait_context_table_rounded <- all_liwc_trait_context_results %>%
  transmute(
    outcome = outcome_label,
    feature = feature,
    context = nice_context(context),
    beta = round(estimate, 3),
    se = round(se, 3),
    ci_low = round(conf.low, 3),
    ci_high = round(conf.high, 3),
    p = signif(p.value, 3),
    p_fdr = signif(p_fdr, 3),
    n_rows = n_rows,
    n_users = n_users
  ) %>%
  arrange(outcome, context, p)

supp_all_liwc_state_context_table_rounded <- bind_rows(
  all_liwc_daily_context_results,
  all_liwc_momentary_context_results
) %>%
  transmute(
    outcome = outcome_label,
    feature = feature,
    context = nice_context(context),
    beta = round(estimate, 3),
    se = round(se, 3),
    ci_low = round(conf.low, 3),
    ci_high = round(conf.high, 3),
    p = signif(p.value, 3),
    p_fdr = signif(p_fdr, 3),
    n_rows = n_rows,
    n_users = n_users,
    singular = singular
  ) %>%
  arrange(outcome, context, p)

write.csv(
  main_theory_trait_table_rounded,
  "results/main_dictionary_theory_trait_context_specific_associations_rounded.csv",
  row.names = FALSE
)

write.csv(
  main_theory_trait_interaction_table_rounded,
  "results/main_dictionary_theory_trait_context_interactions_rounded.csv",
  row.names = FALSE
)

write.csv(
  supp_theory_state_context_table_rounded,
  "results/supp_dictionary_theory_daily_momentary_context_specific_associations_rounded.csv",
  row.names = FALSE
)

write.csv(
  supp_all_liwc_trait_context_table_rounded,
  "results/supp_all_liwc_trait_context_specific_associations_rounded.csv",
  row.names = FALSE
)

write.csv(
  supp_all_liwc_state_context_table_rounded,
  "results/supp_all_liwc_daily_momentary_context_specific_associations_rounded.csv",
  row.names = FALSE
)

# finish