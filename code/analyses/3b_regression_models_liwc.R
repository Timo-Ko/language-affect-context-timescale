############################
#### LIWC / DICTIONARY × AFFECT ANALYSES
#### FIGURE 4 + TABLE S7 + FULL REPOSITORY CSV
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
  "grid"
)

invisible(lapply(required_packages, library, character.only = TRUE))

dir.create("results", recursive = TRUE, showWarnings = FALSE)
dir.create("figures", recursive = TRUE, showWarnings = FALSE)

source("code/analyses/helper/plot_theme.R")
base_theme <- theme_custom(base_size = 12)

set.seed(123)

############################
#### 1) HELPER FUNCTIONS
############################

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

fmt_beta_ci <- function(beta, low, high, digits = 2) {
  ifelse(
    is.na(beta) | is.na(low) | is.na(high),
    NA_character_,
    paste0(
      formatC(beta, format = "f", digits = digits),
      " [",
      formatC(low, format = "f", digits = digits),
      ", ",
      formatC(high, format = "f", digits = digits),
      "]"
    )
  )
}

############################
#### 2) LOAD DATA
############################

keyboard_data_trait <- readRDS("data/results/keyboard_data_trait_final.rds") %>%
  as.data.frame() %>%
  filter(scope %in% c("private", "public")) %>%
  mutate(
    user_id = as.character(user_id),
    context = factor(scope, levels = c("private", "public"))
  )

keyboard_data_day <- readRDS("data/results/keyboard_data_day_final.rds") %>%
  as.data.frame() %>%
  filter(scope %in% c("private", "public")) %>%
  mutate(
    user_id = as.character(user_id),
    context = factor(scope, levels = c("private", "public")),
    date = as.Date(date)
  )

keyboard_data_moment <- readRDS("data/results/keyboard_data_ema_final.rds") %>%
  as.data.frame() %>%
  filter(scope %in% c("private", "public")) %>%
  mutate(
    user_id = as.character(user_id),
    context = factor(scope, levels = c("private", "public"))
  )

stopifnot(all(c("user_id", "scope", "pa_panas", "na_panas") %in% names(keyboard_data_trait)))
stopifnot(all(c("user_id", "scope", "daily_valence") %in% names(keyboard_data_day)))
stopifnot(all(c("user_id", "scope", "valence") %in% names(keyboard_data_moment)))

############################
#### 3) FEATURE DEFINITIONS
############################

# Theory-guided LIWC share features used in the main regression analyses.
theory_feature_labels <- c(
  "liwc_posemo" = "Pos. emotion",
  "liwc_negemo" = "Neg. emotion",
  "liwc_i"      = "I",
  "liwc_we"     = "We"
)

theory_features <- names(theory_feature_labels)

# All LIWC share features for repository export.
# Excludes session-derived summary variables.
liwc_share_features_trait <- names(keyboard_data_trait) %>%
  str_subset("^liwc_") %>%
  .[!str_detect(., "_(mean|sd|min|max)$")] %>%
  .[sapply(keyboard_data_trait[.], is.numeric)]

liwc_share_features_day <- names(keyboard_data_day) %>%
  str_subset("^liwc_") %>%
  .[!str_detect(., "_(mean|sd|min|max)$")] %>%
  .[sapply(keyboard_data_day[.], is.numeric)]

liwc_share_features_moment <- names(keyboard_data_moment) %>%
  str_subset("^liwc_") %>%
  .[!str_detect(., "_(mean|sd|min|max)$")] %>%
  .[sapply(keyboard_data_moment[.], is.numeric)]

all_liwc_features <- Reduce(
  intersect,
  list(
    liwc_share_features_trait,
    liwc_share_features_day,
    liwc_share_features_moment
  )
)

missing_theory <- setdiff(theory_features, all_liwc_features)

if (length(missing_theory) > 0) {
  stop("Missing theory-guided LIWC features: ", paste(missing_theory, collapse = ", "))
}

message("Number of shared LIWC share features: ", length(all_liwc_features))

theory_feature_lookup <- tibble(
  feature = theory_features,
  feature_label = unname(theory_feature_labels[theory_features])
)

all_liwc_feature_lookup <- tibble(
  feature = all_liwc_features,
  feature_label = all_liwc_features
)

write.csv(
  theory_feature_lookup,
  "results/theory_guided_liwc_feature_lookup.csv",
  row.names = FALSE
)

write.csv(
  all_liwc_feature_lookup,
  "results/all_liwc_share_feature_lookup.csv",
  row.names = FALSE
)

############################
#### 4) PREPARE ANALYSIS DATA
############################

# Standardize trait outcomes once at participant level.
trait_outcomes <- keyboard_data_trait %>%
  distinct(user_id, pa_panas, na_panas) %>%
  filter(!is.na(pa_panas), !is.na(na_panas)) %>%
  mutate(
    pa_trait_z = z_scale(pa_panas),
    na_trait_z = z_scale(na_panas)
  ) %>%
  select(user_id, pa_trait_z, na_trait_z)

keyboard_data_trait_z <- keyboard_data_trait %>%
  select(user_id, scope, context, all_of(all_liwc_features)) %>%
  left_join(trait_outcomes, by = "user_id") %>%
  mutate(
    across(
      all_of(all_liwc_features),
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
      all_of(all_liwc_features),
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
      all_of(all_liwc_features),
      z_scale,
      .names = "{.col}_z"
    )
  )

############################
#### 5) MODEL HELPERS: TRAIT MODELS
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

fit_trait_dictionary_model <- function(
  data,
  outcome,
  feature,
  require_both_contexts = FALSE
) {
  feat_z <- paste0(feature, "_z")
  
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
  
  if (require_both_contexts) {
    dat <- dat %>%
      group_by(user_id) %>%
      filter(
        n_distinct(context) == 2,
        n() == 2
      ) %>%
      ungroup()
  }
  
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
      n_users = NA_integer_,
      model_type = "lm_cluster_robust"
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
      model_type = "lm_cluster_robust",
      .before = 1
    )
}

extract_trait_interaction_results <- function(fit, outcome, feature) {
  if (is.null(fit)) {
    return(tibble(
      outcome = outcome,
      outcome_label = nice_outcome_label(outcome),
      feature = feature,
      estimate = NA_real_,
      se = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      p.value = NA_real_,
      n_rows = NA_integer_,
      n_users = NA_integer_,
      model_type = "lm_cluster_robust"
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
      estimate = NA_real_,
      se = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      p.value = NA_real_,
      n_rows = nrow(fit$data),
      n_users = n_distinct(fit$data$user_id),
      model_type = "lm_cluster_robust"
    ))
  }
  
  out %>%
    transmute(
      outcome = outcome,
      outcome_label = nice_outcome_label(outcome),
      feature = feature,
      estimate = estimate,
      se = std.error,
      conf.low = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error,
      p.value = p.value,
      n_rows = nrow(fit$data),
      n_users = n_distinct(fit$data$user_id),
      model_type = "lm_cluster_robust"
    )
}

############################
#### 6) MODEL HELPERS: DAILY / MOMENTARY MIXED MODELS
############################

fit_mixed_model <- function(formula, data) {
  tryCatch(
    suppressWarnings(
      lme4::lmer(
        formula = formula,
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
  
  fml <- as.formula(
    paste0(outcome, " ~ ", feat_z, " * context + (1 | user_id)")
  )
  
  mod <- fit_mixed_model(fml, dat)
  
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
      singular = NA,
      model_type = "lmer_random_intercept"
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
      model_type = "lmer_random_intercept",
      .before = 1
    )
}

extract_state_interaction_results <- function(fit, outcome, feature) {
  if (is.null(fit)) {
    return(tibble(
      outcome = outcome,
      outcome_label = nice_outcome_label(outcome),
      feature = feature,
      estimate = NA_real_,
      se = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      p.value = NA_real_,
      n_rows = NA_integer_,
      n_users = NA_integer_,
      singular = NA,
      model_type = "lmer_random_intercept"
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
      estimate = NA_real_,
      se = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      p.value = NA_real_,
      n_rows = nrow(fit$data),
      n_users = n_distinct(fit$data$user_id),
      singular = lme4::isSingular(fit$model, tol = 1e-4),
      model_type = "lmer_random_intercept"
    ))
  }
  
  out %>%
    transmute(
      outcome = outcome,
      outcome_label = nice_outcome_label(outcome),
      feature = feature,
      estimate = estimate,
      se = std.error,
      conf.low = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error,
      p.value = 2 * pnorm(abs(estimate / std.error), lower.tail = FALSE),
      n_rows = nrow(fit$data),
      n_users = n_distinct(fit$data$user_id),
      singular = lme4::isSingular(fit$model, tol = 1e-4),
      model_type = "lmer_random_intercept"
    )
}

############################
#### 7) RUN MODELS
############################

run_context_models <- function(
  data,
  outcomes,
  features,
  model_family,
  require_both_contexts = FALSE
) {
  bind_rows(lapply(outcomes, function(outcome_i) {
    bind_rows(lapply(features, function(feature_i) {
      
      if (model_family == "trait") {
        fit_i <- fit_trait_dictionary_model(
          data = data,
          outcome = outcome_i,
          feature = feature_i,
          require_both_contexts = require_both_contexts
        )
        
        extract_trait_context_results(
          fit = fit_i,
          outcome = outcome_i,
          feature = feature_i
        )
      } else {
        fit_i <- fit_state_dictionary_model(
          data = data,
          outcome = outcome_i,
          feature = feature_i
        )
        
        extract_state_context_results(
          fit = fit_i,
          outcome = outcome_i,
          feature = feature_i
        )
      }
    }))
  }))
}

run_interaction_models <- function(
  data,
  outcomes,
  features,
  model_family,
  require_both_contexts = FALSE
) {
  bind_rows(lapply(outcomes, function(outcome_i) {
    bind_rows(lapply(features, function(feature_i) {
      
      if (model_family == "trait") {
        fit_i <- fit_trait_dictionary_model(
          data = data,
          outcome = outcome_i,
          feature = feature_i,
          require_both_contexts = require_both_contexts
        )
        
        extract_trait_interaction_results(
          fit = fit_i,
          outcome = outcome_i,
          feature = feature_i
        )
      } else {
        fit_i <- fit_state_dictionary_model(
          data = data,
          outcome = outcome_i,
          feature = feature_i
        )
        
        extract_state_interaction_results(
          fit = fit_i,
          outcome = outcome_i,
          feature = feature_i
        )
      }
    }))
  }))
}

trait_outcomes <- c("pa_trait_z", "na_trait_z")
daily_outcomes <- c("daily_valence_z")
momentary_outcomes <- c("momentary_valence_z")

# Theory-guided models
theory_trait_context <- run_context_models(
  keyboard_data_trait_z,
  trait_outcomes,
  theory_features,
  model_family = "trait"
)

theory_daily_context <- run_context_models(
  keyboard_data_day_z,
  daily_outcomes,
  theory_features,
  model_family = "state"
)

theory_momentary_context <- run_context_models(
  keyboard_data_moment_z,
  momentary_outcomes,
  theory_features,
  model_family = "state"
)

theory_context_results_all <- bind_rows(
  theory_trait_context %>% mutate(timescale = "Trait"),
  theory_daily_context %>% mutate(timescale = "Daily"),
  theory_momentary_context %>% mutate(timescale = "Momentary")
) %>%
  left_join(theory_feature_lookup, by = "feature") %>%
  mutate(
    context_label = nice_context(context),
    outcome_label = nice_outcome_label(outcome),
    outcome_label = factor(
      outcome_label,
      levels = c("Trait NA", "Trait PA", "Daily valence", "Momentary valence")
    ),
    feature_label = factor(
      feature_label,
      levels = unname(theory_feature_labels[theory_features])
    )
  ) %>%
  group_by(outcome, context) %>%
  mutate(p_fdr = p.adjust(p.value, method = "BH")) %>%
  ungroup() %>%
  arrange(outcome_label, feature_label, context)

# Theory-guided interactions
theory_trait_interactions <- run_interaction_models(
  keyboard_data_trait_z,
  trait_outcomes,
  theory_features,
  model_family = "trait"
)

theory_daily_interactions <- run_interaction_models(
  keyboard_data_day_z,
  daily_outcomes,
  theory_features,
  model_family = "state"
)

theory_momentary_interactions <- run_interaction_models(
  keyboard_data_moment_z,
  momentary_outcomes,
  theory_features,
  model_family = "state"
)

theory_interaction_results_all <- bind_rows(
  theory_trait_interactions %>% mutate(timescale = "Trait"),
  theory_daily_interactions %>% mutate(timescale = "Daily"),
  theory_momentary_interactions %>% mutate(timescale = "Momentary")
) %>%
  left_join(theory_feature_lookup, by = "feature") %>%
  mutate(
    outcome_label = nice_outcome_label(outcome),
    outcome_label = factor(
      outcome_label,
      levels = c("Trait NA", "Trait PA", "Daily valence", "Momentary valence")
    )
  ) %>%
  group_by(outcome) %>%
  mutate(p_fdr = p.adjust(p.value, method = "BH")) %>%
  ungroup() %>%
  arrange(outcome_label, feature_label)

write.csv(
  theory_context_results_all,
  "results/theory_guided_liwc_context_specific_regression_results.csv",
  row.names = FALSE
)

write.csv(
  theory_interaction_results_all,
  "results/theory_guided_liwc_context_interaction_results.csv",
  row.names = FALSE
)

############################
#### TRAIT BOTH-CONTEXT SENSITIVITY ANALYSIS
############################

# Restrict each outcome-feature model to participants with usable
# private and public observations for that specific model.

theory_trait_context_both <- run_context_models(
  data = keyboard_data_trait_z,
  outcomes = trait_outcomes,
  features = theory_features,
  model_family = "trait",
  require_both_contexts = TRUE
) %>%
  left_join(theory_feature_lookup, by = "feature") %>%
  mutate(
    timescale = "Trait",
    sensitivity_sample = "Both contexts",
    context_label = nice_context(context),
    outcome_label = nice_outcome_label(outcome)
  ) %>%
  group_by(outcome, context) %>%
  mutate(
    p_fdr = p.adjust(p.value, method = "BH")
  ) %>%
  ungroup() %>%
  arrange(outcome_label, feature_label, context)

theory_trait_interactions_both <- run_interaction_models(
  data = keyboard_data_trait_z,
  outcomes = trait_outcomes,
  features = theory_features,
  model_family = "trait",
  require_both_contexts = TRUE
) %>%
  left_join(theory_feature_lookup, by = "feature") %>%
  mutate(
    timescale = "Trait",
    sensitivity_sample = "Both contexts",
    outcome_label = nice_outcome_label(outcome)
  ) %>%
  group_by(outcome) %>%
  mutate(
    p_fdr = p.adjust(p.value, method = "BH")
  ) %>%
  ungroup() %>%
  arrange(outcome_label, feature_label)

write.csv(
  theory_trait_context_both,
  "results/sensitivity_trait_both_contexts_context_specific_results.csv",
  row.names = FALSE
)

write.csv(
  theory_trait_interactions_both,
  "results/sensitivity_trait_both_contexts_interaction_results.csv",
  row.names = FALSE
)

print(theory_trait_context_both)
print(theory_trait_interactions_both)


############################
#### 8) FIGURE 4
############################

feature_order_main <- c("Pos. emotion", "Neg. emotion", "I", "We")

df_fig4 <- theory_context_results_all %>%
  filter(!is.na(estimate), !is.na(conf.low), !is.na(conf.high)) %>%
  mutate(
    context_label = factor(context_label, levels = c("Private", "Public")),
    outcome_label = factor(
      outcome_label,
      levels = c("Trait NA", "Trait PA", "Daily valence", "Momentary valence")
    ),
    feature_label_plot = recode(
      as.character(feature_label),
      "Pos. emotion" = "Pos. emotion",
      "Neg. emotion" = "Neg. emotion",
      "I" = "I",
      "We" = "We"
    ),
    feature_label_plot = factor(feature_label_plot, levels = feature_order_main),
    ci_zero = if_else(
      conf.low > 0 | conf.high < 0,
      "95% CI excludes 0",
      "95% CI includes 0"
    ),
    ci_zero = factor(
      ci_zero,
      levels = c("95% CI includes 0", "95% CI excludes 0")
    )
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

fig4 <- ggplot(
  df_fig4,
  aes(
    x = feature_label_plot,
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
      ymax = conf.high,
      linewidth = ci_zero,
      alpha = ci_zero
    ),
    position = pd_context
  ) +
  geom_point(
    aes(alpha = ci_zero),
    size = 2.8,
    position = pd_context
  ) +
  facet_wrap(
    ~ outcome_label,
    ncol = 1,
    labeller = as_labeller(c(
      "Trait NA" = "A) Trait negative affect",
      "Trait PA" = "B) Trait positive affect",
      "Daily valence" = "C) Daily affective valence",
      "Momentary valence" = "D) Momentary affective valence"
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
  scale_linewidth_manual(
    values = c(
      "95% CI includes 0" = 0.45,
      "95% CI excludes 0" = 1.05
    ),
    guide = "none"
  ) +
  scale_alpha_manual(
    values = c(
      "95% CI includes 0" = 0.45,
      "95% CI excludes 0" = 1.00
    ),
    guide = "none"
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
      override.aes = list(size = 3.2, alpha = 1, linewidth = 0.8)
    ),
    shape = guide_legend(
      order = 1,
      override.aes = list(size = 3.2, alpha = 1, linewidth = 0.8)
    )
  )

fig4

ggsave(
  filename = "figures/figure_4_theory_guided_liwc_regression_results.png",
  plot = fig4,
  width = 6,
  height = 10,
  dpi = 300
)

write.csv(
  df_fig4,
  "results/figure_4_theory_guided_liwc_plot_data.csv",
  row.names = FALSE
)

############################
#### 9) TABLE S7
############################

table_s7_long <- theory_context_results_all %>%
  mutate(
    Outcome = as.character(outcome_label),
    Context = nice_context(context),
    feature_label = as.character(feature_label),
    beta_ci = fmt_beta_ci(estimate, conf.low, conf.high, digits = 2)
  ) %>%
  select(Outcome, Context, feature_label, beta_ci)

table_s7 <- table_s7_long %>%
  mutate(
    feature_label = recode(
      feature_label,
      "Pos. emotion" = "Pos. emotion beta [95% CI]",
      "Neg. emotion" = "Neg. emotion beta [95% CI]",
      "I" = "I beta [95% CI]",
      "We" = "We beta [95% CI]"
    )
  ) %>%
  pivot_wider(
    names_from = feature_label,
    values_from = beta_ci
  ) %>%
  mutate(
    Outcome = factor(
      Outcome,
      levels = c("Trait PA", "Trait NA", "Daily valence", "Momentary valence")
    ),
    Context = factor(Context, levels = c("Private", "Public"))
  ) %>%
  arrange(Outcome, Context) %>%
  mutate(
    Outcome = as.character(Outcome),
    Context = as.character(Context)
  )

write.csv(
  table_s7,
  "results/table_s7_theory_guided_liwc_regression_results.csv",
  row.names = FALSE,
  na = ""
)

table_s7

############################
#### 10) FULL ALL-LIWC REPOSITORY CSV
############################

# Context-specific coefficients for all LIWC share categories across all timescales.
all_liwc_trait_context <- run_context_models(
  keyboard_data_trait_z,
  trait_outcomes,
  all_liwc_features,
  model_family = "trait"
)

all_liwc_daily_context <- run_context_models(
  keyboard_data_day_z,
  daily_outcomes,
  all_liwc_features,
  model_family = "state"
)

all_liwc_momentary_context <- run_context_models(
  keyboard_data_moment_z,
  momentary_outcomes,
  all_liwc_features,
  model_family = "state"
)

all_liwc_context_results_all <- bind_rows(
  all_liwc_trait_context %>% mutate(timescale = "Trait"),
  all_liwc_daily_context %>% mutate(timescale = "Daily"),
  all_liwc_momentary_context %>% mutate(timescale = "Momentary")
) %>%
  left_join(all_liwc_feature_lookup, by = "feature") %>%
  mutate(
    context_label = nice_context(context),
    outcome_label = nice_outcome_label(outcome)
  ) %>%
  group_by(timescale, outcome, context) %>%
  mutate(p_fdr = p.adjust(p.value, method = "BH")) %>%
  ungroup() %>%
  arrange(timescale, outcome_label, context_label, feature)

write.csv(
  all_liwc_context_results_all,
  "results/repository_all_liwc_share_context_specific_regression_results.csv",
  row.names = FALSE
)

# Context interaction coefficients for all LIWC share categories.
all_liwc_trait_interactions <- run_interaction_models(
  keyboard_data_trait_z,
  trait_outcomes,
  all_liwc_features,
  model_family = "trait"
)

all_liwc_daily_interactions <- run_interaction_models(
  keyboard_data_day_z,
  daily_outcomes,
  all_liwc_features,
  model_family = "state"
)

all_liwc_momentary_interactions <- run_interaction_models(
  keyboard_data_moment_z,
  momentary_outcomes,
  all_liwc_features,
  model_family = "state"
)

all_liwc_interaction_results_all <- bind_rows(
  all_liwc_trait_interactions %>% mutate(timescale = "Trait"),
  all_liwc_daily_interactions %>% mutate(timescale = "Daily"),
  all_liwc_momentary_interactions %>% mutate(timescale = "Momentary")
) %>%
  left_join(all_liwc_feature_lookup, by = "feature") %>%
  mutate(
    outcome_label = nice_outcome_label(outcome)
  ) %>%
  group_by(timescale, outcome) %>%
  mutate(p_fdr = p.adjust(p.value, method = "BH")) %>%
  ungroup() %>%
  arrange(timescale, outcome_label, feature)

write.csv(
  all_liwc_interaction_results_all,
  "results/repository_all_liwc_share_context_interaction_regression_results.csv",
  row.names = FALSE
)

############################
#### TABLE S8:
#### LIWC CATEGORIES MOST STRONGLY ASSOCIATED WITH TRAIT AFFECT
############################

# Table S8 ranks LIWC word categories by their overall absolute association
# with trait affect across:
# Private PA, Private NA, Public PA, Public NA.
# Only trait-level, context-specific coefficients are used.

table_s8_long <- all_liwc_context_results_all %>%
  filter(
    timescale == "Trait",
    outcome %in% c("pa_trait_z", "na_trait_z"),
    context %in% c("private", "public")
  ) %>%
  mutate(
    context_label = nice_context(context),
    outcome_short = case_when(
      outcome == "pa_trait_z" ~ "PA",
      outcome == "na_trait_z" ~ "NA",
      TRUE ~ outcome
    ),
    column_name = paste(context_label, outcome_short, "beta [95% CI]"),
    beta_ci = fmt_beta_ci(estimate, conf.low, conf.high, digits = 2)
  )

# Rank features by summed absolute coefficient across the four trait-affect/context cells.
table_s8_ranked_features <- table_s8_long %>%
  group_by(feature, feature_label) %>%
  summarise(
    overall_abs_trait_affect_association = sum(abs(estimate), na.rm = TRUE),
    n_available_coefficients = sum(!is.na(estimate)),
    .groups = "drop"
  ) %>%
  filter(n_available_coefficients == 4) %>%
  arrange(desc(overall_abs_trait_affect_association)) %>%
  slice_head(n = 10)

table_s8 <- table_s8_long %>%
  semi_join(table_s8_ranked_features, by = c("feature", "feature_label")) %>%
  select(feature, feature_label, column_name, beta_ci) %>%
  pivot_wider(
    names_from = column_name,
    values_from = beta_ci
  ) %>%
  left_join(
    table_s8_ranked_features %>%
      select(feature, overall_abs_trait_affect_association),
    by = "feature"
  ) %>%
  arrange(desc(overall_abs_trait_affect_association)) %>%
  select(
    `Word category` = feature_label,
    `Private PA beta [95% CI]`,
    `Private NA beta [95% CI]`,
    `Public PA beta [95% CI]`,
    `Public NA beta [95% CI]`
  )

write.csv(
  table_s8,
  "results/table_s8_top_liwc_trait_affect_associations.csv",
  row.names = FALSE,
  na = ""
)

# Optional: save ranking values for transparency/repository
table_s8_ranked_features_export <- table_s8_ranked_features %>%
  arrange(desc(overall_abs_trait_affect_association))

write.csv(
  table_s8_ranked_features_export,
  "results/table_s8_top_liwc_trait_affect_associations_ranking.csv",
  row.names = FALSE
)

table_s8

# finish