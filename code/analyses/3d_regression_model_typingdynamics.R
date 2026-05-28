############################
#### TYPING DYNAMICS × AFFECT ANALYSES
#### FULL REPOSITORY CSV
############################

############################
#### 0) PREPARATION
############################

required_packages <- c(
  "dplyr",
  "tidyr",
  "purrr",
  "tibble",
  "stringr",
  "broom",
  "broom.mixed",
  "lmtest",
  "sandwich",
  "lme4"
)

invisible(lapply(required_packages, library, character.only = TRUE))

dir.create("results", recursive = TRUE, showWarnings = FALSE)

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
#### 3) IDENTIFY TYPING-DYNAMICS FEATURES
############################

# Candidate typing-dynamics variables.
# Adjust this pattern if your feature names differ.
# The exclusion step removes outcomes, IDs, context variables, LIWC, emoji, and basic volume variables.

excluded_patterns <- paste(
  c(
    "^user_id$",
    "^scope$",
    "^context$",
    "^date$",
    "^es_questionnaire_id$",
    "^pa_panas$",
    "^na_panas$",
    "^daily_valence$",
    "^valence$",
    "^arousal$",
    "^liwc_",
    "^emoji_",
    "^emoticon",
    "^words_typed$",
    "^emoji_count$",
    "^emoticon_count$",
    "^n_ema",
    "^age$",
    "^gender$"
  ),
  collapse = "|"
)

typing_name_patterns <- paste(
  c(
    "typing",
    "session",
    "speed",
    "latency",
    "duration",
    "pause",
    "backspace",
    "delete",
    "correction",
    "autocorrect",
    "keystroke",
    "key",
    "char",
    "characters",
    "n_sessions"
  ),
  collapse = "|"
)

get_typing_features <- function(df) {
  candidate_features <- names(df) %>%
    .[!str_detect(., excluded_patterns)] %>%
    .[str_detect(., regex(typing_name_patterns, ignore_case = TRUE))]
  
  candidate_features[
    vapply(df[candidate_features], is.numeric, logical(1))
  ]
}

typing_features_trait <- get_typing_features(keyboard_data_trait)
typing_features_day <- get_typing_features(keyboard_data_day)
typing_features_moment <- get_typing_features(keyboard_data_moment)

typing_features <- Reduce(
  intersect,
  list(
    typing_features_trait,
    typing_features_day,
    typing_features_moment
  )
)

if (length(typing_features) == 0) {
  stop(
    "No shared typing-dynamics features found across trait, daily, and momentary datasets. ",
    "Inspect feature names with names(keyboard_data_trait)."
  )
}

message("Number of shared typing-dynamics features: ", length(typing_features))
print(typing_features)

typing_feature_lookup <- tibble(
  feature = typing_features,
  feature_label = typing_features
)

write.csv(
  typing_feature_lookup,
  "results/typing_dynamics_feature_lookup.csv",
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

keyboard_data_trait_typing_z <- keyboard_data_trait %>%
  select(user_id, scope, context, all_of(typing_features)) %>%
  left_join(trait_outcomes, by = "user_id") %>%
  mutate(
    across(
      all_of(typing_features),
      z_scale,
      .names = "{.col}_z"
    )
  )

keyboard_data_day_typing_z <- keyboard_data_day %>%
  mutate(
    daily_valence_z = z_scale(daily_valence)
  ) %>%
  mutate(
    across(
      all_of(typing_features),
      z_scale,
      .names = "{.col}_z"
    )
  )

keyboard_data_moment_typing_z <- keyboard_data_moment %>%
  mutate(
    momentary_valence_z = z_scale(valence)
  ) %>%
  mutate(
    across(
      all_of(typing_features),
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

fit_trait_typing_model <- function(data, outcome, feature) {
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

fit_state_typing_model <- function(data, outcome, feature) {
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
  
  mod <- tryCatch(
    suppressWarnings(
      lme4::lmer(
        formula = fml,
        data = dat,
        REML = FALSE,
        control = lme4::lmerControl(
          optimizer = "bobyqa",
          optCtrl = list(maxfun = 2e5)
        )
      )
    ),
    error = function(e) NULL
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

run_context_models <- function(data, outcomes, features, model_family) {
  bind_rows(lapply(outcomes, function(outcome_i) {
    bind_rows(lapply(features, function(feature_i) {
      
      if (model_family == "trait") {
        fit_i <- fit_trait_typing_model(
          data = data,
          outcome = outcome_i,
          feature = feature_i
        )
        
        extract_trait_context_results(
          fit = fit_i,
          outcome = outcome_i,
          feature = feature_i
        )
      } else {
        fit_i <- fit_state_typing_model(
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

run_interaction_models <- function(data, outcomes, features, model_family) {
  bind_rows(lapply(outcomes, function(outcome_i) {
    bind_rows(lapply(features, function(feature_i) {
      
      if (model_family == "trait") {
        fit_i <- fit_trait_typing_model(
          data = data,
          outcome = outcome_i,
          feature = feature_i
        )
        
        extract_trait_interaction_results(
          fit = fit_i,
          outcome = outcome_i,
          feature = feature_i
        )
      } else {
        fit_i <- fit_state_typing_model(
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

# Context-specific coefficients across all timescales
typing_trait_context <- run_context_models(
  keyboard_data_trait_typing_z,
  trait_outcomes,
  typing_features,
  model_family = "trait"
)

typing_daily_context <- run_context_models(
  keyboard_data_day_typing_z,
  daily_outcomes,
  typing_features,
  model_family = "state"
)

typing_momentary_context <- run_context_models(
  keyboard_data_moment_typing_z,
  momentary_outcomes,
  typing_features,
  model_family = "state"
)

typing_context_results_all <- bind_rows(
  typing_trait_context %>% mutate(timescale = "Trait"),
  typing_daily_context %>% mutate(timescale = "Daily"),
  typing_momentary_context %>% mutate(timescale = "Momentary")
) %>%
  left_join(typing_feature_lookup, by = "feature") %>%
  mutate(
    context_label = nice_context(context),
    outcome_label = nice_outcome_label(outcome)
  ) %>%
  group_by(timescale, outcome, context) %>%
  mutate(p_fdr = p.adjust(p.value, method = "BH")) %>%
  ungroup() %>%
  arrange(timescale, outcome_label, context_label, feature)

write.csv(
  typing_context_results_all,
  "results/repository_typing_dynamics_context_specific_regression_results_all_timescales.csv",
  row.names = FALSE
)

# Context interaction coefficients across all timescales
typing_trait_interactions <- run_interaction_models(
  keyboard_data_trait_typing_z,
  trait_outcomes,
  typing_features,
  model_family = "trait"
)

typing_daily_interactions <- run_interaction_models(
  keyboard_data_day_typing_z,
  daily_outcomes,
  typing_features,
  model_family = "state"
)

typing_momentary_interactions <- run_interaction_models(
  keyboard_data_moment_typing_z,
  momentary_outcomes,
  typing_features,
  model_family = "state"
)

typing_interaction_results_all <- bind_rows(
  typing_trait_interactions %>% mutate(timescale = "Trait"),
  typing_daily_interactions %>% mutate(timescale = "Daily"),
  typing_momentary_interactions %>% mutate(timescale = "Momentary")
) %>%
  left_join(typing_feature_lookup, by = "feature") %>%
  mutate(
    outcome_label = nice_outcome_label(outcome)
  ) %>%
  group_by(timescale, outcome) %>%
  mutate(p_fdr = p.adjust(p.value, method = "BH")) %>%
  ungroup() %>%
  arrange(timescale, outcome_label, feature)

write.csv(
  typing_interaction_results_all,
  "results/repository_typing_dynamics_context_interaction_regression_results_all_timescales.csv",
  row.names = FALSE
)

# finish