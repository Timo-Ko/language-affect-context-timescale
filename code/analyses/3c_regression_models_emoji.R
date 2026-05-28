############################
#### EMOJI × AFFECT ANALYSES
#### TABLE S9 + FULL REPOSITORY CSV
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

to_symbol_from_emoji_feature <- function(feature_name) {
  # Expected format: emoji_128512_share
  code <- stringr::str_match(feature_name, "^emoji_([0-9]+)_share$")[, 2]
  
  if (is.na(code)) {
    return(feature_name)
  }
  
  tryCatch(
    intToUtf8(as.integer(code)),
    error = function(e) feature_name
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
#### 3) IDENTIFY INDIVIDUAL EMOJI FEATURES
############################

# Individual emoji share features only.
# Expected format: emoji_128512_share

get_numeric_emoji_features <- function(df) {
  candidate_features <- names(df) %>%
    str_subset("^emoji_[0-9]+_share$")
  
  candidate_features[
    vapply(df[candidate_features], is.numeric, logical(1))
  ]
}

emoji_features_trait <- get_numeric_emoji_features(keyboard_data_trait)
emoji_features_day <- get_numeric_emoji_features(keyboard_data_day)
emoji_features_moment <- get_numeric_emoji_features(keyboard_data_moment)

all_emoji_features <- Reduce(
  intersect,
  list(
    emoji_features_trait,
    emoji_features_day,
    emoji_features_moment
  )
)

if (length(all_emoji_features) == 0) {
  stop("No shared individual emoji share features found across trait, daily, and momentary datasets.")
}

message("Number of shared individual emoji share features: ", length(all_emoji_features))

emoji_lookup <- tibble(
  feature = all_emoji_features,
  emoji = vapply(all_emoji_features, to_symbol_from_emoji_feature, character(1))
)

write.csv(
  emoji_lookup,
  "results/emoji_feature_lookup.csv",
  row.names = FALSE
)

############################
#### 4) EMOJI PREVALENCE FILTER
############################

# Keep emojis used by at least 10% of trait participants in at least one context.
# For emoji share variables, values > 0 indicate use of the emoji.

emoji_prevalence_trait_context <- keyboard_data_trait %>%
  select(user_id, context, all_of(all_emoji_features)) %>%
  pivot_longer(
    cols = all_of(all_emoji_features),
    names_to = "feature",
    values_to = "value"
  ) %>%
  group_by(feature, context) %>%
  summarise(
    n_users = n_distinct(user_id),
    n_users_nonzero = n_distinct(user_id[!is.na(value) & value > 0]),
    prop_users_nonzero = n_users_nonzero / n_users,
    .groups = "drop"
  ) %>%
  left_join(emoji_lookup, by = "feature")

emoji_keep <- emoji_prevalence_trait_context %>%
  group_by(feature, emoji) %>%
  summarise(
    max_prop_users_nonzero = max(prop_users_nonzero, na.rm = TRUE),
    max_n_users_nonzero = max(n_users_nonzero, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(max_prop_users_nonzero >= 0.10) %>%
  arrange(desc(max_prop_users_nonzero))

emoji_features_keep <- emoji_keep$feature

message("Number of emojis retained for analysis: ", length(emoji_features_keep))

if (length(emoji_features_keep) == 0) {
  stop("No emojis met the 10% prevalence threshold.")
}

write.csv(
  emoji_prevalence_trait_context,
  "results/repository_emoji_prevalence_trait_by_context.csv",
  row.names = FALSE
)

write.csv(
  emoji_keep,
  "results/repository_emoji_features_retained_for_analysis.csv",
  row.names = FALSE
)

############################
#### 5) PREPARE ANALYSIS DATA
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

keyboard_data_trait_emoji_z <- keyboard_data_trait %>%
  select(user_id, scope, context, all_of(emoji_features_keep)) %>%
  left_join(trait_outcomes, by = "user_id") %>%
  mutate(
    across(
      all_of(emoji_features_keep),
      z_scale,
      .names = "{.col}_z"
    )
  )

keyboard_data_day_emoji_z <- keyboard_data_day %>%
  mutate(
    daily_valence_z = z_scale(daily_valence)
  ) %>%
  mutate(
    across(
      all_of(emoji_features_keep),
      z_scale,
      .names = "{.col}_z"
    )
  )

keyboard_data_moment_emoji_z <- keyboard_data_moment %>%
  mutate(
    momentary_valence_z = z_scale(valence)
  ) %>%
  mutate(
    across(
      all_of(emoji_features_keep),
      z_scale,
      .names = "{.col}_z"
    )
  )

############################
#### 6) MODEL HELPERS: TRAIT MODELS
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

fit_trait_emoji_model <- function(data, outcome, feature) {
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
#### 7) MODEL HELPERS: DAILY / MOMENTARY MIXED MODELS
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

fit_state_emoji_model <- function(data, outcome, feature) {
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
#### 8) RUN MODELS
############################

run_context_models <- function(data, outcomes, features, model_family) {
  bind_rows(lapply(outcomes, function(outcome_i) {
    bind_rows(lapply(features, function(feature_i) {
      
      if (model_family == "trait") {
        fit_i <- fit_trait_emoji_model(
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
        fit_i <- fit_state_emoji_model(
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
        fit_i <- fit_trait_emoji_model(
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
        fit_i <- fit_state_emoji_model(
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

# Context-specific emoji coefficients across all timescales
emoji_trait_context <- run_context_models(
  keyboard_data_trait_emoji_z,
  trait_outcomes,
  emoji_features_keep,
  model_family = "trait"
)

emoji_daily_context <- run_context_models(
  keyboard_data_day_emoji_z,
  daily_outcomes,
  emoji_features_keep,
  model_family = "state"
)

emoji_momentary_context <- run_context_models(
  keyboard_data_moment_emoji_z,
  momentary_outcomes,
  emoji_features_keep,
  model_family = "state"
)

emoji_context_results_all <- bind_rows(
  emoji_trait_context %>% mutate(timescale = "Trait"),
  emoji_daily_context %>% mutate(timescale = "Daily"),
  emoji_momentary_context %>% mutate(timescale = "Momentary")
) %>%
  left_join(emoji_lookup, by = "feature") %>%
  left_join(
    emoji_prevalence_trait_context %>%
      select(
        feature,
        context,
        n_users_context = n_users,
        n_users_nonzero_context = n_users_nonzero,
        prop_users_nonzero_context = prop_users_nonzero
      ),
    by = c("feature", "context")
  ) %>%
  mutate(
    context_label = nice_context(context),
    outcome_label = nice_outcome_label(outcome)
  ) %>%
  group_by(timescale, outcome, context) %>%
  mutate(p_fdr = p.adjust(p.value, method = "BH")) %>%
  ungroup() %>%
  arrange(timescale, outcome_label, context_label, feature)

write.csv(
  emoji_context_results_all,
  "results/repository_emoji_context_specific_regression_results_all_timescales.csv",
  row.names = FALSE
)

# Context interaction coefficients across all timescales
emoji_trait_interactions <- run_interaction_models(
  keyboard_data_trait_emoji_z,
  trait_outcomes,
  emoji_features_keep,
  model_family = "trait"
)

emoji_daily_interactions <- run_interaction_models(
  keyboard_data_day_emoji_z,
  daily_outcomes,
  emoji_features_keep,
  model_family = "state"
)

emoji_momentary_interactions <- run_interaction_models(
  keyboard_data_moment_emoji_z,
  momentary_outcomes,
  emoji_features_keep,
  model_family = "state"
)

emoji_interaction_results_all <- bind_rows(
  emoji_trait_interactions %>% mutate(timescale = "Trait"),
  emoji_daily_interactions %>% mutate(timescale = "Daily"),
  emoji_momentary_interactions %>% mutate(timescale = "Momentary")
) %>%
  left_join(emoji_lookup, by = "feature") %>%
  mutate(
    outcome_label = nice_outcome_label(outcome)
  ) %>%
  group_by(timescale, outcome) %>%
  mutate(p_fdr = p.adjust(p.value, method = "BH")) %>%
  ungroup() %>%
  arrange(timescale, outcome_label, feature)

write.csv(
  emoji_interaction_results_all,
  "results/repository_emoji_context_interaction_regression_results_all_timescales.csv",
  row.names = FALSE
)

############################
#### 9) TABLE S9
############################

# Table S9 ranks emojis by their overall absolute association with trait affect
# across Private PA, Private NA, Public PA, and Public NA.

table_s9_long <- emoji_context_results_all %>%
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

table_s9_ranked_features <- table_s9_long %>%
  group_by(feature, emoji) %>%
  summarise(
    overall_abs_trait_affect_association = sum(abs(estimate), na.rm = TRUE),
    n_available_coefficients = sum(!is.na(estimate)),
    .groups = "drop"
  ) %>%
  filter(n_available_coefficients == 4) %>%
  arrange(desc(overall_abs_trait_affect_association)) %>%
  slice_head(n = 10)

table_s9 <- table_s9_long %>%
  semi_join(table_s9_ranked_features, by = c("feature", "emoji")) %>%
  select(feature, emoji, column_name, beta_ci) %>%
  pivot_wider(
    names_from = column_name,
    values_from = beta_ci
  ) %>%
  left_join(
    table_s9_ranked_features %>%
      select(feature, overall_abs_trait_affect_association),
    by = "feature"
  ) %>%
  arrange(desc(overall_abs_trait_affect_association)) %>%
  select(
    Emoji = emoji,
    `Private PA beta [95% CI]`,
    `Private NA beta [95% CI]`,
    `Public PA beta [95% CI]`,
    `Public NA beta [95% CI]`
  )

write.csv(
  table_s9,
  "results/table_s9_top_emoji_trait_affect_associations.csv",
  row.names = FALSE,
  na = ""
)

write.csv(
  table_s9_ranked_features,
  "results/table_s9_top_emoji_trait_affect_associations_ranking.csv",
  row.names = FALSE
)

table_s9

# finish