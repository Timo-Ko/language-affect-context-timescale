############################
#### LIWC / DICTIONARY × AFFECT ANALYSES
#### FIGURE 4 + TABLE S6 + FULL REPOSITORY CSV
############################

############################
#### 0) PREPARATION
############################

required_packages <- c(
  "dplyr", "tidyr", "purrr", "tibble", "ggplot2", "lme4",
  "broom", "broom.mixed", "lmtest", "sandwich", "clubSandwich",
  "scales", "stringr", "grid"
)
invisible(lapply(required_packages, library, character.only = TRUE))

dir.create("results", recursive = TRUE, showWarnings = FALSE)
dir.create("figures", recursive = TRUE, showWarnings = FALSE)
source("code/analyses/helper/plot_theme.R")
base_theme <- theme_custom(base_size = 12)
set.seed(123)

############################
#### 1) HELPERS
############################

z_scale <- function(x) {
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(NA_real_, length(x)))
  as.numeric(scale(x))
}

safe_sd <- function(x) sd(x, na.rm = TRUE)

p_from_est_se <- function(estimate, se) {
  ifelse(
    is.na(estimate) | is.na(se) | se <= 0,
    NA_real_,
    2 * pnorm(abs(estimate / se), lower.tail = FALSE)
  )
}

nice_context <- function(x) {
  case_when(
    x == "private" ~ "Private",
    x == "public" ~ "Public",
    TRUE ~ as.character(x)
  )
}

nice_outcome_label <- function(x) {
  case_when(
    x == "pa_trait_z" ~ "Trait PA",
    x == "na_trait_z" ~ "Trait NA",
    x == "daily_valence_z" ~ "Daily valence",
    x == "momentary_valence_z" ~ "Momentary valence",
    TRUE ~ x
  )
}

fmt_beta_ci <- function(beta, low, high, digits = 2) {
  ifelse(
    is.na(beta) | is.na(low) | is.na(high),
    NA_character_,
    paste0(
      formatC(beta, format = "f", digits = digits), " [",
      formatC(low, format = "f", digits = digits), ", ",
      formatC(high, format = "f", digits = digits), "]"
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
    date = as.Date(date),
    occasion_id = interaction(user_id, date, drop = TRUE)
  )

keyboard_data_moment <- readRDS("data/results/keyboard_data_ema_final.rds") %>%
  as.data.frame() %>%
  filter(scope %in% c("private", "public")) %>%
  mutate(
    user_id = as.character(user_id),
    context = factor(scope, levels = c("private", "public")),
    occasion_id = interaction(user_id, es_questionnaire_id, drop = TRUE)
  )

stopifnot(
  all(c("user_id", "scope", "pa_panas", "na_panas") %in% names(keyboard_data_trait)),
  all(c("user_id", "scope", "daily_valence", "date") %in% names(keyboard_data_day)),
  all(c("user_id", "scope", "valence", "es_questionnaire_id") %in% names(keyboard_data_moment))
)

# Diagnostic: how many context rows occur per day / EMA occasion?
write.csv(
  keyboard_data_day %>% count(user_id, date, name = "n_context_rows") %>% count(n_context_rows, name = "n_occasions"),
  "results/check_daily_rows_per_occasion.csv",
  row.names = FALSE
)
write.csv(
  keyboard_data_moment %>% count(user_id, es_questionnaire_id, name = "n_context_rows") %>% count(n_context_rows, name = "n_occasions"),
  "results/check_momentary_rows_per_occasion.csv",
  row.names = FALSE
)

############################
#### 3) FEATURE DEFINITIONS
############################

theory_feature_labels <- c(
  "liwc_posemo" = "Pos. emotion",
  "liwc_negemo" = "Neg. emotion",
  "liwc_i" = "I",
  "liwc_we" = "We"
)
theory_features <- names(theory_feature_labels)

get_liwc_shares <- function(data) {
  out <- names(data) %>%
    str_subset("^liwc_") %>%
    .[!str_detect(., "_(mean|sd|min|max)$")]
  out[sapply(data[out], is.numeric)]
}

liwc_share_features_trait <- get_liwc_shares(keyboard_data_trait)
liwc_share_features_day <- get_liwc_shares(keyboard_data_day)
liwc_share_features_moment <- get_liwc_shares(keyboard_data_moment)
all_liwc_features <- Reduce(
  intersect,
  list(liwc_share_features_trait, liwc_share_features_day, liwc_share_features_moment)
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

############################
#### 4) PREPARE ANALYSIS DATA
############################

trait_outcome_data <- keyboard_data_trait %>%
  distinct(user_id, pa_panas, na_panas) %>%
  filter(!is.na(pa_panas), !is.na(na_panas)) %>%
  mutate(
    pa_trait_z = z_scale(pa_panas),
    na_trait_z = z_scale(na_panas)
  ) %>%
  select(user_id, pa_trait_z, na_trait_z)

keyboard_data_trait_z <- keyboard_data_trait %>%
  select(user_id, scope, context, all_of(all_liwc_features)) %>%
  left_join(trait_outcome_data, by = "user_id") %>%
  mutate(across(all_of(all_liwc_features), z_scale, .names = "{.col}_z"))

keyboard_data_day_z <- keyboard_data_day %>%
  mutate(
    daily_valence_z = z_scale(daily_valence),
    occasion_id = factor(occasion_id)
  )

keyboard_data_moment_z <- keyboard_data_moment %>%
  mutate(
    momentary_valence_z = z_scale(valence),
    occasion_id = factor(occasion_id)
  )

############################
#### 5) TRAIT MODEL HELPERS
############################

coef_by_context_lm <- function(model, term, vcov_mat, conf = 0.95) {
  b <- coef(model)
  int1 <- paste0(term, ":contextpublic")
  int2 <- paste0("contextpublic:", term)
  int_term <- if (int1 %in% names(b)) int1 else if (int2 %in% names(b)) int2 else NA_character_
  
  if (!term %in% names(b)) {
    return(tibble(
      context = c("private", "public"), estimate = NA_real_, se = NA_real_,
      conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_
    ))
  }
  
  z <- qnorm(1 - (1 - conf) / 2)
  est_private <- unname(b[term])
  se_private <- sqrt(unname(vcov_mat[term, term]))
  
  if (!is.na(int_term) && int_term %in% rownames(vcov_mat)) {
    est_public <- unname(b[term] + b[int_term])
    var_public <- unname(
      vcov_mat[term, term] + vcov_mat[int_term, int_term] + 2 * vcov_mat[term, int_term]
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
    conf.low = c(est_private - z * se_private, est_public - z * se_public),
    conf.high = c(est_private + z * se_private, est_public + z * se_public),
    p.value = p_from_est_se(c(est_private, est_public), c(se_private, se_public))
  )
}

fit_trait_dictionary_model <- function(data, outcome, feature, require_both_contexts = FALSE) {
  feat_z <- paste0(feature, "_z")
  dat <- data %>%
    filter(
      !is.na(.data[[outcome]]), !is.na(.data[[feat_z]]),
      !is.na(context), !is.na(user_id)
    ) %>%
    mutate(context = factor(context, levels = c("private", "public")))
  
  if (require_both_contexts) {
    dat <- dat %>%
      group_by(user_id) %>%
      filter(n_distinct(context) == 2, n() == 2) %>%
      ungroup()
  }
  
  if (
    nrow(dat) < 30 || n_distinct(dat$user_id) < 20 || n_distinct(dat$context) < 2 ||
    is.na(safe_sd(dat[[outcome]])) || is.na(safe_sd(dat[[feat_z]])) ||
    safe_sd(dat[[outcome]]) == 0 || safe_sd(dat[[feat_z]]) == 0
  ) return(NULL)
  
  mod <- lm(as.formula(paste0(outcome, " ~ ", feat_z, " * context")), data = dat)
  V <- tryCatch(
    sandwich::vcovCL(mod, cluster = dat$user_id, type = "HC1"),
    error = function(e) sandwich::vcovHC(mod, type = "HC3")
  )
  list(model = mod, vcov = V, data = dat, feature_z = feat_z)
}

extract_trait_context_results <- function(fit, outcome, feature) {
  if (is.null(fit)) {
    return(tibble(
      outcome = outcome, outcome_label = nice_outcome_label(outcome), feature = feature,
      context = c("private", "public"), estimate = NA_real_, se = NA_real_,
      conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_,
      n_rows = NA_integer_, n_users = NA_integer_, model_type = "lm_cluster_robust"
    ))
  }
  coef_by_context_lm(fit$model, fit$feature_z, fit$vcov) %>%
    mutate(
      outcome = outcome, outcome_label = nice_outcome_label(outcome), feature = feature,
      n_rows = nrow(fit$data), n_users = n_distinct(fit$data$user_id),
      model_type = "lm_cluster_robust", .before = 1
    )
}

extract_trait_interaction_results <- function(fit, outcome, feature) {
  if (is.null(fit)) {
    return(tibble(
      outcome = outcome, outcome_label = nice_outcome_label(outcome), feature = feature,
      estimate = NA_real_, se = NA_real_, conf.low = NA_real_, conf.high = NA_real_,
      p.value = NA_real_, n_rows = NA_integer_, n_users = NA_integer_,
      model_type = "lm_cluster_robust"
    ))
  }
  
  td <- broom::tidy(lmtest::coeftest(fit$model, vcov. = fit$vcov))
  terms <- c(
    paste0(fit$feature_z, ":contextpublic"),
    paste0("contextpublic:", fit$feature_z)
  )
  out <- td %>% filter(term %in% terms) %>% slice(1)
  if (nrow(out) == 0) return(NULL)
  
  out %>% transmute(
    outcome = outcome, outcome_label = nice_outcome_label(outcome), feature = feature,
    estimate = estimate, se = std.error,
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    p.value = p.value, n_rows = nrow(fit$data),
    n_users = n_distinct(fit$data$user_id), model_type = "lm_cluster_robust"
  )
}

############################
#### 6) DAILY / MOMENTARY DECOMPOSED MODEL HELPERS
############################

fit_mixed_model <- function(formula, data) {
  tryCatch(
    suppressWarnings(
      lmer(
        formula = formula,
        data = data,
        REML = FALSE,
        control = lmerControl(
          optimizer = "bobyqa",
          optCtrl = list(maxfun = 2e5)
        )
      )
    ),
    error = function(e) {
      message("Mixed model failed: ", conditionMessage(e))
      NULL
    }
  )
}


# Standardize the between-person component using one row per
# participant × context, so participants with more observations
# do not receive greater weight when defining its SD.
standardize_between_component <- function(dat, mean_var) {
  
  between_lookup <- dat %>%
    distinct(user_id, context, .data[[mean_var]]) %>%
    mutate(
      between_z = z_scale(.data[[mean_var]])
    ) %>%
    select(user_id, context, between_z)
  
  dat %>%
    left_join(
      between_lookup,
      by = c("user_id", "context")
    )
}


prepare_decomposed_feature <- function(data, outcome, feature) {
  
  dat <- data %>%
    filter(
      !is.na(.data[[outcome]]),
      !is.na(.data[[feature]]),
      !is.na(context),
      !is.na(user_id),
      !is.na(occasion_id)
    ) %>%
    mutate(
      user_id = factor(user_id),
      context = factor(context, levels = c("private", "public")),
      occasion_id = factor(occasion_id)
    )
  
  if (nrow(dat) == 0) return(NULL)
  
  # Decomposition is performed within participant × context.
  # Thus, "usual language use" is context-specific.
  dat <- dat %>%
    group_by(user_id, context) %>%
    mutate(
      feature_person_mean = mean(.data[[feature]], na.rm = TRUE),
      feature_within_raw = .data[[feature]] - feature_person_mean,
      n_user_context_observations = n()
    ) %>%
    ungroup()
  
  dat <- standardize_between_component(
    dat = dat,
    mean_var = "feature_person_mean"
  ) %>%
    mutate(
      within_z = z_scale(feature_within_raw)
    )
  
  dat
}


get_cluster_robust_vcov <- function(model, data) {
  clubSandwich::vcovCR(
    model,
    cluster = data$user_id,
    type = "CR2"
  )
}

fit_state_dictionary_model <- function(data, outcome, feature) {
  
  dat <- prepare_decomposed_feature(
    data = data,
    outcome = outcome,
    feature = feature
  )
  
  if (is.null(dat)) return(NULL)
  
  # There must be variation in both components and both contexts.
  if (
    nrow(dat) < 30 ||
    n_distinct(dat$user_id) < 10 ||
    n_distinct(dat$context) < 2 ||
    is.na(safe_sd(dat[[outcome]])) ||
    safe_sd(dat[[outcome]]) == 0 ||
    is.na(safe_sd(dat$between_z)) ||
    safe_sd(dat$between_z) == 0 ||
    is.na(safe_sd(dat$within_z)) ||
    safe_sd(dat$within_z) == 0
  ) {
    return(NULL)
  }
  
  fml <- as.formula(
    paste0(
      outcome,
      " ~ between_z * context + within_z * context",
      " + (1 | user_id)"
    )
  )
  
  mod <- fit_mixed_model(fml, dat)
  
  if (is.null(mod)) return(NULL)
  
  V_CR2 <- tryCatch(
    get_cluster_robust_vcov(mod, dat),
    error = function(e) {
      message("CR2 covariance failed: ", conditionMessage(e))
      NULL
    }
  )
  
  if (is.null(V_CR2)) return(NULL)
  
  list(
    model = mod,
    vcov = V_CR2,
    data = dat,
    between_term = "between_z",
    within_term = "within_z",
    feature = feature
  )
}


coef_by_context_lmer <- function(model, term, vcov_mat, conf = 0.95) {
  
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
  
  b <- fixef(model)
  V <- as.matrix(vcov_mat)
  
  int1 <- paste0(term, ":contextpublic")
  int2 <- paste0("contextpublic:", term)
  
  int_term <- if (int1 %in% names(b)) {
    int1
  } else if (int2 %in% names(b)) {
    int2
  } else {
    NA_character_
  }
  
  if (!term %in% names(b)) {
    return(tibble(
      context = c("private", "public"),
      estimate = NA_real_,
      se = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      p.value = NA_real_
    ))
  }
  
  z_crit <- qnorm(1 - (1 - conf) / 2)
  
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
      est_private - z_crit * se_private,
      est_public - z_crit * se_public
    ),
    conf.high = c(
      est_private + z_crit * se_private,
      est_public + z_crit * se_public
    ),
    p.value = p_from_est_se(
      c(est_private, est_public),
      c(se_private, se_public)
    )
  )
}


extract_state_context_results <- function(
  fit,
  outcome,
  feature,
  component = c("within", "between")
) {
  
  component <- match.arg(component)
  
  if (is.null(fit)) {
    return(tibble(
      outcome = outcome,
      outcome_label = nice_outcome_label(outcome),
      feature = feature,
      component = component,
      context = c("private", "public"),
      estimate = NA_real_,
      se = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      p.value = NA_real_,
      n_rows = NA_integer_,
      n_users = NA_integer_,
      n_occasions = NA_integer_,
      n_user_contexts = NA_integer_,
      singular = NA,
      model_type = "lmer_within_between_CR2"
    ))
  }
  
  term <- if (component == "within") {
    fit$within_term
  } else {
    fit$between_term
  }
  
  coef_by_context_lmer(
    model = fit$model,
    term = term,
    vcov_mat = fit$vcov
  ) %>%
    mutate(
      outcome = outcome,
      outcome_label = nice_outcome_label(outcome),
      feature = feature,
      component = component,
      n_rows = nrow(fit$data),
      n_users = n_distinct(fit$data$user_id),
      n_occasions = n_distinct(fit$data$occasion_id),
      n_user_contexts = n_distinct(
        interaction(
          fit$data$user_id,
          fit$data$context,
          drop = TRUE
        )
      ),
      singular = isSingular(fit$model, tol = 1e-4),
      model_type = "lmer_within_between_CR2",
      .before = 1
    )
}


extract_state_interaction_results <- function(
  fit,
  outcome,
  feature,
  component = c("within", "between")
) {
  
  component <- match.arg(component)
  
  empty_result <- function(
    n_rows = NA_integer_,
    n_users = NA_integer_,
    n_occasions = NA_integer_,
    n_user_contexts = NA_integer_,
    singular = NA
  ) {
    tibble(
      outcome = outcome,
      outcome_label = nice_outcome_label(outcome),
      feature = feature,
      component = component,
      estimate = NA_real_,
      se = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      p.value = NA_real_,
      n_rows = n_rows,
      n_users = n_users,
      n_occasions = n_occasions,
      n_user_contexts = n_user_contexts,
      singular = singular,
      model_type = "lmer_within_between_CR2"
    )
  }
  
  if (is.null(fit)) {
    return(empty_result())
  }
  
  term <- if (component == "within") {
    fit$within_term
  } else {
    fit$between_term
  }
  
  b <- lme4::fixef(fit$model)
  V <- as.matrix(fit$vcov)
  
  possible_terms <- c(
    paste0(term, ":contextpublic"),
    paste0("contextpublic:", term)
  )
  
  interaction_term <- possible_terms[
    possible_terms %in% names(b)
  ][1]
  
  if (length(interaction_term) == 0 || is.na(interaction_term)) {
    return(
      empty_result(
        n_rows = nrow(fit$data),
        n_users = n_distinct(fit$data$user_id),
        n_occasions = n_distinct(fit$data$occasion_id),
        n_user_contexts = n_distinct(
          interaction(
            fit$data$user_id,
            fit$data$context,
            drop = TRUE
          )
        ),
        singular = isSingular(fit$model, tol = 1e-4)
      )
    )
  }
  
  estimate_i <- unname(b[interaction_term])
  se_i <- sqrt(unname(V[interaction_term, interaction_term]))
  
  tibble(
    outcome = outcome,
    outcome_label = nice_outcome_label(outcome),
    feature = feature,
    component = component,
    estimate = estimate_i,
    se = se_i,
    conf.low = estimate_i - 1.96 * se_i,
    conf.high = estimate_i + 1.96 * se_i,
    p.value = p_from_est_se(estimate_i, se_i),
    n_rows = nrow(fit$data),
    n_users = n_distinct(fit$data$user_id),
    n_occasions = n_distinct(fit$data$occasion_id),
    n_user_contexts = n_distinct(
      interaction(
        fit$data$user_id,
        fit$data$context,
        drop = TRUE
      )
    ),
    singular = isSingular(fit$model, tol = 1e-4),
    model_type = "lmer_within_between_CR2"
  )
}
  
  
############################
#### 7) RUNNER FUNCTIONS
############################

run_context_models <- function(
  data,
  outcomes,
  features,
  model_family,
  require_both_contexts = FALSE,
  component = "within"
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
          feature = feature_i,
          component = component
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
  require_both_contexts = FALSE,
  component = "within"
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
          feature = feature_i,
          component = component
        )
      }
    }))
  }))
}

trait_outcomes <- c("pa_trait_z", "na_trait_z")
daily_outcomes <- "daily_valence_z"
momentary_outcomes <- "momentary_valence_z"

############################
#### 8) THEORY-GUIDED MODELS
############################

theory_trait_context <- run_context_models(
  keyboard_data_trait_z, trait_outcomes, theory_features, "trait"
)
# Main daily and momentary results:
# within-person deviations from each participant's context-specific mean.
theory_daily_context <- run_context_models(
  keyboard_data_day_z,
  daily_outcomes,
  theory_features,
  "state",
  component = "within"
)

theory_momentary_context <- run_context_models(
  keyboard_data_moment_z,
  momentary_outcomes,
  theory_features,
  "state",
  component = "within"
)

# Corresponding between-person estimates, retained separately.
theory_daily_context_between <- run_context_models(
  keyboard_data_day_z,
  daily_outcomes,
  theory_features,
  "state",
  component = "between"
)

theory_momentary_context_between <- run_context_models(
  keyboard_data_moment_z,
  momentary_outcomes,
  theory_features,
  "state",
  component = "between"
)

theory_context_results_all <- bind_rows(
  theory_trait_context %>%
    mutate(
      timescale = "Trait",
      effect_level = "Between-person"
    ),
  theory_daily_context %>%
    mutate(
      timescale = "Daily",
      effect_level = "Within-person"
    ),
  theory_momentary_context %>%
    mutate(
      timescale = "Momentary",
      effect_level = "Within-person"
    )
) %>%
  left_join(theory_feature_lookup, by = "feature") %>%
  mutate(
    context_label = nice_context(context),
    outcome_label = factor(
      nice_outcome_label(outcome),
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

# Formal context-difference tests.
theory_trait_interactions <- run_interaction_models(
  keyboard_data_trait_z, trait_outcomes, theory_features, "trait"
)
# Main formal context moderation tests for within-person effects.
theory_daily_interactions <- run_interaction_models(
  keyboard_data_day_z,
  daily_outcomes,
  theory_features,
  "state",
  component = "within"
)

theory_momentary_interactions <- run_interaction_models(
  keyboard_data_moment_z,
  momentary_outcomes,
  theory_features,
  "state",
  component = "within"
)

# Separate context moderation tests for between-person effects.
theory_daily_interactions_between <- run_interaction_models(
  keyboard_data_day_z,
  daily_outcomes,
  theory_features,
  "state",
  component = "between"
)

theory_momentary_interactions_between <- run_interaction_models(
  keyboard_data_moment_z,
  momentary_outcomes,
  theory_features,
  "state",
  component = "between"
)

# BH correction is applied separately within each affect outcome.
theory_interaction_results_all <- bind_rows(
  theory_trait_interactions %>%
    mutate(
      timescale = "Trait",
      effect_level = "Between-person"
    ),
  theory_daily_interactions %>%
    mutate(
      timescale = "Daily",
      effect_level = "Within-person"
    ),
  theory_momentary_interactions %>%
    mutate(
      timescale = "Momentary",
      effect_level = "Within-person"
    )
) %>%
  left_join(theory_feature_lookup, by = "feature") %>%
  mutate(
    outcome_label = factor(
      nice_outcome_label(outcome),
      levels = c(
        "Trait NA",
        "Trait PA",
        "Daily valence",
        "Momentary valence"
      )
    ),
    feature_label = factor(
      feature_label,
      levels = unname(theory_feature_labels[theory_features])
    )
  ) %>%
  group_by(outcome) %>%
  mutate(p_fdr = p.adjust(p.value, method = "BH")) %>%
  ungroup() %>%
  arrange(outcome_label, feature_label)

############################
#### BETWEEN-PERSON DAILY / MOMENTARY RESULTS
############################

theory_between_context_results <- bind_rows(
  theory_daily_context_between %>%
    mutate(timescale = "Daily"),
  theory_momentary_context_between %>%
    mutate(timescale = "Momentary")
) %>%
  left_join(theory_feature_lookup, by = "feature") %>%
  mutate(
    context_label = nice_context(context),
    outcome_label = factor(
      nice_outcome_label(outcome),
      levels = c("Daily valence", "Momentary valence")
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


theory_between_interaction_results <- bind_rows(
  theory_daily_interactions_between %>%
    mutate(timescale = "Daily"),
  theory_momentary_interactions_between %>%
    mutate(timescale = "Momentary")
) %>%
  left_join(theory_feature_lookup, by = "feature") %>%
  mutate(
    outcome_label = factor(
      nice_outcome_label(outcome),
      levels = c("Daily valence", "Momentary valence")
    ),
    feature_label = factor(
      feature_label,
      levels = unname(theory_feature_labels[theory_features])
    )
  ) %>%
  group_by(outcome) %>%
  mutate(p_fdr = p.adjust(p.value, method = "BH")) %>%
  ungroup() %>%
  arrange(outcome_label, feature_label)


write.csv(
  theory_between_context_results,
  "results/theory_between_person_context_specific_results.csv",
  row.names = FALSE
)

write.csv(
  theory_between_interaction_results,
  "results/theory_between_person_context_interaction_results.csv",
  row.names = FALSE
)


############################
#### 9) BOTH-CONTEXT TRAIT SENSITIVITY
############################

theory_trait_context_both <- run_context_models(
  keyboard_data_trait_z, trait_outcomes, theory_features, "trait",
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
  mutate(p_fdr = p.adjust(p.value, method = "BH")) %>%
  ungroup() %>%
  arrange(outcome_label, feature_label, context)

theory_trait_interactions_both <- run_interaction_models(
  keyboard_data_trait_z, trait_outcomes, theory_features, "trait",
  require_both_contexts = TRUE
) %>%
  left_join(theory_feature_lookup, by = "feature") %>%
  mutate(
    timescale = "Trait",
    sensitivity_sample = "Both contexts",
    outcome_label = nice_outcome_label(outcome)
  ) %>%
  group_by(outcome) %>%
  mutate(p_fdr = p.adjust(p.value, method = "BH")) %>%
  ungroup() %>%
  arrange(outcome_label, feature_label)

write.csv(
  theory_context_results_all,
  "results/theory_context_specific_results_all.csv",
  row.names = FALSE
)
write.csv(
  theory_interaction_results_all,
  "results/theory_context_interaction_results_all.csv",
  row.names = FALSE
)
write.csv(
  theory_trait_context_both,
  "results/theory_trait_context_specific_results_both_contexts.csv",
  row.names = FALSE
)
write.csv(
  theory_trait_interactions_both,
  "results/theory_trait_context_interactions_both_contexts.csv",
  row.names = FALSE
)

############################
#### 10) FIGURE 4
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
    feature_label_plot = factor(as.character(feature_label), levels = feature_order_main)
  )

context_cols <- c("Private" = "#E69F00", "Public" = "#56B4E9")
context_shapes <- c("Private" = 16, "Public" = 17)
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
    aes(ymin = conf.low, ymax = conf.high),
    position = pd_context,
    linewidth = 0.75,
    alpha = 0.85
  ) +
  geom_point(
    position = pd_context,
    size = 2.8,
    alpha = 0.95
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
  scale_color_manual(values = context_cols, name = "Context") +
  scale_shape_manual(values = context_shapes, name = "Context") +
  scale_y_continuous(
    breaks = seq(-0.30, 0.30, by = 0.10),
    labels = scales::label_number(accuracy = 0.1, trim = TRUE)
  ) +
  coord_cartesian(ylim = c(-0.30, 0.30)) +
  labs(x = NULL, y = "Standardized regression coefficient") +
  base_theme +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title = element_text(size = 10.5, face = "bold"),
    legend.text = element_text(size = 10),
    legend.margin = margin(b = -4),
    strip.background = element_rect(fill = "gray96", color = "gray60", linewidth = 0.7),
    strip.text = element_text(face = "bold", size = 10.5),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 9),
    axis.text.y = element_text(size = 9.2),
    axis.title.y = element_text(size = 10.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing.y = unit(1.0, "lines"),
    plot.margin = margin(8, 10, 6, 8)
  ) +
  guides(
    color = guide_legend(order = 1, override.aes = list(size = 3.2, alpha = 1, linewidth = 0.8)),
    shape = guide_legend(order = 1, override.aes = list(size = 3.2, alpha = 1, linewidth = 0.8))
  )

fig4

ggsave(
  filename = "figures/figure_4_theory_guided_liwc_regression_results.png",
  plot = fig4,
  width = 6,
  height = 10,
  dpi = 300
)

############################
#### 11) TABLE S6
#### CONTEXT SLOPES + INTERACTIONS
############################

table_s6_context <- theory_context_results_all %>%
  mutate(
    Context = nice_context(context),
    beta_ci = fmt_beta_ci(
      estimate,
      conf.low,
      conf.high,
      digits = 2
    )
  ) %>%
  select(
    outcome,
    feature,
    effect_level,
    Context,
    beta_ci
  ) %>%
  pivot_wider(
    names_from = Context,
    values_from = beta_ci
  )

table_s6_interactions <- theory_interaction_results_all %>%
  transmute(
    outcome,
    feature,
    interaction_beta_ci = fmt_beta_ci(
      estimate,
      conf.low,
      conf.high,
      digits = 2
    ),
    interaction_p = p.value,
    interaction_p_fdr = p_fdr
  )

table_s6 <- table_s6_context %>%
  left_join(
    table_s6_interactions,
    by = c("outcome", "feature")
  ) %>%
  left_join(
    theory_feature_lookup,
    by = "feature"
  ) %>%
  mutate(
    Outcome = factor(
      nice_outcome_label(outcome),
      levels = c(
        "Trait NA",
        "Trait PA",
        "Daily valence",
        "Momentary valence"
      )
    ),
    Feature = factor(
      feature_label,
      levels = feature_order_main
    )
  ) %>%
  arrange(Outcome, Feature) %>%
  transmute(
    Outcome = as.character(Outcome),
    `Effect level` = effect_level,
    Feature = as.character(Feature),
    `Private beta [95% CI]` = Private,
    `Public beta [95% CI]` = Public,
    `Context interaction beta [95% CI]` =
      interaction_beta_ci,
    `Interaction p` = signif(interaction_p, 3),
    `Interaction pFDR` = signif(
      interaction_p_fdr,
      3
    )
  )

write.csv(
  table_s6,
  "results/table_s6_theory_guided_liwc_regression_results.csv",
  row.names = FALSE,
  na = ""
)

############################
#### 12) FULL ALL-LIWC REPOSITORY
############################

all_liwc_trait_context <- run_context_models(
  keyboard_data_trait_z, trait_outcomes, all_liwc_features, "trait"
)
all_liwc_daily_context <- run_context_models(
  keyboard_data_day_z,
  daily_outcomes,
  all_liwc_features,
  "state",
  component = "within"
)

all_liwc_momentary_context <- run_context_models(
  keyboard_data_moment_z,
  momentary_outcomes,
  all_liwc_features,
  "state",
  component = "within"
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

all_liwc_trait_interactions <- run_interaction_models(
  keyboard_data_trait_z, trait_outcomes, all_liwc_features, "trait"
)
all_liwc_daily_interactions <- run_interaction_models(
  keyboard_data_day_z,
  daily_outcomes,
  all_liwc_features,
  "state",
  component = "within"
)

all_liwc_momentary_interactions <- run_interaction_models(
  keyboard_data_moment_z,
  momentary_outcomes,
  all_liwc_features,
  "state",
  component = "within"
)

all_liwc_interaction_results_all <- bind_rows(
  all_liwc_trait_interactions %>% mutate(timescale = "Trait"),
  all_liwc_daily_interactions %>% mutate(timescale = "Daily"),
  all_liwc_momentary_interactions %>% mutate(timescale = "Momentary")
) %>%
  left_join(all_liwc_feature_lookup, by = "feature") %>%
  mutate(outcome_label = nice_outcome_label(outcome)) %>%
  group_by(timescale, outcome) %>%
  mutate(p_fdr = p.adjust(p.value, method = "BH")) %>%
  ungroup() %>%
  arrange(timescale, outcome_label, feature)

model_diagnostics <- bind_rows(
  theory_daily_context %>%
    mutate(timescale = "Daily"),
  theory_momentary_context %>%
    mutate(timescale = "Momentary")
) %>%
  distinct(
    timescale,
    outcome,
    feature,
    n_rows,
    n_users,
    n_occasions,
    n_user_contexts,
    singular,
    model_type
  )

print(model_diagnostics)


############################
#### 13) TABLE S8
############################

table_s7_long <- all_liwc_context_results_all %>%
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

table_s7_ranked_features <- table_s7_long %>%
  group_by(feature, feature_label) %>%
  summarise(
    overall_abs_trait_affect_association = sum(abs(estimate), na.rm = TRUE),
    n_available_coefficients = sum(!is.na(estimate)),
    .groups = "drop"
  ) %>%
  filter(n_available_coefficients == 4) %>%
  arrange(desc(overall_abs_trait_affect_association)) %>%
  slice_head(n = 10)

table_s7 <- table_s7_long %>%
  semi_join(table_s7_ranked_features, by = c("feature", "feature_label")) %>%
  select(feature, feature_label, column_name, beta_ci) %>%
  pivot_wider(names_from = column_name, values_from = beta_ci) %>%
  left_join(
    table_s7_ranked_features %>% select(feature, overall_abs_trait_affect_association),
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
  table_s7,
  "results/table_s_top_liwc_trait_affect_associations.csv",
  row.names = FALSE,
  na = ""
)

############################
#### 14) PRINT OUTPUTS
############################

print(theory_context_results_all)
print(theory_interaction_results_all)
print(theory_trait_context_both)
print(theory_trait_interactions_both)
print(table_s6)
print(table_s7)
fig4

# finish
