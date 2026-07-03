############################
#### EMOJI / EMOTICON × AFFECT ANALYSES
#### TABLE S8 + FULL REPOSITORY CSV
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
  "lmtest",
  "sandwich",
  "clubSandwich",
  "lme4"
)

invisible(
  lapply(
    required_packages,
    library,
    character.only = TRUE
  )
)

dir.create(
  "results",
  recursive = TRUE,
  showWarnings = FALSE
)

set.seed(123)

############################
#### 1) GENERAL HELPERS
############################

z_scale <- function(x) {
  
  s <- sd(
    x,
    na.rm = TRUE
  )
  
  if (
    is.na(s) ||
    s == 0
  ) {
    return(
      rep(
        NA_real_,
        length(x)
      )
    )
  }
  
  as.numeric(
    scale(x)
  )
}


safe_sd <- function(x) {
  
  sd(
    x,
    na.rm = TRUE
  )
}


p_from_est_se <- function(
  estimate,
  se
) {
  
  ifelse(
    is.na(estimate) |
      is.na(se) |
      se <= 0,
    NA_real_,
    2 * pnorm(
      abs(estimate / se),
      lower.tail = FALSE
    )
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


fmt_beta_ci <- function(
  beta,
  low,
  high,
  digits = 2
) {
  
  ifelse(
    is.na(beta) |
      is.na(low) |
      is.na(high),
    NA_character_,
    paste0(
      formatC(
        beta,
        format = "f",
        digits = digits
      ),
      " [",
      formatC(
        low,
        format = "f",
        digits = digits
      ),
      ", ",
      formatC(
        high,
        format = "f",
        digits = digits
      ),
      "]"
    )
  )
}

############################
#### 2) SYMBOL-NAME HELPERS
############################

to_symbol_from_emoji_feature <- function(feature_name) {
  
  code <- stringr::str_match(
    feature_name,
    "^emoji_([0-9]+)_share$"
  )[, 2]
  
  if (is.na(code)) {
    return(feature_name)
  }
  
  tryCatch(
    intToUtf8(
      as.integer(code)
    ),
    error = function(e) {
      feature_name
    }
  )
}


to_symbol_from_emoticon_feature <- function(feature_name) {
  
  x <- feature_name %>%
    stringr::str_remove(
      "^emoticon_"
    ) %>%
    stringr::str_remove(
      "_share$"
    )
  
  x <- x %>%
    stringr::str_replace_all(
      "__COLON__",
      ":"
    ) %>%
    stringr::str_replace_all(
      "__SEMICOLON__",
      ";"
    ) %>%
    stringr::str_replace_all(
      "__EQUALS__",
      "="
    ) %>%
    stringr::str_replace_all(
      "__DASH__",
      "-"
    ) %>%
    stringr::str_replace_all(
      "__UNDERSCORE__",
      "_"
    ) %>%
    stringr::str_replace_all(
      "__LPAREN__",
      "("
    ) %>%
    stringr::str_replace_all(
      "__RPAREN__",
      ")"
    ) %>%
    stringr::str_replace_all(
      "__SLASH__",
      "/"
    ) %>%
    stringr::str_replace_all(
      "__BACKSLASH__",
      "\\\\"
    ) %>%
    stringr::str_replace_all(
      "__APOSTROPHE__",
      "'"
    )
  
  ifelse(
    nchar(x) > 0,
    x,
    feature_name
  )
}


get_symbol_type <- function(feature_name) {
  
  dplyr::case_when(
    stringr::str_detect(
      feature_name,
      "^emoji_[0-9]+_share$"
    ) ~ "Emoji",
    
    stringr::str_detect(
      feature_name,
      "^emoticon_.*_share$"
    ) ~ "Emoticon",
    
    TRUE ~ "Other"
  )
}


get_symbol_label <- function(feature_name) {
  
  type <- get_symbol_type(
    feature_name
  )
  
  if (type == "Emoji") {
    return(
      to_symbol_from_emoji_feature(
        feature_name
      )
    )
  }
  
  if (type == "Emoticon") {
    return(
      to_symbol_from_emoticon_feature(
        feature_name
      )
    )
  }
  
  feature_name
}


get_numeric_symbol_features <- function(df) {
  
  candidate_features <- names(df) %>%
    stringr::str_subset(
      "^(emoji_[0-9]+_share|emoticon_.*_share)$"
    )
  
  candidate_features[
    vapply(
      df[candidate_features],
      is.numeric,
      logical(1)
    )
  ]
}

############################
#### 3) LOAD DATA
############################

keyboard_data_trait_raw <- readRDS(
  "data/results/keyboard_data_trait_final.rds"
) %>%
  as.data.frame() %>%
  mutate(
    user_id = as.character(user_id)
  )


keyboard_data_trait <- keyboard_data_trait_raw %>%
  filter(
    scope %in% c(
      "private",
      "public"
    )
  ) %>%
  mutate(
    context = factor(
      scope,
      levels = c(
        "private",
        "public"
      )
    )
  )


keyboard_data_day <- readRDS(
  "data/results/keyboard_data_day_final.rds"
) %>%
  as.data.frame() %>%
  filter(
    scope %in% c(
      "private",
      "public"
    )
  ) %>%
  mutate(
    user_id = as.character(user_id),
    context = factor(
      scope,
      levels = c(
        "private",
        "public"
      )
    ),
    date = as.Date(date),
    occasion_id = interaction(
      user_id,
      date,
      drop = TRUE
    )
  )


keyboard_data_moment <- readRDS(
  "data/results/keyboard_data_ema_final.rds"
) %>%
  as.data.frame() %>%
  filter(
    scope %in% c(
      "private",
      "public"
    )
  ) %>%
  mutate(
    user_id = as.character(user_id),
    context = factor(
      scope,
      levels = c(
        "private",
        "public"
      )
    ),
    occasion_id = interaction(
      user_id,
      es_questionnaire_id,
      drop = TRUE
    )
  )


stopifnot(
  all(
    c(
      "user_id",
      "scope",
      "pa_panas",
      "na_panas"
    ) %in%
      names(keyboard_data_trait_raw)
  ),
  
  all(
    c(
      "user_id",
      "scope",
      "daily_valence",
      "date"
    ) %in%
      names(keyboard_data_day)
  ),
  
  all(
    c(
      "user_id",
      "scope",
      "valence",
      "es_questionnaire_id"
    ) %in%
      names(keyboard_data_moment)
  )
)

############################
#### 4) OCCASION DIAGNOSTICS
############################

write.csv(
  keyboard_data_day %>%
    count(
      user_id,
      date,
      name = "n_context_rows"
    ) %>%
    count(
      n_context_rows,
      name = "n_occasions"
    ),
  "results/check_symbol_daily_rows_per_occasion.csv",
  row.names = FALSE
)


write.csv(
  keyboard_data_moment %>%
    count(
      user_id,
      es_questionnaire_id,
      name = "n_context_rows"
    ) %>%
    count(
      n_context_rows,
      name = "n_occasions"
    ),
  "results/check_symbol_momentary_rows_per_occasion.csv",
  row.names = FALSE
)

############################
#### 5) IDENTIFY SYMBOL FEATURES
############################

symbol_features_trait <- get_numeric_symbol_features(
  keyboard_data_trait_raw
)

symbol_features_day <- get_numeric_symbol_features(
  keyboard_data_day
)

symbol_features_moment <- get_numeric_symbol_features(
  keyboard_data_moment
)


if (length(symbol_features_trait) == 0) {
  
  stop(
    paste0(
      "No numeric emoji or emoticon share features ",
      "were found in the trait dataset."
    )
  )
}


symbol_lookup_trait <- tibble(
  feature = symbol_features_trait,
  
  symbol_type = vapply(
    symbol_features_trait,
    get_symbol_type,
    character(1)
  ),
  
  symbol = vapply(
    symbol_features_trait,
    get_symbol_label,
    character(1)
  )
)


write.csv(
  symbol_lookup_trait,
  "results/symbol_feature_lookup.csv",
  row.names = FALSE
)


message(
  "Trait-level symbol features found: ",
  length(symbol_features_trait),
  " (",
  sum(
    symbol_lookup_trait$symbol_type == "Emoji"
  ),
  " emoji; ",
  sum(
    symbol_lookup_trait$symbol_type == "Emoticon"
  ),
  " emoticon)"
)

############################
#### 6) PARTICIPANT-LEVEL PREVALENCE
############################

# Retain symbols used by at least 10% of participants overall.
#
# Prefer scope == "all" rows. If these are not available,
# reconstruct overall use from the private and public rows.

if (
  "all" %in%
  keyboard_data_trait_raw$scope
) {
  
  symbol_prevalence_source <- keyboard_data_trait_raw %>%
    filter(
      scope == "all"
    ) %>%
    select(
      user_id,
      all_of(
        symbol_features_trait
      )
    )
  
} else {
  
  symbol_prevalence_source <- keyboard_data_trait_raw %>%
    filter(
      scope %in% c(
        "private",
        "public"
      )
    ) %>%
    select(
      user_id,
      all_of(
        symbol_features_trait
      )
    ) %>%
    group_by(
      user_id
    ) %>%
    summarise(
      across(
        all_of(
          symbol_features_trait
        ),
        ~ {
          if (all(is.na(.x))) {
            NA_real_
          } else {
            max(
              .x,
              na.rm = TRUE
            )
          }
        }
      ),
      .groups = "drop"
    )
}


symbol_prevalence_trait <- symbol_prevalence_source %>%
  pivot_longer(
    cols = all_of(
      symbol_features_trait
    ),
    names_to = "feature",
    values_to = "value"
  ) %>%
  group_by(
    feature
  ) %>%
  summarise(
    n_users_observed = n_distinct(
      user_id[
        !is.na(value)
      ]
    ),
    
    n_users_nonzero = n_distinct(
      user_id[
        !is.na(value) &
          value > 0
      ]
    ),
    
    prop_users_nonzero = ifelse(
      n_users_observed > 0,
      n_users_nonzero /
        n_users_observed,
      NA_real_
    ),
    
    .groups = "drop"
  ) %>%
  left_join(
    symbol_lookup_trait,
    by = "feature"
  ) %>%
  arrange(
    desc(
      prop_users_nonzero
    )
  )


symbol_keep <- symbol_prevalence_trait %>%
  filter(
    !is.na(
      prop_users_nonzero
    ),
    prop_users_nonzero >= 0.10
  )


if (nrow(symbol_keep) == 0) {
  
  stop(
    paste0(
      "No emoji or emoticon features met the ",
      "10% participant-level prevalence threshold."
    )
  )
}


symbol_features_keep_trait <- intersect(
  symbol_features_trait,
  symbol_keep$feature
)

symbol_features_keep_day <- intersect(
  symbol_features_day,
  symbol_keep$feature
)

symbol_features_keep_moment <- intersect(
  symbol_features_moment,
  symbol_keep$feature
)


message(
  "Symbols retained at trait level: ",
  length(
    symbol_features_keep_trait
  )
)

message(
  "Retained symbols available at daily level: ",
  length(
    symbol_features_keep_day
  )
)

message(
  "Retained symbols available at momentary level: ",
  length(
    symbol_features_keep_moment
  )
)


write.csv(
  symbol_prevalence_trait,
  "results/repository_symbol_prevalence_trait_overall.csv",
  row.names = FALSE
)


write.csv(
  symbol_keep,
  "results/repository_symbol_features_retained_for_analysis.csv",
  row.names = FALSE
)

############################
#### 7) PREPARE ANALYSIS DATA
############################

trait_outcome_data <- keyboard_data_trait %>%
  distinct(
    user_id,
    pa_panas,
    na_panas
  ) %>%
  filter(
    !is.na(pa_panas),
    !is.na(na_panas)
  ) %>%
  mutate(
    pa_trait_z = z_scale(
      pa_panas
    ),
    na_trait_z = z_scale(
      na_panas
    )
  ) %>%
  select(
    user_id,
    pa_trait_z,
    na_trait_z
  )


keyboard_data_trait_symbol_z <- keyboard_data_trait %>%
  select(
    user_id,
    scope,
    context,
    all_of(
      symbol_features_keep_trait
    )
  ) %>%
  left_join(
    trait_outcome_data,
    by = "user_id"
  ) %>%
  mutate(
    across(
      all_of(
        symbol_features_keep_trait
      ),
      z_scale,
      .names = "{.col}_z"
    )
  )


keyboard_data_day_symbol <- keyboard_data_day %>%
  mutate(
    daily_valence_z = z_scale(
      daily_valence
    ),
    occasion_id = factor(
      occasion_id
    )
  )


keyboard_data_moment_symbol <- keyboard_data_moment %>%
  mutate(
    momentary_valence_z = z_scale(
      valence
    ),
    occasion_id = factor(
      occasion_id
    )
  )


trait_outcomes <- c(
  "pa_trait_z",
  "na_trait_z"
)

daily_outcomes <- "daily_valence_z"

momentary_outcomes <- "momentary_valence_z"

############################
#### 8) TRAIT MODEL HELPERS
############################

coef_by_context_lm <- function(
  model,
  term,
  vcov_mat,
  conf = 0.95
) {
  
  b <- coef(model)
  
  int_term_1 <- paste0(
    term,
    ":contextpublic"
  )
  
  int_term_2 <- paste0(
    "contextpublic:",
    term
  )
  
  int_term <- if (
    int_term_1 %in%
    names(b)
  ) {
    int_term_1
  } else if (
    int_term_2 %in%
    names(b)
  ) {
    int_term_2
  } else {
    NA_character_
  }
  
  
  if (
    !term %in%
    names(b)
  ) {
    
    return(
      tibble(
        context = c(
          "private",
          "public"
        ),
        estimate = NA_real_,
        se = NA_real_,
        conf.low = NA_real_,
        conf.high = NA_real_,
        p.value = NA_real_
      )
    )
  }
  
  
  z_crit <- qnorm(
    1 - (1 - conf) / 2
  )
  
  est_private <- unname(
    b[term]
  )
  
  se_private <- sqrt(
    unname(
      vcov_mat[
        term,
        term
      ]
    )
  )
  
  
  if (
    !is.na(int_term) &&
    int_term %in%
    rownames(vcov_mat)
  ) {
    
    est_public <- unname(
      b[term] +
        b[int_term]
    )
    
    var_public <- unname(
      vcov_mat[
        term,
        term
      ] +
        vcov_mat[
          int_term,
          int_term
        ] +
        2 *
        vcov_mat[
          term,
          int_term
        ]
    )
    
    se_public <- sqrt(
      pmax(
        var_public,
        0
      )
    )
    
  } else {
    
    est_public <- NA_real_
    se_public <- NA_real_
  }
  
  
  tibble(
    context = c(
      "private",
      "public"
    ),
    
    estimate = c(
      est_private,
      est_public
    ),
    
    se = c(
      se_private,
      se_public
    ),
    
    conf.low = c(
      est_private -
        z_crit *
        se_private,
      
      est_public -
        z_crit *
        se_public
    ),
    
    conf.high = c(
      est_private +
        z_crit *
        se_private,
      
      est_public +
        z_crit *
        se_public
    ),
    
    p.value = p_from_est_se(
      estimate = c(
        est_private,
        est_public
      ),
      se = c(
        se_private,
        se_public
      )
    )
  )
}


fit_trait_symbol_model <- function(
  data,
  outcome,
  feature
) {
  
  feature_z <- paste0(
    feature,
    "_z"
  )
  
  
  dat <- data %>%
    filter(
      !is.na(
        .data[[outcome]]
      ),
      !is.na(
        .data[[feature_z]]
      ),
      !is.na(context),
      !is.na(user_id)
    ) %>%
    mutate(
      context = factor(
        context,
        levels = c(
          "private",
          "public"
        )
      )
    )
  
  
  if (
    nrow(dat) < 30 ||
    n_distinct(
      dat$user_id
    ) < 20 ||
    n_distinct(
      dat$context
    ) < 2 ||
    is.na(
      safe_sd(
        dat[[outcome]]
      )
    ) ||
    safe_sd(
      dat[[outcome]]
    ) == 0 ||
    is.na(
      safe_sd(
        dat[[feature_z]]
      )
    ) ||
    safe_sd(
      dat[[feature_z]]
    ) == 0
  ) {
    
    return(NULL)
  }
  
  
  formula_i <- as.formula(
    paste0(
      outcome,
      " ~ ",
      feature_z,
      " * context"
    )
  )
  
  
  model_i <- lm(
    formula_i,
    data = dat
  )
  
  
  vcov_i <- tryCatch(
    sandwich::vcovCL(
      model_i,
      cluster = dat$user_id,
      type = "HC1"
    ),
    error = function(e) {
      
      message(
        "Cluster-robust trait covariance failed for ",
        outcome,
        " × ",
        feature,
        ": ",
        conditionMessage(e),
        ". Using HC3."
      )
      
      sandwich::vcovHC(
        model_i,
        type = "HC3"
      )
    }
  )
  
  
  list(
    model = model_i,
    vcov = vcov_i,
    data = dat,
    feature_z = feature_z
  )
}


extract_trait_context_results <- function(
  fit,
  outcome,
  feature
) {
  
  if (is.null(fit)) {
    
    return(
      tibble(
        outcome = outcome,
        outcome_label = nice_outcome_label(
          outcome
        ),
        feature = feature,
        context = c(
          "private",
          "public"
        ),
        estimate = NA_real_,
        se = NA_real_,
        conf.low = NA_real_,
        conf.high = NA_real_,
        p.value = NA_real_,
        n_rows = NA_integer_,
        n_users = NA_integer_,
        model_type = "lm_cluster_robust"
      )
    )
  }
  
  
  coef_by_context_lm(
    model = fit$model,
    term = fit$feature_z,
    vcov_mat = fit$vcov
  ) %>%
    mutate(
      outcome = outcome,
      outcome_label = nice_outcome_label(
        outcome
      ),
      feature = feature,
      n_rows = nrow(
        fit$data
      ),
      n_users = n_distinct(
        fit$data$user_id
      ),
      model_type = "lm_cluster_robust",
      .before = 1
    )
}


extract_trait_interaction_results <- function(
  fit,
  outcome,
  feature
) {
  
  if (is.null(fit)) {
    
    return(
      tibble(
        outcome = outcome,
        outcome_label = nice_outcome_label(
          outcome
        ),
        feature = feature,
        estimate = NA_real_,
        se = NA_real_,
        conf.low = NA_real_,
        conf.high = NA_real_,
        p.value = NA_real_,
        n_rows = NA_integer_,
        n_users = NA_integer_,
        model_type = "lm_cluster_robust"
      )
    )
  }
  
  
  tidy_i <- broom::tidy(
    lmtest::coeftest(
      fit$model,
      vcov. = fit$vcov
    )
  )
  
  
  possible_terms <- c(
    paste0(
      fit$feature_z,
      ":contextpublic"
    ),
    paste0(
      "contextpublic:",
      fit$feature_z
    )
  )
  
  
  result_i <- tidy_i %>%
    filter(
      term %in%
        possible_terms
    ) %>%
    slice(1)
  
  
  if (nrow(result_i) == 0) {
    
    return(
      tibble(
        outcome = outcome,
        outcome_label = nice_outcome_label(
          outcome
        ),
        feature = feature,
        estimate = NA_real_,
        se = NA_real_,
        conf.low = NA_real_,
        conf.high = NA_real_,
        p.value = NA_real_,
        n_rows = nrow(
          fit$data
        ),
        n_users = n_distinct(
          fit$data$user_id
        ),
        model_type = "lm_cluster_robust"
      )
    )
  }
  
  
  result_i %>%
    transmute(
      outcome = outcome,
      outcome_label = nice_outcome_label(
        outcome
      ),
      feature = feature,
      estimate = estimate,
      se = std.error,
      conf.low = estimate -
        1.96 *
        std.error,
      conf.high = estimate +
        1.96 *
        std.error,
      p.value = p.value,
      n_rows = nrow(
        fit$data
      ),
      n_users = n_distinct(
        fit$data$user_id
      ),
      model_type = "lm_cluster_robust"
    )
}

############################
#### 9) STATE MODEL HELPERS
############################

fit_mixed_model <- function(
  formula,
  data
) {
  
  tryCatch(
    suppressWarnings(
      lme4::lmer(
        formula = formula,
        data = data,
        REML = FALSE,
        control = lme4::lmerControl(
          optimizer = "bobyqa",
          optCtrl = list(
            maxfun = 2e5
          )
        )
      )
    ),
    error = function(e) {
      
      message(
        "Mixed model failed: ",
        conditionMessage(e)
      )
      
      NULL
    }
  )
}


standardize_between_component <- function(
  dat,
  mean_var
) {
  
  between_lookup <- dat %>%
    distinct(
      user_id,
      context,
      .data[[mean_var]]
    ) %>%
    mutate(
      between_z = z_scale(
        .data[[mean_var]]
      )
    ) %>%
    select(
      user_id,
      context,
      between_z
    )
  
  
  dat %>%
    left_join(
      between_lookup,
      by = c(
        "user_id",
        "context"
      )
    )
}


prepare_decomposed_symbol <- function(
  data,
  outcome,
  feature
) {
  
  dat <- data %>%
    filter(
      !is.na(
        .data[[outcome]]
      ),
      !is.na(
        .data[[feature]]
      ),
      !is.na(context),
      !is.na(user_id),
      !is.na(occasion_id)
    ) %>%
    mutate(
      user_id = factor(
        user_id
      ),
      context = factor(
        context,
        levels = c(
          "private",
          "public"
        )
      ),
      occasion_id = factor(
        occasion_id
      )
    )
  
  
  if (nrow(dat) == 0) {
    
    return(NULL)
  }
  
  
  # Decomposition is performed within participant × context.
  #
  # feature_person_mean:
  # participant's typical use of the symbol in that context.
  #
  # feature_within_raw:
  # occasion-specific deviation from typical use in that context.
  
  dat <- dat %>%
    group_by(
      user_id,
      context
    ) %>%
    mutate(
      feature_person_mean = mean(
        .data[[feature]],
        na.rm = TRUE
      ),
      
      feature_within_raw =
        .data[[feature]] -
        feature_person_mean,
      
      n_user_context_observations = n()
    ) %>%
    ungroup()
  
  
  dat <- standardize_between_component(
    dat = dat,
    mean_var = "feature_person_mean"
  ) %>%
    mutate(
      within_z = z_scale(
        feature_within_raw
      )
    )
  
  
  dat
}


get_cluster_robust_vcov <- function(
  model,
  data
) {
  
  clubSandwich::vcovCR(
    model,
    cluster = data$user_id,
    type = "CR2"
  )
}


fit_state_symbol_model <- function(
  data,
  outcome,
  feature
) {
  
  dat <- prepare_decomposed_symbol(
    data = data,
    outcome = outcome,
    feature = feature
  )
  
  
  if (is.null(dat)) {
    
    return(NULL)
  }
  
  
  if (
    nrow(dat) < 30 ||
    n_distinct(
      dat$user_id
    ) < 10 ||
    n_distinct(
      dat$context
    ) < 2 ||
    is.na(
      safe_sd(
        dat[[outcome]]
      )
    ) ||
    safe_sd(
      dat[[outcome]]
    ) == 0 ||
    is.na(
      safe_sd(
        dat$between_z
      )
    ) ||
    safe_sd(
      dat$between_z
    ) == 0 ||
    is.na(
      safe_sd(
        dat$within_z
      )
    ) ||
    safe_sd(
      dat$within_z
    ) == 0
  ) {
    
    return(NULL)
  }
  
  
  formula_i <- as.formula(
    paste0(
      outcome,
      " ~ between_z * context",
      " + within_z * context",
      " + (1 | user_id)"
    )
  )
  
  
  model_i <- fit_mixed_model(
    formula = formula_i,
    data = dat
  )
  
  
  if (is.null(model_i)) {
    
    return(NULL)
  }
  
  
  vcov_cr2 <- tryCatch(
    get_cluster_robust_vcov(
      model = model_i,
      data = dat
    ),
    error = function(e) {
      
      message(
        "CR2 covariance failed for ",
        outcome,
        " × ",
        feature,
        ": ",
        conditionMessage(e)
      )
      
      NULL
    }
  )
  
  
  if (is.null(vcov_cr2)) {
    
    return(NULL)
  }
  
  
  list(
    model = model_i,
    vcov = vcov_cr2,
    data = dat,
    between_term = "between_z",
    within_term = "within_z",
    feature = feature
  )
}


coef_by_context_lmer <- function(
  model,
  term,
  vcov_mat,
  conf = 0.95
) {
  
  if (is.null(model)) {
    
    return(
      tibble(
        context = c(
          "private",
          "public"
        ),
        estimate = NA_real_,
        se = NA_real_,
        conf.low = NA_real_,
        conf.high = NA_real_,
        p.value = NA_real_
      )
    )
  }
  
  
  b <- lme4::fixef(
    model
  )
  
  V <- as.matrix(
    vcov_mat
  )
  
  
  int_term_1 <- paste0(
    term,
    ":contextpublic"
  )
  
  int_term_2 <- paste0(
    "contextpublic:",
    term
  )
  
  
  int_term <- if (
    int_term_1 %in%
    names(b)
  ) {
    int_term_1
  } else if (
    int_term_2 %in%
    names(b)
  ) {
    int_term_2
  } else {
    NA_character_
  }
  
  
  if (
    !term %in%
    names(b)
  ) {
    
    return(
      tibble(
        context = c(
          "private",
          "public"
        ),
        estimate = NA_real_,
        se = NA_real_,
        conf.low = NA_real_,
        conf.high = NA_real_,
        p.value = NA_real_
      )
    )
  }
  
  
  z_crit <- qnorm(
    1 - (1 - conf) / 2
  )
  
  
  est_private <- unname(
    b[term]
  )
  
  se_private <- sqrt(
    unname(
      V[
        term,
        term
      ]
    )
  )
  
  
  if (
    !is.na(int_term) &&
    int_term %in%
    rownames(V)
  ) {
    
    est_public <- unname(
      b[term] +
        b[int_term]
    )
    
    var_public <- unname(
      V[
        term,
        term
      ] +
        V[
          int_term,
          int_term
        ] +
        2 *
        V[
          term,
          int_term
        ]
    )
    
    se_public <- sqrt(
      pmax(
        var_public,
        0
      )
    )
    
  } else {
    
    est_public <- NA_real_
    se_public <- NA_real_
  }
  
  
  tibble(
    context = c(
      "private",
      "public"
    ),
    
    estimate = c(
      est_private,
      est_public
    ),
    
    se = c(
      se_private,
      se_public
    ),
    
    conf.low = c(
      est_private -
        z_crit *
        se_private,
      
      est_public -
        z_crit *
        se_public
    ),
    
    conf.high = c(
      est_private +
        z_crit *
        se_private,
      
      est_public +
        z_crit *
        se_public
    ),
    
    p.value = p_from_est_se(
      estimate = c(
        est_private,
        est_public
      ),
      se = c(
        se_private,
        se_public
      )
    )
  )
}


extract_state_context_results <- function(
  fit,
  outcome,
  feature,
  component = c(
    "within",
    "between"
  )
) {
  
  component <- match.arg(
    component
  )
  
  
  if (is.null(fit)) {
    
    return(
      tibble(
        outcome = outcome,
        outcome_label = nice_outcome_label(
          outcome
        ),
        feature = feature,
        component = component,
        context = c(
          "private",
          "public"
        ),
        estimate = NA_real_,
        se = NA_real_,
        conf.low = NA_real_,
        conf.high = NA_real_,
        p.value = NA_real_,
        n_rows = NA_integer_,
        n_users = NA_integer_,
        n_occasions = NA_integer_,
        n_user_contexts = NA_integer_,
        n_users_repeated_private = NA_integer_,
        n_users_repeated_public = NA_integer_,
        singular = NA,
        model_type = "lmer_within_between_CR2"
      )
    )
  }
  
  
  term <- if (
    component == "within"
  ) {
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
      
      outcome_label = nice_outcome_label(
        outcome
      ),
      
      feature = feature,
      
      component = component,
      
      n_rows = nrow(
        fit$data
      ),
      
      n_users = n_distinct(
        fit$data$user_id
      ),
      
      n_occasions = n_distinct(
        fit$data$occasion_id
      ),
      
      n_user_contexts = n_distinct(
        interaction(
          fit$data$user_id,
          fit$data$context,
          drop = TRUE
        )
      ),
      
      n_users_repeated_private = n_distinct(
        fit$data$user_id[
          fit$data$context == "private" &
            fit$data$n_user_context_observations >= 2
        ]
      ),
      
      n_users_repeated_public = n_distinct(
        fit$data$user_id[
          fit$data$context == "public" &
            fit$data$n_user_context_observations >= 2
        ]
      ),
      
      singular = lme4::isSingular(
        fit$model,
        tol = 1e-4
      ),
      
      model_type = "lmer_within_between_CR2",
      
      .before = 1
    )
}


extract_state_interaction_results <- function(
  fit,
  outcome,
  feature,
  component = c(
    "within",
    "between"
  )
) {
  
  component <- match.arg(
    component
  )
  
  
  empty_result <- function(
    n_rows = NA_integer_,
    n_users = NA_integer_,
    n_occasions = NA_integer_,
    n_user_contexts = NA_integer_,
    n_users_repeated_private = NA_integer_,
    n_users_repeated_public = NA_integer_,
    singular = NA
  ) {
    
    tibble(
      outcome = outcome,
      outcome_label = nice_outcome_label(
        outcome
      ),
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
      n_users_repeated_private =
        n_users_repeated_private,
      n_users_repeated_public =
        n_users_repeated_public,
      singular = singular,
      model_type = "lmer_within_between_CR2"
    )
  }
  
  
  if (is.null(fit)) {
    
    return(
      empty_result()
    )
  }
  
  
  term <- if (
    component == "within"
  ) {
    fit$within_term
  } else {
    fit$between_term
  }
  
  
  b <- lme4::fixef(
    fit$model
  )
  
  V <- as.matrix(
    fit$vcov
  )
  
  
  possible_terms <- c(
    paste0(
      term,
      ":contextpublic"
    ),
    paste0(
      "contextpublic:",
      term
    )
  )
  
  
  interaction_term <- possible_terms[
    possible_terms %in%
      names(b)
  ][1]
  
  
  fit_diagnostics <- list(
    n_rows = nrow(
      fit$data
    ),
    
    n_users = n_distinct(
      fit$data$user_id
    ),
    
    n_occasions = n_distinct(
      fit$data$occasion_id
    ),
    
    n_user_contexts = n_distinct(
      interaction(
        fit$data$user_id,
        fit$data$context,
        drop = TRUE
      )
    ),
    
    n_users_repeated_private = n_distinct(
      fit$data$user_id[
        fit$data$context == "private" &
          fit$data$n_user_context_observations >= 2
      ]
    ),
    
    n_users_repeated_public = n_distinct(
      fit$data$user_id[
        fit$data$context == "public" &
          fit$data$n_user_context_observations >= 2
      ]
    ),
    
    singular = lme4::isSingular(
      fit$model,
      tol = 1e-4
    )
  )
  
  
  if (
    length(interaction_term) == 0 ||
    is.na(interaction_term)
  ) {
    
    return(
      empty_result(
        n_rows =
          fit_diagnostics$n_rows,
        
        n_users =
          fit_diagnostics$n_users,
        
        n_occasions =
          fit_diagnostics$n_occasions,
        
        n_user_contexts =
          fit_diagnostics$n_user_contexts,
        
        n_users_repeated_private =
          fit_diagnostics$n_users_repeated_private,
        
        n_users_repeated_public =
          fit_diagnostics$n_users_repeated_public,
        
        singular =
          fit_diagnostics$singular
      )
    )
  }
  
  
  estimate_i <- unname(
    b[interaction_term]
  )
  
  se_i <- sqrt(
    unname(
      V[
        interaction_term,
        interaction_term
      ]
    )
  )
  
  
  tibble(
    outcome = outcome,
    
    outcome_label = nice_outcome_label(
      outcome
    ),
    
    feature = feature,
    
    component = component,
    
    estimate = estimate_i,
    
    se = se_i,
    
    conf.low = estimate_i -
      1.96 *
      se_i,
    
    conf.high = estimate_i +
      1.96 *
      se_i,
    
    p.value = p_from_est_se(
      estimate_i,
      se_i
    ),
    
    n_rows =
      fit_diagnostics$n_rows,
    
    n_users =
      fit_diagnostics$n_users,
    
    n_occasions =
      fit_diagnostics$n_occasions,
    
    n_user_contexts =
      fit_diagnostics$n_user_contexts,
    
    n_users_repeated_private =
      fit_diagnostics$n_users_repeated_private,
    
    n_users_repeated_public =
      fit_diagnostics$n_users_repeated_public,
    
    singular =
      fit_diagnostics$singular,
    
    model_type =
      "lmer_within_between_CR2"
  )
}

############################
#### 10) RUNNER FUNCTIONS
############################

run_context_models <- function(
  data,
  outcomes,
  features,
  model_family,
  component = "within"
) {
  
  if (length(features) == 0) {
    
    return(
      tibble()
    )
  }
  
  
  bind_rows(
    lapply(
      outcomes,
      function(outcome_i) {
        
        bind_rows(
          lapply(
            features,
            function(feature_i) {
              
              if (
                model_family == "trait"
              ) {
                
                fit_i <- fit_trait_symbol_model(
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
                
                fit_i <- fit_state_symbol_model(
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
            }
          )
        )
      }
    )
  )
}


run_interaction_models <- function(
  data,
  outcomes,
  features,
  model_family,
  component = "within"
) {
  
  if (length(features) == 0) {
    
    return(
      tibble()
    )
  }
  
  
  bind_rows(
    lapply(
      outcomes,
      function(outcome_i) {
        
        bind_rows(
          lapply(
            features,
            function(feature_i) {
              
              if (
                model_family == "trait"
              ) {
                
                fit_i <- fit_trait_symbol_model(
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
                
                fit_i <- fit_state_symbol_model(
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
            }
          )
        )
      }
    )
  )
}

############################
#### 11) RUN CONTEXT-SPECIFIC MODELS
############################

symbol_trait_context <- run_context_models(
  data = keyboard_data_trait_symbol_z,
  outcomes = trait_outcomes,
  features = symbol_features_keep_trait,
  model_family = "trait"
)


# Primary daily results:
# within-person deviations from context-specific means.

symbol_daily_context <- run_context_models(
  data = keyboard_data_day_symbol,
  outcomes = daily_outcomes,
  features = symbol_features_keep_day,
  model_family = "state",
  component = "within"
)


# Primary momentary results:
# within-person deviations from context-specific means.

symbol_momentary_context <- run_context_models(
  data = keyboard_data_moment_symbol,
  outcomes = momentary_outcomes,
  features = symbol_features_keep_moment,
  model_family = "state",
  component = "within"
)


# Complementary between-person estimates.

symbol_daily_context_between <- run_context_models(
  data = keyboard_data_day_symbol,
  outcomes = daily_outcomes,
  features = symbol_features_keep_day,
  model_family = "state",
  component = "between"
)


symbol_momentary_context_between <- run_context_models(
  data = keyboard_data_moment_symbol,
  outcomes = momentary_outcomes,
  features = symbol_features_keep_moment,
  model_family = "state",
  component = "between"
)

############################
#### 12) COMBINE PRIMARY CONTEXT RESULTS
############################

symbol_context_results_all <- bind_rows(
  symbol_trait_context %>%
    mutate(
      timescale = "Trait",
      effect_level = "Between-person"
    ),
  
  symbol_daily_context %>%
    mutate(
      timescale = "Daily",
      effect_level = "Within-person"
    ),
  
  symbol_momentary_context %>%
    mutate(
      timescale = "Momentary",
      effect_level = "Within-person"
    )
) %>%
  left_join(
    symbol_lookup_trait,
    by = "feature"
  ) %>%
  left_join(
    symbol_prevalence_trait %>%
      select(
        feature,
        n_users_observed_prevalence =
          n_users_observed,
        n_users_nonzero_prevalence =
          n_users_nonzero,
        prop_users_nonzero_prevalence =
          prop_users_nonzero
      ),
    by = "feature"
  ) %>%
  mutate(
    context_label = nice_context(
      context
    ),
    outcome_label = nice_outcome_label(
      outcome
    )
  ) %>%
  group_by(
    timescale,
    outcome,
    context
  ) %>%
  mutate(
    p_fdr = p.adjust(
      p.value,
      method = "BH"
    )
  ) %>%
  ungroup() %>%
  arrange(
    timescale,
    outcome_label,
    context_label,
    feature
  )


write.csv(
  symbol_context_results_all,
  paste0(
    "results/",
    "repository_symbol_context_specific_",
    "regression_results_all_timescales.csv"
  ),
  row.names = FALSE
)

############################
#### 13) RUN INTERACTION MODELS
############################

symbol_trait_interactions <- run_interaction_models(
  data = keyboard_data_trait_symbol_z,
  outcomes = trait_outcomes,
  features = symbol_features_keep_trait,
  model_family = "trait"
)


symbol_daily_interactions <- run_interaction_models(
  data = keyboard_data_day_symbol,
  outcomes = daily_outcomes,
  features = symbol_features_keep_day,
  model_family = "state",
  component = "within"
)


symbol_momentary_interactions <- run_interaction_models(
  data = keyboard_data_moment_symbol,
  outcomes = momentary_outcomes,
  features = symbol_features_keep_moment,
  model_family = "state",
  component = "within"
)


symbol_daily_interactions_between <- run_interaction_models(
  data = keyboard_data_day_symbol,
  outcomes = daily_outcomes,
  features = symbol_features_keep_day,
  model_family = "state",
  component = "between"
)


symbol_momentary_interactions_between <- run_interaction_models(
  data = keyboard_data_moment_symbol,
  outcomes = momentary_outcomes,
  features = symbol_features_keep_moment,
  model_family = "state",
  component = "between"
)

############################
#### 14) COMBINE PRIMARY INTERACTIONS
############################

symbol_interaction_results_all <- bind_rows(
  symbol_trait_interactions %>%
    mutate(
      timescale = "Trait",
      effect_level = "Between-person"
    ),
  
  symbol_daily_interactions %>%
    mutate(
      timescale = "Daily",
      effect_level = "Within-person"
    ),
  
  symbol_momentary_interactions %>%
    mutate(
      timescale = "Momentary",
      effect_level = "Within-person"
    )
) %>%
  left_join(
    symbol_lookup_trait,
    by = "feature"
  ) %>%
  left_join(
    symbol_prevalence_trait %>%
      select(
        feature,
        n_users_observed_prevalence =
          n_users_observed,
        n_users_nonzero_prevalence =
          n_users_nonzero,
        prop_users_nonzero_prevalence =
          prop_users_nonzero
      ),
    by = "feature"
  ) %>%
  mutate(
    outcome_label = nice_outcome_label(
      outcome
    )
  ) %>%
  group_by(
    timescale,
    outcome
  ) %>%
  mutate(
    p_fdr = p.adjust(
      p.value,
      method = "BH"
    )
  ) %>%
  ungroup() %>%
  arrange(
    timescale,
    outcome_label,
    feature
  )


write.csv(
  symbol_interaction_results_all,
  paste0(
    "results/",
    "repository_symbol_context_interaction_",
    "regression_results_all_timescales.csv"
  ),
  row.names = FALSE
)

############################
#### 15) COMPLEMENTARY BETWEEN-PERSON
#### DAILY / MOMENTARY RESULTS
############################

symbol_between_context_results <- bind_rows(
  symbol_daily_context_between %>%
    mutate(
      timescale = "Daily",
      effect_level = "Between-person"
    ),
  
  symbol_momentary_context_between %>%
    mutate(
      timescale = "Momentary",
      effect_level = "Between-person"
    )
) %>%
  left_join(
    symbol_lookup_trait,
    by = "feature"
  ) %>%
  left_join(
    symbol_prevalence_trait %>%
      select(
        feature,
        n_users_observed_prevalence =
          n_users_observed,
        n_users_nonzero_prevalence =
          n_users_nonzero,
        prop_users_nonzero_prevalence =
          prop_users_nonzero
      ),
    by = "feature"
  ) %>%
  mutate(
    context_label = nice_context(
      context
    ),
    outcome_label = nice_outcome_label(
      outcome
    )
  ) %>%
  group_by(
    timescale,
    outcome,
    context
  ) %>%
  mutate(
    p_fdr = p.adjust(
      p.value,
      method = "BH"
    )
  ) %>%
  ungroup() %>%
  arrange(
    timescale,
    outcome_label,
    context_label,
    feature
  )


symbol_between_interaction_results <- bind_rows(
  symbol_daily_interactions_between %>%
    mutate(
      timescale = "Daily",
      effect_level = "Between-person"
    ),
  
  symbol_momentary_interactions_between %>%
    mutate(
      timescale = "Momentary",
      effect_level = "Between-person"
    )
) %>%
  left_join(
    symbol_lookup_trait,
    by = "feature"
  ) %>%
  left_join(
    symbol_prevalence_trait %>%
      select(
        feature,
        n_users_observed_prevalence =
          n_users_observed,
        n_users_nonzero_prevalence =
          n_users_nonzero,
        prop_users_nonzero_prevalence =
          prop_users_nonzero
      ),
    by = "feature"
  ) %>%
  mutate(
    outcome_label = nice_outcome_label(
      outcome
    )
  ) %>%
  group_by(
    timescale,
    outcome
  ) %>%
  mutate(
    p_fdr = p.adjust(
      p.value,
      method = "BH"
    )
  ) %>%
  ungroup() %>%
  arrange(
    timescale,
    outcome_label,
    feature
  )


write.csv(
  symbol_between_context_results,
  paste0(
    "results/",
    "repository_symbol_between_person_",
    "context_specific_results.csv"
  ),
  row.names = FALSE
)


write.csv(
  symbol_between_interaction_results,
  paste0(
    "results/",
    "repository_symbol_between_person_",
    "context_interaction_results.csv"
  ),
  row.names = FALSE
)

############################
#### 16) MODEL DIAGNOSTICS
############################

symbol_model_diagnostics <- bind_rows(
  symbol_daily_context %>%
    mutate(
      timescale = "Daily"
    ),
  
  symbol_momentary_context %>%
    mutate(
      timescale = "Momentary"
    )
) %>%
  distinct(
    timescale,
    outcome,
    feature,
    n_rows,
    n_users,
    n_occasions,
    n_user_contexts,
    n_users_repeated_private,
    n_users_repeated_public,
    singular,
    model_type
  ) %>%
  left_join(
    symbol_lookup_trait,
    by = "feature"
  ) %>%
  arrange(
    timescale,
    outcome,
    feature
  )


write.csv(
  symbol_model_diagnostics,
  "results/repository_symbol_model_diagnostics.csv",
  row.names = FALSE
)

############################
#### 17) TABLE S8
############################

# Rank retained symbols by the summed absolute standardized
# coefficient across:
#
# Private PA
# Private NA
# Public PA
# Public NA

table_s8_long <- symbol_context_results_all %>%
  filter(
    timescale == "Trait",
    outcome %in% c(
      "pa_trait_z",
      "na_trait_z"
    ),
    context %in% c(
      "private",
      "public"
    )
  ) %>%
  mutate(
    context_label = nice_context(
      context
    ),
    
    outcome_short = case_when(
      outcome == "pa_trait_z" ~ "PA",
      outcome == "na_trait_z" ~ "NA",
      TRUE                    ~ outcome
    ),
    
    column_name = paste(
      context_label,
      outcome_short,
      "beta [95% CI]"
    ),
    
    beta_ci = fmt_beta_ci(
      estimate,
      conf.low,
      conf.high,
      digits = 2
    )
  )


table_s8_ranked_features <- table_s8_long %>%
  group_by(
    feature,
    symbol,
    symbol_type
  ) %>%
  summarise(
    overall_abs_trait_affect_association = sum(
      abs(estimate),
      na.rm = TRUE
    ),
    
    n_available_coefficients = sum(
      !is.na(estimate)
    ),
    
    .groups = "drop"
  ) %>%
  filter(
    n_available_coefficients == 4
  ) %>%
  arrange(
    desc(
      overall_abs_trait_affect_association
    )
  ) %>%
  slice_head(
    n = 10
  )


table_s8 <- table_s8_long %>%
  semi_join(
    table_s8_ranked_features,
    by = c(
      "feature",
      "symbol",
      "symbol_type"
    )
  ) %>%
  select(
    feature,
    symbol,
    symbol_type,
    column_name,
    beta_ci
  ) %>%
  pivot_wider(
    names_from = column_name,
    values_from = beta_ci
  ) %>%
  left_join(
    table_s8_ranked_features %>%
      select(
        feature,
        overall_abs_trait_affect_association
      ),
    by = "feature"
  ) %>%
  arrange(
    desc(
      overall_abs_trait_affect_association
    )
  ) %>%
  select(
    Symbol = symbol,
    Type = symbol_type,
    `Private PA beta [95% CI]`,
    `Private NA beta [95% CI]`,
    `Public PA beta [95% CI]`,
    `Public NA beta [95% CI]`
  )


write.csv(
  table_s8,
  "results/table_s8_top_symbol_trait_affect_associations.csv",
  row.names = FALSE,
  na = ""
)

############################
#### 18) FINAL CHECKS
############################

stopifnot(
  all(
    symbol_context_results_all$model_type[
      symbol_context_results_all$timescale != "Trait" &
        !is.na(
          symbol_context_results_all$model_type
        )
    ] ==
      "lmer_within_between_CR2"
  ),
  
  all(
    symbol_interaction_results_all$model_type[
      symbol_interaction_results_all$timescale != "Trait" &
        !is.na(
          symbol_interaction_results_all$model_type
        )
    ] ==
      "lmer_within_between_CR2"
  )
)


print(
  symbol_model_diagnostics %>%
    count(
      timescale,
      singular
    )
)

############################
#### 19) PRINT OUTPUTS
############################

print(
  symbol_keep
)

print(
  symbol_context_results_all
)

print(
  symbol_interaction_results_all
)

print(
  symbol_between_context_results
)

print(
  symbol_between_interaction_results
)

print(
  symbol_model_diagnostics
)

print(
  table_s8
)

# finish