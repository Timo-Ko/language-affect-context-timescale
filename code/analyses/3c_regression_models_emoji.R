############################
#### EMOJI / EMOTICON × TRAIT AFFECT ANALYSES
#### TABLE S9 + FULL REPOSITORY CSVs
############################

############################
#### 0) PREPARATION
############################

required_packages <- c(
  "dplyr",
  "tidyr",
  "tibble",
  "stringr",
  "broom",
  "lmtest",
  "sandwich"
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
    x == "pa_trait_z" ~ "Trait PA",
    x == "na_trait_z" ~ "Trait NA",
    TRUE              ~ x
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
#### 3) LOAD TRAIT DATA
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


stopifnot(
  all(
    c(
      "user_id",
      "scope",
      "pa_panas",
      "na_panas"
    ) %in%
      names(keyboard_data_trait_raw)
  )
)


############################
#### 4) IDENTIFY SYMBOL FEATURES
############################

symbol_features_trait <- get_numeric_symbol_features(
  keyboard_data_trait_raw
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
#### 5) CONTEXT-SPECIFIC PARTICIPANT PREVALENCE
############################

# For context-specific trait regressions, retain symbols used by
# at least 10% of participants in BOTH private and public communication.
#
# Prevalence is calculated in the trait analysis sample used for the
# PA and NA regressions.

symbol_prevalence_source <- keyboard_data_trait %>%
  filter(
    !is.na(pa_panas),
    !is.na(na_panas)
  ) %>%
  select(
    user_id,
    context,
    all_of(
      symbol_features_trait
    )
  )


symbol_prevalence_trait <- symbol_prevalence_source %>%
  pivot_longer(
    cols = all_of(
      symbol_features_trait
    ),
    names_to = "feature",
    values_to = "value"
  ) %>%
  group_by(
    context,
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
    feature,
    context
  )


symbol_keep <- symbol_prevalence_trait %>%
  filter(
    !is.na(
      prop_users_nonzero
    )
  ) %>%
  group_by(
    feature,
    symbol,
    symbol_type
  ) %>%
  summarise(
    n_contexts = n_distinct(
      context
    ),
    
    private_prevalence = prop_users_nonzero[
      context == "private"
    ][1],
    
    public_prevalence = prop_users_nonzero[
      context == "public"
    ][1],
    
    private_n_users_nonzero = n_users_nonzero[
      context == "private"
    ][1],
    
    public_n_users_nonzero = n_users_nonzero[
      context == "public"
    ][1],
    
    .groups = "drop"
  ) %>%
  filter(
    n_contexts == 2,
    !is.na(private_prevalence),
    !is.na(public_prevalence),
    private_prevalence >= 0.10,
    public_prevalence >= 0.10
  ) %>%
  arrange(
    desc(
      pmin(
        private_prevalence,
        public_prevalence
      )
    )
  )


if (nrow(symbol_keep) == 0) {
  
  stop(
    paste0(
      "No emoji or emoticon features met the ",
      "10% prevalence threshold in both contexts."
    )
  )
}


symbol_features_keep_trait <- intersect(
  symbol_features_trait,
  symbol_keep$feature
)


message(
  "Symbols retained for context-specific trait regressions: ",
  length(
    symbol_features_keep_trait
  ),
  " (",
  sum(
    symbol_keep$symbol_type == "Emoji"
  ),
  " emoji; ",
  sum(
    symbol_keep$symbol_type == "Emoticon"
  ),
  " emoticon)"
)


write.csv(
  symbol_prevalence_trait,
  paste0(
    "results/",
    "repository_symbol_prevalence_trait_by_context.csv"
  ),
  row.names = FALSE
)


write.csv(
  symbol_keep,
  paste0(
    "results/",
    "repository_symbol_prevalence_trait_",
    "both_contexts_eligible.csv"
  ),
  row.names = FALSE
)

############################
#### 6) PREPARE TRAIT ANALYSIS DATA
############################

trait_outcome_data <- keyboard_data_trait %>%
  distinct(
    user_id,
    pa_panas,
    na_panas
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


trait_outcomes <- c(
  "pa_trait_z",
  "na_trait_z"
)


############################
#### 7) TRAIT MODEL HELPERS
############################

coef_by_context_lm <- function(
  model,
  term,
  vcov_mat,
  conf = 0.95
) {
  
  b <- coef(
    model
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
      vcov_mat[
        term,
        term
      ]
    )
  )
  
  
  if (
    !is.na(int_term) &&
    int_term %in%
    names(b) &&
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
    
    message(
      "Skipping insufficient model: ",
      outcome,
      " × ",
      feature
    )
    
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
  
  
  # Do not retain rank-deficient models.
  if (
    model_i$rank <
    length(
      coef(
        model_i
      )
    )
  ) {
    
    message(
      "Skipping rank-deficient trait model: ",
      outcome,
      " × ",
      feature
    )
    
    return(NULL)
  }
  
  
  vcov_i <- tryCatch(
    sandwich::vcovCL(
      model_i,
      cluster = dat$user_id,
      type = "HC1"
    ),
    error = function(e) {
      
      message(
        "Cluster-robust covariance failed for ",
        outcome,
        " × ",
        feature,
        ": ",
        conditionMessage(e)
      )
      
      NULL
    }
  )
  
  
  if (is.null(vcov_i)) {
    
    return(NULL)
  }
  
  
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
#### 8) RUN TRAIT MODELS
############################

symbol_trait_fits <- tidyr::expand_grid(
  outcome = trait_outcomes,
  feature = symbol_features_keep_trait
) %>%
  mutate(
    fit = Map(
      function(outcome_i, feature_i) {
        fit_trait_symbol_model(
          data = keyboard_data_trait_symbol_z,
          outcome = outcome_i,
          feature = feature_i
        )
      },
      outcome,
      feature
    )
  )


############################
#### 9) CONTEXT-SPECIFIC RESULTS
############################

symbol_trait_context <- bind_rows(
  Map(
    function(fit_i, outcome_i, feature_i) {
      
      extract_trait_context_results(
        fit = fit_i,
        outcome = outcome_i,
        feature = feature_i
      )
      
    },
    symbol_trait_fits$fit,
    symbol_trait_fits$outcome,
    symbol_trait_fits$feature
  )
)


symbol_trait_context_results <- symbol_trait_context %>%
  left_join(
    symbol_lookup_trait,
    by = "feature"
  ) %>%
  left_join(
    symbol_prevalence_trait %>%
      select(
        feature,
        context,
        n_users_observed_prevalence =
          n_users_observed,
        n_users_nonzero_prevalence =
          n_users_nonzero,
        prop_users_nonzero_prevalence =
          prop_users_nonzero
      ),
    by = c(
      "feature",
      "context"
    )
  ) %>%
  mutate(
    timescale = "Trait",
    effect_level = "Between-person",
    context_label = nice_context(
      context
    ),
    outcome_label = nice_outcome_label(
      outcome
    )
  ) %>%
  group_by(
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
    outcome_label,
    context_label,
    feature
  )


write.csv(
  symbol_trait_context_results,
  paste0(
    "results/",
    "repository_symbol_context_specific_",
    "regression_results_trait.csv"
  ),
  row.names = FALSE
)


############################
#### 10) CONTEXT INTERACTION RESULTS
############################

symbol_trait_interactions <- bind_rows(
  Map(
    function(fit_i, outcome_i, feature_i) {
      
      extract_trait_interaction_results(
        fit = fit_i,
        outcome = outcome_i,
        feature = feature_i
      )
      
    },
    symbol_trait_fits$fit,
    symbol_trait_fits$outcome,
    symbol_trait_fits$feature
  )
)


symbol_prevalence_trait_wide <- symbol_prevalence_trait %>%
  select(
    feature,
    context,
    n_users_observed,
    n_users_nonzero,
    prop_users_nonzero
  ) %>%
  mutate(
    context = as.character(
      context
    )
  ) %>%
  pivot_wider(
    names_from = context,
    values_from = c(
      n_users_observed,
      n_users_nonzero,
      prop_users_nonzero
    ),
    names_glue = "{context}_{.value}"
  )

symbol_trait_interaction_results <- symbol_trait_interactions %>%
  left_join(
    symbol_lookup_trait,
    by = "feature"
  ) %>%
  left_join(
    symbol_prevalence_trait_wide,
    by = "feature"
  ) %>%
  mutate(
    timescale = "Trait",
    effect_level = "Between-person",
    outcome_label = nice_outcome_label(
      outcome
    )
  ) %>%
  group_by(
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
    outcome_label,
    feature
  )


write.csv(
  symbol_trait_interaction_results,
  paste0(
    "results/",
    "repository_symbol_context_interaction_",
    "regression_results_trait.csv"
  ),
  row.names = FALSE
)


############################
#### 11) TABLE S9
############################

# Rank retained symbols by the summed absolute standardized
# coefficient across:
#
# Private PA
# Private NA
# Public PA
# Public NA

table_s9_long <- symbol_trait_context_results %>%
  filter(
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


table_s9_ranked_features <- table_s9_long %>%
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


table_s9 <- table_s9_long %>%
  semi_join(
    table_s9_ranked_features,
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
    table_s9_ranked_features %>%
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
  table_s9,
  "results/table_s9_top_symbol_trait_affect_associations.csv",
  row.names = FALSE,
  na = ""
)


############################
#### 12) MODEL COMPLETENESS CHECK
############################

symbol_model_completeness <- symbol_trait_fits %>%
  transmute(
    outcome,
    feature,
    model_available = !vapply(
      fit,
      is.null,
      logical(1)
    )
  ) %>%
  left_join(
    symbol_lookup_trait,
    by = "feature"
  )


write.csv(
  symbol_model_completeness,
  "results/repository_symbol_trait_model_completeness.csv",
  row.names = FALSE
)


message(
  "Trait symbol models retained: ",
  sum(
    symbol_model_completeness$model_available
  ),
  " / ",
  nrow(
    symbol_model_completeness
  )
)


stopifnot(
  all(
    table_s9_ranked_features$n_available_coefficients == 4
  )
)


############################
#### 13) PRINT OUTPUTS
############################

print(
  symbol_keep
)

print(
  symbol_model_completeness %>%
    count(
      outcome,
      model_available
    )
)

print(
  symbol_trait_context_results
)

print(
  symbol_trait_interaction_results
)

print(
  table_s9
)

# finish