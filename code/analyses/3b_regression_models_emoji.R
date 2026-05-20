############################
#### EMOJI × TRAIT ANALYSES
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
  "ggrepel",
  "broom",
  "lmtest",
  "sandwich",
  "scales",
  "stringr",
  "forcats"
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

safe_cor_p <- function(estimate, se) {
  ifelse(
    is.na(estimate) | is.na(se) | se <= 0,
    NA_real_,
    2 * pnorm(abs(estimate / se), lower.tail = FALSE)
  )
}

to_symbol_from_emoji_feature <- function(feature_name) {
  code <- stringr::str_match(feature_name, "^emoji_([0-9]+)_session_mean$")[, 2]
  if (is.na(code)) return(feature_name)
  
  out <- tryCatch(
    intToUtf8(as.integer(code)),
    error = function(e) feature_name
  )
  
  out
}

nice_outcome_label <- function(x) {
  dplyr::case_when(
    x == "affective_balance_z" ~ "Trait affective balance",
    x == "pa_trait_z"          ~ "Trait PA",
    x == "na_trait_z"          ~ "Trait NA",
    TRUE                       ~ x
  )
}

############################
#### 1) LOAD TRAIT DATA
############################

keyboard_data_trait <- readRDS("data/results/keyboard_data_trait_final.rds") %>%
  as.data.frame() %>%
  filter(scope %in% c("private", "public")) %>%
  mutate(
    context = factor(scope, levels = c("private", "public"))
  )

if (!all(c("user_id", "pa_panas", "na_panas", "scope") %in% names(keyboard_data_trait))) {
  stop("Expected columns missing: user_id, pa_panas, na_panas, scope.")
}

############################
#### 2) IDENTIFY INDIVIDUAL EMOJI FEATURES
############################

emoji_features <- names(keyboard_data_trait) %>%
  stringr::str_subset("^emoji_[0-9]+_session_mean$")

if (length(emoji_features) == 0) {
  stop("No individual emoji session-mean features found. Expected columns like emoji_128512_session_mean.")
}

message("Number of individual emoji features found: ", length(emoji_features))

emoji_lookup <- tibble(
  feature = emoji_features,
  symbol = vapply(emoji_features, to_symbol_from_emoji_feature, character(1))
)

write.csv(
  emoji_lookup,
  "results/emoji_feature_lookup.csv",
  row.names = FALSE
)

############################
#### 3) CREATE PARTICIPANT-LEVEL TRAIT OUTCOMES
############################

# Important:
# PA and NA are standardized at the participant level before joining back to
# private/public rows. This avoids double-weighting participants with both contexts.

trait_outcomes <- keyboard_data_trait %>%
  distinct(user_id, pa_panas, na_panas) %>%
  filter(!is.na(pa_panas), !is.na(na_panas)) %>%
  mutate(
    pa_trait_z = z_scale(pa_panas),
    na_trait_z = z_scale(na_panas),
    affective_balance_raw = pa_trait_z - na_trait_z,
    affective_balance_z = z_scale(affective_balance_raw)
  ) %>%
  select(
    user_id,
    pa_panas,
    na_panas,
    pa_trait_z,
    na_trait_z,
    affective_balance_z
  )

keyboard_data_trait_emoji <- keyboard_data_trait %>%
  select(
    user_id,
    scope,
    context,
    all_of(emoji_features)
  ) %>%
  left_join(trait_outcomes, by = "user_id") %>%
  mutate(
    across(
      all_of(emoji_features),
      z_scale,
      .names = "{.col}_z"
    )
  )

############################
#### 4) EMOJI PREVALENCE DIAGNOSTICS
############################

# Assumption:
# For individual emoji session-mean features, values > 0 indicate use of that emoji.
# If your feature encoding differs, adapt the nonzero definition here.

emoji_prevalence_context <- keyboard_data_trait_emoji %>%
  select(user_id, context, all_of(emoji_features)) %>%
  pivot_longer(
    cols = all_of(emoji_features),
    names_to = "feature",
    values_to = "value"
  ) %>%
  group_by(feature, context) %>%
  summarise(
    n_rows = n(),
    n_users = n_distinct(user_id),
    n_users_nonzero = n_distinct(user_id[!is.na(value) & value > 0]),
    prop_users_nonzero = n_users_nonzero / n_users,
    mean_raw = mean(value, na.rm = TRUE),
    sd_raw = sd(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(emoji_lookup, by = "feature") %>%
  arrange(desc(prop_users_nonzero), feature, context)

emoji_prevalence_overall <- keyboard_data_trait_emoji %>%
  select(user_id, all_of(emoji_features)) %>%
  pivot_longer(
    cols = all_of(emoji_features),
    names_to = "feature",
    values_to = "value"
  ) %>%
  group_by(feature) %>%
  summarise(
    n_users = n_distinct(user_id),
    n_users_nonzero = n_distinct(user_id[!is.na(value) & value > 0]),
    prop_users_nonzero = n_users_nonzero / n_users,
    mean_raw = mean(value, na.rm = TRUE),
    sd_raw = sd(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(emoji_lookup, by = "feature") %>%
  arrange(desc(prop_users_nonzero), feature)

write.csv(
  emoji_prevalence_context,
  "results/supp_emoji_prevalence_by_context.csv",
  row.names = FALSE
)

write.csv(
  emoji_prevalence_overall,
  "results/supp_emoji_prevalence_overall.csv",
  row.names = FALSE
)

############################
#### 5) FILTER EMOJIS FOR ANALYSES
############################

# Main criterion:
# Use emojis used by at least 10% of participants in at least one context.
# This matches the intended "common emoji" strategy while preserving context comparison.
#
# Additional diagnostics retain context-specific n_users_nonzero so unstable estimates
# can be inspected in the supplement.

emoji_keep <- emoji_prevalence_context %>%
  group_by(feature, symbol) %>%
  summarise(
    max_prop_users_nonzero = max(prop_users_nonzero, na.rm = TRUE),
    min_n_users_nonzero = min(n_users_nonzero, na.rm = TRUE),
    max_n_users_nonzero = max(n_users_nonzero, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(max_prop_users_nonzero >= 0.10) %>%
  arrange(desc(max_prop_users_nonzero))

emoji_features_keep <- emoji_keep$feature

message("Number of emoji features retained for analysis: ", length(emoji_features_keep))

if (length(emoji_features_keep) == 0) {
  stop("No emojis met the prevalence threshold.")
}

write.csv(
  emoji_keep,
  "results/emoji_features_retained_for_analysis.csv",
  row.names = FALSE
)

############################
#### 6) MODEL HELPERS
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
  
  est_priv <- unname(b[term])
  se_priv  <- sqrt(unname(vcov_mat[term, term]))
  
  if (!is.na(int_term) && int_term %in% rownames(vcov_mat)) {
    est_pub <- unname(b[term] + b[int_term])
    
    var_pub <- unname(
      vcov_mat[term, term] +
        vcov_mat[int_term, int_term] +
        2 * vcov_mat[term, int_term]
    )
    
    se_pub <- sqrt(pmax(var_pub, 0))
  } else {
    est_pub <- NA_real_
    se_pub <- NA_real_
  }
  
  tibble(
    context = c("private", "public"),
    estimate = c(est_priv, est_pub),
    se = c(se_priv, se_pub),
    conf.low = c(est_priv - z * se_priv, est_pub - z * se_pub),
    conf.high = c(est_priv + z * se_priv, est_pub + z * se_pub),
    p.value = safe_cor_p(
      estimate = c(est_priv, est_pub),
      se = c(se_priv, se_pub)
    )
  )
}

fit_context_emoji_model <- function(data, outcome, feature) {
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

extract_context_model_results <- function(fit, outcome, feature) {
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

extract_interaction_result <- function(fit, outcome, feature) {
  if (is.null(fit)) {
    return(tibble(
      outcome = outcome,
      outcome_label = nice_outcome_label(outcome),
      feature = feature,
      term = "emoji_z:contextpublic",
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
      term = "emoji_z:contextpublic",
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
      term = "emoji_z:contextpublic",
      estimate = estimate,
      se = std.error,
      conf.low = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error,
      p.value = p.value,
      n_rows = nrow(fit$data),
      n_users = n_distinct(fit$data$user_id)
    )
}

fit_pooled_emoji_model <- function(data, outcome, feature) {
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
    is.na(safe_sd(dat[[outcome]])) ||
    is.na(safe_sd(dat[[feat_z]])) ||
    safe_sd(dat[[outcome]]) == 0 ||
    safe_sd(dat[[feat_z]]) == 0
  ) {
    return(NULL)
  }
  
  # Descriptive pooled association adjusted for context.
  # This is not the primary analysis; it is supplementary.
  fml <- as.formula(paste0(outcome, " ~ ", feat_z, " + context"))
  mod <- lm(fml, data = dat)
  
  V <- tryCatch(
    sandwich::vcovCL(mod, cluster = dat$user_id, type = "HC1"),
    error = function(e) sandwich::vcovHC(mod, type = "HC3")
  )
  
  ct <- lmtest::coeftest(mod, vcov. = V)
  td <- broom::tidy(ct)
  
  td %>%
    filter(term == feat_z) %>%
    transmute(
      outcome = outcome,
      outcome_label = nice_outcome_label(outcome),
      feature = feature,
      estimate = estimate,
      se = std.error,
      conf.low = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error,
      p.value = p.value,
      n_rows = nrow(dat),
      n_users = n_distinct(dat$user_id)
    )
}

############################
#### 7) FIT EMOJI MODELS
############################

emoji_outcomes <- c(
  "affective_balance_z",
  "pa_trait_z",
  "na_trait_z"
)

emoji_context_results <- bind_rows(
  lapply(emoji_outcomes, function(outcome_i) {
    bind_rows(lapply(emoji_features_keep, function(feature_i) {
      fit_i <- fit_context_emoji_model(
        data = keyboard_data_trait_emoji,
        outcome = outcome_i,
        feature = feature_i
      )
      
      extract_context_model_results(
        fit = fit_i,
        outcome = outcome_i,
        feature = feature_i
      )
    }))
  })
) %>%
  left_join(emoji_lookup, by = "feature") %>%
  left_join(
    emoji_prevalence_context %>%
      select(
        feature,
        context,
        n_users_context = n_users,
        n_users_nonzero_context = n_users_nonzero,
        prop_users_nonzero_context = prop_users_nonzero
      ),
    by = c("feature", "context")
  ) %>%
  group_by(outcome, context) %>%
  mutate(p_fdr = p.adjust(p.value, method = "BH")) %>%
  ungroup() %>%
  arrange(outcome_label, context, p.value)

emoji_interaction_results <- bind_rows(
  lapply(emoji_outcomes, function(outcome_i) {
    bind_rows(lapply(emoji_features_keep, function(feature_i) {
      fit_i <- fit_context_emoji_model(
        data = keyboard_data_trait_emoji,
        outcome = outcome_i,
        feature = feature_i
      )
      
      extract_interaction_result(
        fit = fit_i,
        outcome = outcome_i,
        feature = feature_i
      )
    }))
  })
) %>%
  left_join(emoji_lookup, by = "feature") %>%
  group_by(outcome) %>%
  mutate(p_fdr = p.adjust(p.value, method = "BH")) %>%
  ungroup() %>%
  arrange(outcome_label, p.value)

emoji_pooled_results <- bind_rows(
  lapply(emoji_outcomes, function(outcome_i) {
    bind_rows(lapply(emoji_features_keep, function(feature_i) {
      fit_pooled_emoji_model(
        data = keyboard_data_trait_emoji,
        outcome = outcome_i,
        feature = feature_i
      )
    }))
  })
) %>%
  left_join(emoji_lookup, by = "feature") %>%
  left_join(
    emoji_prevalence_overall %>%
      select(
        feature,
        n_users_overall = n_users,
        n_users_nonzero_overall = n_users_nonzero,
        prop_users_nonzero_overall = prop_users_nonzero
      ),
    by = "feature"
  ) %>%
  group_by(outcome) %>%
  mutate(p_fdr = p.adjust(p.value, method = "BH")) %>%
  ungroup() %>%
  arrange(outcome_label, p.value)

write.csv(
  emoji_context_results,
  "results/supp_emoji_trait_context_specific_associations.csv",
  row.names = FALSE
)

write.csv(
  emoji_interaction_results,
  "results/supp_emoji_trait_context_interactions.csv",
  row.names = FALSE
)

write.csv(
  emoji_pooled_results,
  "results/supp_emoji_trait_pooled_context_adjusted_associations.csv",
  row.names = FALSE
)

############################
#### 8) MAIN-PAPER FIGURE:
#### INDIVIDUAL EMOJIS × TRAIT AFFECTIVE BALANCE
############################

emoji_balance_complete <- emoji_context_results %>%
  filter(
    outcome == "affective_balance_z",
    !is.na(estimate),
    context %in% c("private", "public")
  ) %>%
  select(
    feature,
    symbol,
    context,
    estimate,
    conf.low,
    conf.high,
    p.value,
    p_fdr,
    n_users,
    n_users_nonzero_context,
    prop_users_nonzero_context
  ) %>%
  tidyr::pivot_wider(
    names_from = context,
    values_from = c(
      estimate,
      conf.low,
      conf.high,
      p.value,
      p_fdr,
      n_users,
      n_users_nonzero_context,
      prop_users_nonzero_context
    ),
    names_sep = "_"
  ) %>%
  filter(!is.na(estimate_private), !is.na(estimate_public)) %>%
  mutate(
    rank_score = pmax(abs(estimate_private), abs(estimate_public), na.rm = TRUE),
    mean_abs_estimate = rowMeans(
      cbind(abs(estimate_private), abs(estimate_public)),
      na.rm = TRUE
    )
  )

top_n_main <- 10

emoji_balance_top <- emoji_balance_complete %>%
  slice_max(order_by = rank_score, n = top_n_main, with_ties = FALSE) %>%
  arrange(rank_score)

write.csv(
  emoji_balance_complete,
  "results/figure_emoji_affective_balance_private_public_all_emojis.csv",
  row.names = FALSE
)

write.csv(
  emoji_balance_top,
  "results/figure_emoji_affective_balance_private_public_top_emojis.csv",
  row.names = FALSE
)

axis_lim_main <- max(
  abs(c(
    emoji_balance_top$estimate_private,
    emoji_balance_top$estimate_public
  )),
  na.rm = TRUE
)

axis_lim_main <- max(0.30, ceiling(axis_lim_main * 10) / 10)
axis_lim_main <- min(axis_lim_main, 0.60)

fig_emoji_balance <- ggplot(
  emoji_balance_top,
  aes(
    x = estimate_private,
    y = estimate_public,
    label = symbol
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray70",
    linewidth = 0.4
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "gray70",
    linewidth = 0.4
  ) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dotted",
    color = "gray60",
    linewidth = 0.5
  ) +
  geom_point(
    size = 2.8,
    alpha = 0.9,
    color = "black"
  ) +
  ggrepel::geom_text_repel(
    size = 6.0,
    box.padding = 0.35,
    point.padding = 0.25,
    min.segment.length = 0,
    segment.color = "gray70",
    segment.alpha = 0.85,
    color = "black",
    show.legend = FALSE,
    seed = 123,
    max.overlaps = Inf
  ) +
  coord_equal() +
  scale_x_continuous(
    limits = c(-axis_lim_main, axis_lim_main),
    breaks = seq(-axis_lim_main, axis_lim_main, by = 0.1),
    labels = scales::label_number(accuracy = 0.1, trim = TRUE)
  ) +
  scale_y_continuous(
    limits = c(-axis_lim_main, axis_lim_main),
    breaks = seq(-axis_lim_main, axis_lim_main, by = 0.1),
    labels = scales::label_number(accuracy = 0.1, trim = TRUE)
  ) +
  labs(
    x = "Private communication\nstandardized association with affective balance (\u03b2)",
    y = "Public communication\nstandardized association with affective balance (\u03b2)"
  ) +
  base_theme +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 10),
    plot.margin = margin(10, 16, 8, 8)
  )

fig_emoji_balance

ggsave(
  filename = "figures/figure_emoji_affective_balance_private_public_scatter.png",
  plot = fig_emoji_balance,
  width = 6.6,
  height = 6.2,
  dpi = 300
)

ggsave(
  filename = "figures/figure_emoji_affective_balance_private_public_scatter.pdf",
  plot = fig_emoji_balance,
  width = 6.6,
  height = 6.2
)

############################
#### 9) SUPPLEMENTARY FIGURES:
#### SEPARATE PA AND NA EMOJI SCATTERS
############################

make_supp_emoji_scatter <- function(results, outcome_keep, top_n = 10) {
  dat_complete <- results %>%
    filter(
      outcome == outcome_keep,
      !is.na(estimate),
      context %in% c("private", "public")
    ) %>%
    select(
      feature,
      symbol,
      context,
      estimate,
      conf.low,
      conf.high,
      p.value,
      p_fdr
    ) %>%
    tidyr::pivot_wider(
      names_from = context,
      values_from = c(estimate, conf.low, conf.high, p.value, p_fdr),
      names_sep = "_"
    ) %>%
    filter(!is.na(estimate_private), !is.na(estimate_public)) %>%
    mutate(
      rank_score = pmax(abs(estimate_private), abs(estimate_public), na.rm = TRUE),
      mean_abs_estimate = rowMeans(
        cbind(abs(estimate_private), abs(estimate_public)),
        na.rm = TRUE
      )
    )
  
  dat_top <- dat_complete %>%
    slice_max(order_by = rank_score, n = top_n, with_ties = FALSE)
  
  axis_lim <- max(
    abs(c(dat_top$estimate_private, dat_top$estimate_public)),
    na.rm = TRUE
  )
  
  axis_lim <- max(0.30, ceiling(axis_lim * 10) / 10)
  axis_lim <- min(axis_lim, 0.60)
  
  p <- ggplot(
    dat_top,
    aes(
      x = estimate_private,
      y = estimate_public,
      label = symbol
    )
  ) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      color = "gray70",
      linewidth = 0.4
    ) +
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      color = "gray70",
      linewidth = 0.4
    ) +
    geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dotted",
      color = "gray60",
      linewidth = 0.5
    ) +
    geom_point(
      size = 2.8,
      alpha = 0.9,
      color = "black"
    ) +
    ggrepel::geom_text_repel(
      size = 6.0,
      box.padding = 0.35,
      point.padding = 0.25,
      min.segment.length = 0,
      segment.color = "gray70",
      segment.alpha = 0.85,
      color = "black",
      show.legend = FALSE,
      seed = 123,
      max.overlaps = Inf
    ) +
    coord_equal() +
    scale_x_continuous(
      limits = c(-axis_lim, axis_lim),
      breaks = seq(-axis_lim, axis_lim, by = 0.1),
      labels = scales::label_number(accuracy = 0.1, trim = TRUE)
    ) +
    scale_y_continuous(
      limits = c(-axis_lim, axis_lim),
      breaks = seq(-axis_lim, axis_lim, by = 0.1),
      labels = scales::label_number(accuracy = 0.1, trim = TRUE)
    ) +
    labs(
      x = paste0("Private communication\nstandardized association with ", nice_outcome_label(outcome_keep), " (\u03b2)"),
      y = paste0("Public communication\nstandardized association with ", nice_outcome_label(outcome_keep), " (\u03b2)"),
      title = nice_outcome_label(outcome_keep)
    ) +
    base_theme +
    theme(
      legend.position = "none",
      axis.text = element_text(size = 10),
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.margin = margin(10, 16, 8, 8)
    )
  
  list(
    plot = p,
    all_data = dat_complete,
    top_data = dat_top
  )
}

supp_pa <- make_supp_emoji_scatter(
  results = emoji_context_results,
  outcome_keep = "pa_trait_z",
  top_n = 10
)

supp_na <- make_supp_emoji_scatter(
  results = emoji_context_results,
  outcome_keep = "na_trait_z",
  top_n = 10
)

supp_pa$plot
supp_na$plot

ggsave(
  filename = "figures/supp_emoji_trait_pa_private_public_scatter.png",
  plot = supp_pa$plot,
  width = 6.6,
  height = 6.2,
  dpi = 300
)

ggsave(
  filename = "figures/supp_emoji_trait_na_private_public_scatter.png",
  plot = supp_na$plot,
  width = 6.6,
  height = 6.2,
  dpi = 300
)

ggsave(
  filename = "figures/supp_emoji_trait_pa_private_public_scatter.pdf",
  plot = supp_pa$plot,
  width = 6.6,
  height = 6.2
)

ggsave(
  filename = "figures/supp_emoji_trait_na_private_public_scatter.pdf",
  plot = supp_na$plot,
  width = 6.6,
  height = 6.2
)

write.csv(
  supp_pa$all_data,
  "results/supp_emoji_trait_pa_private_public_all_emojis.csv",
  row.names = FALSE
)

write.csv(
  supp_pa$top_data,
  "results/supp_emoji_trait_pa_private_public_top_emojis.csv",
  row.names = FALSE
)

write.csv(
  supp_na$all_data,
  "results/supp_emoji_trait_na_private_public_all_emojis.csv",
  row.names = FALSE
)

write.csv(
  supp_na$top_data,
  "results/supp_emoji_trait_na_private_public_top_emojis.csv",
  row.names = FALSE
)

############################
#### 10) OPTIONAL COMPACT TABLES FOR MANUSCRIPT/SUPPLEMENT
############################

main_emoji_table <- emoji_balance_top %>%
  transmute(
    emoji = symbol,
    feature = feature,
    beta_private = round(estimate_private, 3),
    beta_public = round(estimate_public, 3),
    p_private = signif(p.value_private, 3),
    p_public = signif(p.value_public, 3),
    p_fdr_private = signif(p_fdr_private, 3),
    p_fdr_public = signif(p_fdr_public, 3),
    n_users_nonzero_private = n_users_nonzero_context_private,
    n_users_nonzero_public = n_users_nonzero_context_public,
    prop_users_nonzero_private = round(prop_users_nonzero_context_private, 3),
    prop_users_nonzero_public = round(prop_users_nonzero_context_public, 3)
  ) %>%
  arrange(desc(pmax(abs(beta_private), abs(beta_public))))

write.csv(
  main_emoji_table,
  "results/main_emoji_affective_balance_top10_table.csv",
  row.names = FALSE
)

supp_context_table_rounded <- emoji_context_results %>%
  transmute(
    outcome = outcome_label,
    emoji = symbol,
    feature = feature,
    context = context,
    beta = round(estimate, 3),
    se = round(se, 3),
    ci_low = round(conf.low, 3),
    ci_high = round(conf.high, 3),
    p = signif(p.value, 3),
    p_fdr = signif(p_fdr, 3),
    n_rows = n_rows,
    n_users = n_users,
    n_users_nonzero_context = n_users_nonzero_context,
    prop_users_nonzero_context = round(prop_users_nonzero_context, 3)
  ) %>%
  arrange(outcome, context, p)

supp_interaction_table_rounded <- emoji_interaction_results %>%
  transmute(
    outcome = outcome_label,
    emoji = symbol,
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
  arrange(outcome, p)

supp_pooled_table_rounded <- emoji_pooled_results %>%
  transmute(
    outcome = outcome_label,
    emoji = symbol,
    feature = feature,
    beta_pooled_context_adjusted = round(estimate, 3),
    se = round(se, 3),
    ci_low = round(conf.low, 3),
    ci_high = round(conf.high, 3),
    p = signif(p.value, 3),
    p_fdr = signif(p_fdr, 3),
    n_rows = n_rows,
    n_users = n_users,
    n_users_nonzero_overall = n_users_nonzero_overall,
    prop_users_nonzero_overall = round(prop_users_nonzero_overall, 3)
  ) %>%
  arrange(outcome, p)

write.csv(
  supp_context_table_rounded,
  "results/supp_emoji_context_specific_associations_rounded.csv",
  row.names = FALSE
)

write.csv(
  supp_interaction_table_rounded,
  "results/supp_emoji_context_interactions_rounded.csv",
  row.names = FALSE
)

write.csv(
  supp_pooled_table_rounded,
  "results/supp_emoji_pooled_context_adjusted_associations_rounded.csv",
  row.names = FALSE
)

############################
#### 11) SUPPLEMENT:
#### DAILY-LEVEL AGGREGATE EMOJI ANALYSES
############################

library(lme4)
library(broom.mixed)

keyboard_data_day <- readRDS("data/results/keyboard_data_day_final.rds") %>%
  as.data.frame() %>%
  filter(scope %in% c("private", "public")) %>%
  mutate(
    context = factor(scope, levels = c("private", "public")),
    date = as.Date(date),
    user_day_id = interaction(user_id, date, drop = TRUE),
    daily_valence_z = z_scale(daily_valence)
  )

if (!all(c("user_id", "scope", "daily_valence") %in% names(keyboard_data_day))) {
  stop("Expected daily columns missing: user_id, scope, daily_valence.")
}

############################
#### 11.1) IDENTIFY AGGREGATE EMOJI FEATURES
############################

# Adjust this list if your aggregate emoji variable names differ.
# The script keeps only variables that actually exist in the daily dataset.

candidate_daily_emoji_features <- c(
  "emoji_count",
  "emoji_n",
  "emoji_total",
  "emoji_per_word",
  "emoji_count_per_word",
  "emoji_ratio",
  "emoji_sentiment_mean",
  "emoji_sentiment_sd",
  "emoji_sentiment_min",
  "emoji_sentiment_max",
  "emoji_unique",
  "emoji_unique_n",
  "emoji_diversity",
  "emoticon_count",
  "emoticon_per_word",
  "emoticon_count_per_word"
)

daily_emoji_features <- intersect(candidate_daily_emoji_features, names(keyboard_data_day))

# Fallback: catch plausible aggregate emoji/emoticon variables while excluding
# individual emoji symbol features such as emoji_128512_session_mean.
if (length(daily_emoji_features) == 0) {
  daily_emoji_features <- names(keyboard_data_day) %>%
    stringr::str_subset("emoji|emoticon") %>%
    .[!stringr::str_detect(., "^emoji_[0-9]+")] %>%
    .[!stringr::str_detect(., "_session_")] %>%
    .[sapply(keyboard_data_day[.], is.numeric)]
}

if (length(daily_emoji_features) == 0) {
  stop("No aggregate daily emoji features found. Check daily emoji feature names.")
}

message("Aggregate daily emoji features used: ")
print(daily_emoji_features)

daily_emoji_labels <- c(
  "emoji_count" = "Emoji count",
  "emoji_n" = "Emoji count",
  "emoji_total" = "Emoji count",
  "emoji_per_word" = "Emoji per word",
  "emoji_count_per_word" = "Emoji per word",
  "emoji_ratio" = "Emoji per word",
  "emoji_sentiment_mean" = "Mean emoji sentiment",
  "emoji_sentiment_sd" = "Emoji sentiment variability",
  "emoji_sentiment_min" = "Minimum emoji sentiment",
  "emoji_sentiment_max" = "Maximum emoji sentiment",
  "emoji_unique" = "Unique emojis",
  "emoji_unique_n" = "Unique emojis",
  "emoji_diversity" = "Emoji diversity",
  "emoticon_count" = "Emoticon count",
  "emoticon_per_word" = "Emoticon per word",
  "emoticon_count_per_word" = "Emoticon per word"
)

daily_emoji_feature_lookup <- tibble(
  feature = daily_emoji_features,
  label = ifelse(
    daily_emoji_features %in% names(daily_emoji_labels),
    unname(daily_emoji_labels[daily_emoji_features]),
    daily_emoji_features
  )
)

write.csv(
  daily_emoji_feature_lookup,
  "results/supp_daily_aggregate_emoji_feature_lookup.csv",
  row.names = FALSE
)

############################
#### 11.2) PREPARE DAILY DATA
############################

keyboard_data_day_emoji_z <- keyboard_data_day %>%
  mutate(
    across(
      all_of(daily_emoji_features),
      z_scale,
      .names = "{.col}_z"
    )
  )

daily_emoji_descriptives <- keyboard_data_day %>%
  select(user_id, context, all_of(daily_emoji_features)) %>%
  pivot_longer(
    cols = all_of(daily_emoji_features),
    names_to = "feature",
    values_to = "value"
  ) %>%
  group_by(feature, context) %>%
  summarise(
    n_rows = n(),
    n_users = n_distinct(user_id),
    n_nonmissing = sum(!is.na(value)),
    n_nonzero = sum(!is.na(value) & value > 0),
    prop_nonzero = n_nonzero / n_rows,
    mean_raw = mean(value, na.rm = TRUE),
    sd_raw = sd(value, na.rm = TRUE),
    median_raw = median(value, na.rm = TRUE),
    q25_raw = quantile(value, 0.25, na.rm = TRUE),
    q75_raw = quantile(value, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(daily_emoji_feature_lookup, by = "feature") %>%
  arrange(feature, context)

write.csv(
  daily_emoji_descriptives,
  "results/supp_daily_aggregate_emoji_descriptives_by_context.csv",
  row.names = FALSE
)

############################
#### 11.3) MODEL HELPERS
############################

fit_daily_aggregate_emoji_model <- function(data, feature, outcome = "daily_valence_z") {
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
    dplyr::n_distinct(dat$user_id) < 10 ||
    dplyr::n_distinct(dat$context) < 2 ||
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
      lmer(
        formula = fml,
        data = dat,
        REML = FALSE,
        control = lmerControl(
          optimizer = "bobyqa",
          optCtrl = list(maxfun = 2e5)
        )
      )
    ),
    error = function(e) NULL
  )
  
  if (is.null(mod)) {
    return(NULL)
  }
  
  list(
    model = mod,
    data = dat,
    feature_z = feat_z
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
    p.value = safe_cor_p(
      estimate = c(est_private, est_public),
      se = c(se_private, se_public)
    )
  )
}

extract_daily_aggregate_emoji_context_results <- function(fit, feature) {
  if (is.null(fit)) {
    return(tibble(
      outcome = "Daily valence",
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
      outcome = "Daily valence",
      feature = feature,
      n_rows = nrow(fit$data),
      n_users = dplyr::n_distinct(fit$data$user_id),
      singular = lme4::isSingular(fit$model, tol = 1e-4),
      .before = 1
    )
}

extract_daily_aggregate_emoji_interaction <- function(fit, feature) {
  if (is.null(fit)) {
    return(tibble(
      outcome = "Daily valence",
      feature = feature,
      term = "emoji_z:contextpublic",
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
      outcome = "Daily valence",
      feature = feature,
      term = "emoji_z:contextpublic",
      estimate = NA_real_,
      se = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      p.value = NA_real_,
      n_rows = nrow(fit$data),
      n_users = dplyr::n_distinct(fit$data$user_id),
      singular = lme4::isSingular(fit$model, tol = 1e-4)
    ))
  }
  
  out %>%
    transmute(
      outcome = "Daily valence",
      feature = feature,
      term = "emoji_z:contextpublic",
      estimate = estimate,
      se = std.error,
      conf.low = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error,
      p.value = 2 * pnorm(abs(estimate / std.error), lower.tail = FALSE),
      n_rows = nrow(fit$data),
      n_users = dplyr::n_distinct(fit$data$user_id),
      singular = lme4::isSingular(fit$model, tol = 1e-4)
    )
}

############################
#### 11.4) FIT DAILY MODELS
############################

daily_aggregate_emoji_models <- setNames(
  lapply(daily_emoji_features, function(feature_i) {
    fit_daily_aggregate_emoji_model(
      data = keyboard_data_day_emoji_z,
      feature = feature_i,
      outcome = "daily_valence_z"
    )
  }),
  daily_emoji_features
)

daily_aggregate_emoji_context_results <- bind_rows(
  lapply(daily_emoji_features, function(feature_i) {
    extract_daily_aggregate_emoji_context_results(
      fit = daily_aggregate_emoji_models[[feature_i]],
      feature = feature_i
    )
  })
) %>%
  left_join(daily_emoji_feature_lookup, by = "feature") %>%
  left_join(
    daily_emoji_descriptives %>%
      select(
        feature,
        context,
        n_rows_context = n_rows,
        n_users_context = n_users,
        prop_nonzero_context = prop_nonzero,
        mean_raw_context = mean_raw,
        sd_raw_context = sd_raw
      ),
    by = c("feature", "context")
  ) %>%
  group_by(context) %>%
  mutate(p_fdr = p.adjust(p.value, method = "BH")) %>%
  ungroup() %>%
  arrange(context, p.value)

daily_aggregate_emoji_interaction_results <- bind_rows(
  lapply(daily_emoji_features, function(feature_i) {
    extract_daily_aggregate_emoji_interaction(
      fit = daily_aggregate_emoji_models[[feature_i]],
      feature = feature_i
    )
  })
) %>%
  left_join(daily_emoji_feature_lookup, by = "feature") %>%
  mutate(p_fdr = p.adjust(p.value, method = "BH")) %>%
  arrange(p.value)

write.csv(
  daily_aggregate_emoji_context_results,
  "results/supp_daily_aggregate_emoji_context_specific_associations.csv",
  row.names = FALSE
)

write.csv(
  daily_aggregate_emoji_interaction_results,
  "results/supp_daily_aggregate_emoji_context_interactions.csv",
  row.names = FALSE
)

############################
#### 11.5) ROUNDED SUPPLEMENT TABLES
############################

supp_daily_aggregate_emoji_context_table_rounded <- daily_aggregate_emoji_context_results %>%
  transmute(
    outcome = outcome,
    emoji_feature = label,
    feature = feature,
    context = context,
    beta = round(estimate, 3),
    se = round(se, 3),
    ci_low = round(conf.low, 3),
    ci_high = round(conf.high, 3),
    p = signif(p.value, 3),
    p_fdr = signif(p_fdr, 3),
    n_rows_model = n_rows,
    n_users_model = n_users,
    singular = singular,
    n_rows_context = n_rows_context,
    n_users_context = n_users_context,
    prop_nonzero_context = round(prop_nonzero_context, 3),
    mean_raw_context = round(mean_raw_context, 3),
    sd_raw_context = round(sd_raw_context, 3)
  ) %>%
  arrange(context, p)

supp_daily_aggregate_emoji_interaction_table_rounded <- daily_aggregate_emoji_interaction_results %>%
  transmute(
    outcome = outcome,
    emoji_feature = label,
    feature = feature,
    beta_interaction_public_minus_private = round(estimate, 3),
    se = round(se, 3),
    ci_low = round(conf.low, 3),
    ci_high = round(conf.high, 3),
    p = signif(p.value, 3),
    p_fdr = signif(p_fdr, 3),
    n_rows_model = n_rows,
    n_users_model = n_users,
    singular = singular
  ) %>%
  arrange(p)

write.csv(
  supp_daily_aggregate_emoji_context_table_rounded,
  "results/supp_daily_aggregate_emoji_context_specific_associations_rounded.csv",
  row.names = FALSE
)

write.csv(
  supp_daily_aggregate_emoji_interaction_table_rounded,
  "results/supp_daily_aggregate_emoji_context_interactions_rounded.csv",
  row.names = FALSE
)

############################
#### 11.6) OPTIONAL SUPPLEMENT FIGURE
############################

daily_aggregate_emoji_plot_df <- supp_daily_aggregate_emoji_context_table_rounded %>%
  filter(!is.na(beta), !is.na(ci_low), !is.na(ci_high)) %>%
  mutate(
    context = factor(context, levels = c("private", "public")),
    emoji_feature = forcats::fct_reorder(emoji_feature, abs(beta), .fun = max, .desc = FALSE),
    sig_ci = if_else(ci_low > 0 | ci_high < 0, "CI excludes 0", "CI includes 0")
  )

if (nrow(daily_aggregate_emoji_plot_df) > 0) {
  fig_supp_daily_aggregate_emoji <- ggplot(
    daily_aggregate_emoji_plot_df,
    aes(
      x = beta,
      y = emoji_feature,
      xmin = ci_low,
      xmax = ci_high,
      shape = context,
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
      position = position_dodge(width = 0.55),
      height = 0,
      linewidth = 0.75,
      color = "gray35"
    ) +
    geom_point(
      position = position_dodge(width = 0.55),
      size = 3,
      color = "black"
    ) +
    scale_shape_manual(
      values = c("private" = 16, "public" = 17),
      labels = c("private" = "Private", "public" = "Public"),
      name = "Context"
    ) +
    scale_alpha_manual(
      values = c("CI excludes 0" = 1, "CI includes 0" = 0.45),
      guide = "none"
    ) +
    labs(
      x = "Standardized coefficient (\u03b2) with 95% CI",
      y = NULL
    ) +
    base_theme +
    theme(
      legend.position = "top",
      plot.margin = margin(8, 12, 8, 8)
    )
  
  fig_supp_daily_aggregate_emoji
  
  ggsave(
    filename = "figures/supp_daily_aggregate_emoji_coefficients.png",
    plot = fig_supp_daily_aggregate_emoji,
    width = 7.2,
    height = 4.8,
    dpi = 300
  )
  
  ggsave(
    filename = "figures/supp_daily_aggregate_emoji_coefficients.pdf",
    plot = fig_supp_daily_aggregate_emoji,
    width = 7.2,
    height = 4.8
  )
}

message("Daily-level aggregate emoji analyses complete.")

## finish


### emoji main fig plot


############################
#### MAIN PAPER FIGURE:
#### EMOJI CONTEXT-SPECIFIC ASSOCIATIONS
#### TRAIT AFFECTIVE BALANCE
############################

top_n_each <- 4

emoji_balance_scatter_df <- emoji_context_results %>%
  filter(
    outcome == "affective_balance_z",
    !is.na(estimate),
    context %in% c("private", "public")
  ) %>%
  select(
    feature,
    symbol,
    context,
    estimate,
    conf.low,
    conf.high,
    p.value,
    p_fdr,
    n_users_nonzero_context,
    prop_users_nonzero_context
  ) %>%
  tidyr::pivot_wider(
    names_from = context,
    values_from = c(
      estimate,
      conf.low,
      conf.high,
      p.value,
      p_fdr,
      n_users_nonzero_context,
      prop_users_nonzero_context
    ),
    names_sep = "_"
  ) %>%
  filter(
    !is.na(estimate_private),
    !is.na(estimate_public)
  ) %>%
  mutate(
    context_difference = estimate_private - estimate_public,
    abs_context_difference = abs(context_difference),
    max_abs_estimate = pmax(abs(estimate_private), abs(estimate_public), na.rm = TRUE)
  )

# Label a principled subset:
# 1) strongest positive private associations
# 2) strongest negative private associations
# 3) largest private-public differences

label_positive_private <- emoji_balance_scatter_df %>%
  slice_max(order_by = estimate_private, n = top_n_each, with_ties = FALSE) %>%
  mutate(label_reason = "Positive private association")

label_negative_private <- emoji_balance_scatter_df %>%
  slice_min(order_by = estimate_private, n = top_n_each, with_ties = FALSE) %>%
  mutate(label_reason = "Negative private association")

label_context_difference <- emoji_balance_scatter_df %>%
  slice_max(order_by = abs_context_difference, n = top_n_each, with_ties = FALSE) %>%
  mutate(label_reason = "Private-public difference")

emoji_labels_df <- bind_rows(
  label_positive_private,
  label_negative_private,
  label_context_difference
) %>%
  group_by(feature, symbol) %>%
  summarise(
    estimate_private = first(estimate_private),
    estimate_public = first(estimate_public),
    context_difference = first(context_difference),
    abs_context_difference = first(abs_context_difference),
    label_reason = paste(unique(label_reason), collapse = "; "),
    .groups = "drop"
  )

write.csv(
  emoji_balance_scatter_df,
  "results/figure_emoji_affective_balance_private_public_scatter_all_emojis.csv",
  row.names = FALSE
)

write.csv(
  emoji_labels_df,
  "results/figure_emoji_affective_balance_private_public_scatter_labeled_emojis.csv",
  row.names = FALSE
)

axis_lim_emoji <- max(
  abs(c(
    emoji_balance_scatter_df$estimate_private,
    emoji_balance_scatter_df$estimate_public
  )),
  na.rm = TRUE
)

axis_lim_emoji <- max(0.30, ceiling(axis_lim_emoji * 10) / 10)
axis_lim_emoji <- min(axis_lim_emoji, 0.60)

fig_emoji_context_scatter <- ggplot(
  emoji_balance_scatter_df,
  aes(
    x = estimate_private,
    y = estimate_public
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray75",
    linewidth = 0.45
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "gray75",
    linewidth = 0.45
  ) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dotted",
    color = "gray60",
    linewidth = 0.55
  ) +
  
  # All emojis
  geom_point(
    color = "gray55",
    alpha = 0.55,
    size = 2.0
  ) +
  
  # Labeled subset
  geom_point(
    data = emoji_labels_df,
    aes(
      x = estimate_private,
      y = estimate_public
    ),
    inherit.aes = FALSE,
    color = "black",
    alpha = 0.95,
    size = 2.8
  ) +
  ggrepel::geom_text_repel(
    data = emoji_labels_df,
    aes(
      x = estimate_private,
      y = estimate_public,
      label = symbol
    ),
    inherit.aes = FALSE,
    size = 6.0,
    box.padding = 0.35,
    point.padding = 0.25,
    min.segment.length = 0,
    segment.color = "gray70",
    segment.alpha = 0.85,
    color = "black",
    show.legend = FALSE,
    seed = 123,
    max.overlaps = Inf
  ) +
  
  coord_equal() +
  scale_x_continuous(
    limits = c(-axis_lim_emoji, axis_lim_emoji),
    breaks = seq(-axis_lim_emoji, axis_lim_emoji, by = 0.1),
    labels = scales::label_number(accuracy = 0.1, trim = TRUE),
    oob = scales::squish
  ) +
  scale_y_continuous(
    limits = c(-axis_lim_emoji, axis_lim_emoji),
    breaks = seq(-axis_lim_emoji, axis_lim_emoji, by = 0.1),
    labels = scales::label_number(accuracy = 0.1, trim = TRUE),
    oob = scales::squish
  ) +
  labs(
    x = "Private communication\nassociation with trait affective balance",
    y = "Public communication\nassociation with trait affective balance"
  ) +
  base_theme +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10.5),
    panel.grid.minor = element_blank(),
    plot.margin = margin(8, 14, 8, 8)
  )

fig_emoji_context_scatter

ggsave(
  filename = "figures/figure_emoji_affective_balance_private_public_context_scatter.png",
  plot = fig_emoji_context_scatter,
  width = 6.4,
  height = 6.2,
  dpi = 300
)



