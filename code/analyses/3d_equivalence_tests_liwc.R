############################
#### WITHIN-PERSON LIWC EQUIVALENCE TESTS
############################

# Additional analyses for the revision; run from the project root.
# Bounds refer to slopes standardized using within-person variation in both
# language and affect, rather than the manuscript's overall affect SD.
# Reference SDs are pooled across contexts and treated as fixed scaling constants.

############################
#### 0) PREPARATION
############################

required_packages <- c("dplyr", "lme4", "clubSandwich")
invisible(lapply(required_packages, library, character.only = TRUE))

equivalence_bound <- 0.10
alpha <- 0.05

theory_feature_labels <- c(
  "liwc_posemo" = "Pos. emotion",
  "liwc_negemo" = "Neg. emotion",
  "liwc_i" = "I",
  "liwc_we" = "We"
)
theory_features <- names(theory_feature_labels)

############################
#### 1) LOAD DATA
############################

keyboard_data_day <- as.data.frame(readRDS("data/results/keyboard_data_day_final.rds"))
keyboard_data_moment <- as.data.frame(readRDS("data/results/keyboard_data_ema_final.rds"))

############################
#### 2) PREPARE STATE DATA
############################

pooled_within_sd <- function(x, group) {
  deviations <- x - ave(x, group, FUN = mean)
  df <- length(x) - length(unique(group))
  if (df <= 0) return(NA_real_)
  sqrt(sum(deviations^2) / df)
}

prepare_state_data <- function(data, outcome, occasion) {
  required <- c("user_id", "scope", outcome, occasion, theory_features)
  stopifnot(all(required %in% names(data)))
  stopifnot(all(vapply(data[c(outcome, theory_features)], is.numeric, logical(1))))

  if (occasion == "date") data$date <- as.Date(data$date)

  dat <- data %>%
    filter(
      scope %in% c("private", "public"),
      !is.na(user_id), !is.na(.data[[occasion]]), is.finite(.data[[outcome]])
    ) %>%
    mutate(
      user_id = factor(user_id),
      context = factor(scope, levels = c("private", "public")),
      occasion_id = interaction(user_id, .data[[occasion]], drop = TRUE)
    )

  if (anyDuplicated(dat[c("user_id", "context", "occasion_id")])) {
    stop("Duplicate participant-context-occasion rows.")
  }
  occasion_values <- split(dat[[outcome]], dat$occasion_id)
  if (any(vapply(occasion_values, function(x) length(unique(x)) > 1, logical(1)))) {
    stop("Private and public rows contain different affect scores for the same occasion.")
  }

  # Pooled within-person SD uses N minus the number of participant-context groups.
  group <- interaction(dat$user_id, dat$context, drop = TRUE)
  outcome_sd <- pooled_within_sd(dat[[outcome]], group)
  if (!is.finite(outcome_sd) || outcome_sd <= 0) {
    stop("Insufficient within-person affect variation.")
  }

  # Rescale the outcome without removing its between-person differences.
  dat$valence_z <- (dat[[outcome]] - mean(dat[[outcome]])) / outcome_sd

  list(data = dat, outcome_sd = outcome_sd)
}

state_data <- list(
  daily = prepare_state_data(keyboard_data_day, "valence_day_mean", "date"),
  momentary = prepare_state_data(keyboard_data_moment, "valence", "es_questionnaire_id")
)

############################
#### 3) FIT MODELS AND TEST EQUIVALENCE
############################

fit_equivalence_model <- function(data, feature, timescale) {
  dat <- data$data %>%
    filter(is.finite(.data[[feature]])) %>%
    mutate(user_id = droplevels(user_id)) %>%
    group_by(user_id, context) %>%
    mutate(
      feature_person_mean = mean(.data[[feature]]),
      feature_within = .data[[feature]] - feature_person_mean
    ) %>%
    ungroup()

  between_means <- dat %>% distinct(user_id, context, feature_person_mean)
  group <- interaction(dat$user_id, dat$context, drop = TRUE)
  language_sd <- pooled_within_sd(dat[[feature]], group)
  between_sd <- sd(between_means$feature_person_mean)

  if (!is.finite(language_sd) || language_sd <= 0 ||
      !is.finite(between_sd) || between_sd <= 0) {
    stop(timescale, ": insufficient language variation for ", feature)
  }

  dat <- dat %>%
    mutate(
      between_z = (feature_person_mean - mean(between_means$feature_person_mean)) / between_sd,
      within_z = feature_within / language_sd
    )

  design <- model.matrix(~ between_z * context + within_z * context, data = dat)
  if (qr(design)$rank < ncol(design)) {
    stop(timescale, ": slopes are not separately estimable for ", feature)
  }

  model <- lmer(
    valence_z ~ between_z * context + within_z * context + (1 | user_id),
    data = dat,
    REML = FALSE,
    na.action = na.fail,
    control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )

  convergence <- model@optinfo$conv
  convergence_messages <- unlist(convergence$lme4$messages)
  convergence_messages <- convergence_messages[!grepl("boundary.*singular", convergence_messages)]
  if (any(unlist(convergence$opt) != 0) || length(convergence_messages)) {
    stop(timescale, ": model did not converge for ", feature)
  }

  V_CR2 <- vcovCR(model, cluster = dat$user_id, type = "CR2")

  # Private slope = within_z; public slope = within_z + its context interaction.
  coefficient_names <- names(fixef(model))
  contrasts <- matrix(
    0, nrow = 2, ncol = length(coefficient_names),
    dimnames = list(c("private", "public"), coefficient_names)
  )
  contrasts[, "within_z"] <- 1
  interaction_term <- intersect(
    c("within_z:contextpublic", "contextpublic:within_z"),
    coefficient_names
  )
  stopifnot(length(interaction_term) == 1)
  contrasts["public", interaction_term] <- 1

  estimates <- as.data.frame(linear_contrast(
    model, vcov = V_CR2, contrasts = contrasts,
    test = "Satterthwaite", level = 1 - 2 * alpha
  ))
  if (any(!is.finite(as.matrix(estimates[c("Est", "SE", "df")]))) ||
      any(estimates$SE <= 0) || any(estimates$df <= 0)) {
    stop(timescale, ": uncertainty could not be estimated for ", feature)
  }

  context_counts <- dat %>%
    group_by(user_id, context) %>%
    summarise(n_observations = n(), language_sd = sd(.data[[feature]]), .groups = "drop") %>%
    group_by(context) %>%
    summarise(
      n_rows = sum(n_observations),
      n_participants = n(),
      n_varying_participants = sum(language_sd > 0, na.rm = TRUE),
      .groups = "drop"
    )

  # Both one-sided tests must reject: beta > -bound AND beta < bound.
  tibble(
    timescale = timescale,
    feature = feature,
    feature_label = unname(theory_feature_labels[feature]),
    context = c("private", "public"),
    beta_within = estimates$Est,
    se_CR2 = estimates$SE,
    df_Satterthwaite = estimates$df,
    bound_lower = -equivalence_bound,
    bound_upper = equivalence_bound,
    ci90_lower = estimates$CI_L,
    ci90_upper = estimates$CI_U,
    ci95_lower = beta_within - qt(0.975, df_Satterthwaite) * se_CR2,
    ci95_upper = beta_within + qt(0.975, df_Satterthwaite) * se_CR2,
    t_lower = (beta_within - bound_lower) / se_CR2,
    t_upper = (beta_within - bound_upper) / se_CR2,
    p_lower = pt(t_lower, df_Satterthwaite, lower.tail = FALSE),
    p_upper = pt(t_upper, df_Satterthwaite, lower.tail = TRUE),
    p_TOST = pmax(p_lower, p_upper),
    equivalent_unadjusted = p_TOST < alpha,
    language_within_sd = language_sd,
    outcome_within_sd = data$outcome_sd,
    singular_fit = isSingular(model, tol = 1e-4)
  ) %>%
    left_join(context_counts %>% mutate(context = as.character(context)), by = "context")
}

equivalence_results <- list()

for (timescale in names(state_data)) {
  for (feature in theory_features) {
    message("Fitting ", timescale, ": ", feature)
    equivalence_results[[paste(timescale, feature, sep = "_")]] <-
      fit_equivalence_model(state_data[[timescale]], feature, timescale)
  }
}

############################
#### 4) ADJUST AND SAVE RESULTS
############################

# Adjust the combined TOST p values across four markers per timescale and context.
# The confidence intervals remain pointwise; the main decision uses p_TOST_Holm.
equivalence_results <- bind_rows(equivalence_results) %>%
  group_by(timescale, context) %>%
  mutate(
    p_TOST_Holm = p.adjust(p_TOST, method = "holm", n = 4),
    equivalent_Holm = p_TOST_Holm < alpha
  ) %>%
  ungroup()

stopifnot(nrow(equivalence_results) == 16)

dir.create("results", recursive = TRUE, showWarnings = FALSE)
write.csv(
  equivalence_results,
  file = "results/liwc_within_person_equivalence.csv",
  row.names = FALSE
)

source("code/analyses/helper/table_s10_equivalence_power.R")
table_s10_equivalence_power <- write_table_s10_equivalence_power()

equivalence_results %>%
  select(timescale, feature_label, context, beta_within, ci90_lower, ci90_upper,
         p_TOST_Holm, equivalent_Holm) %>%
  print(n = 16)

# finish
