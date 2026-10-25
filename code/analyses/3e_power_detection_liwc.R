############################
#### POWER TO DETECT WITHIN-PERSON LIWC ASSOCIATIONS
############################

# Additional sensitivity analyses for the revision; run from the project root.
# Each scenario sets one marker's within-person slope to -.10 or +.10 in BOTH
# contexts. Other markers can share signal through their observed correlations.
# Language, missingness and observation counts are held fixed. Affect is generated
# once per occasion, with Gaussian participant intercepts and occasion errors.
# Reference within-person SDs are fixed, as in the equivalence analysis. This is
# power under these assumptions, not observed-effect power or equivalence power.

############################
#### 0) PREPARATION
############################

required_packages <- c("dplyr", "lme4", "clubSandwich")
invisible(lapply(required_packages, library, character.only = TRUE))

set.seed(123)
n_sim <- 2000L # Use 200 for a pilot run.
effect_sizes <- c(-0.10, 0.10)
alpha <- 0.05
model_control <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))

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
#### 2) PREPARE DATA AND MODEL TEMPLATES
############################

pooled_within_sd <- function(x, group) {
  deviations <- x - ave(x, group, FUN = mean)
  df <- length(x) - length(unique(group))
  if (df <= 0) return(NA_real_)
  sqrt(sum(deviations^2) / df)
}

check_convergence <- function(model) {
  convergence <- model@optinfo$conv
  messages <- unlist(convergence$lme4$messages)
  messages <- messages[!grepl("boundary.*singular", messages)]
  if (any(unlist(convergence$opt) != 0) || length(messages)) {
    stop("Model did not converge: ", paste(messages, collapse = "; "))
  }
}

prepare_power_data <- function(data, outcome, occasion) {
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
  group <- interaction(dat$user_id, dat$context, drop = TRUE)
  outcome_sd <- pooled_within_sd(dat[[outcome]], group)
  if (!is.finite(outcome_sd) || outcome_sd <= 0) {
    stop("Insufficient within-person affect variation.")
  }
  dat$valence_z <- (dat[[outcome]] - mean(dat[[outcome]])) / outcome_sd

  occasions <- dat %>% distinct(occasion_id, user_id, valence_z)
  if (nrow(occasions) != n_distinct(dat$occasion_id)) {
    stop("Private and public rows contain different affect scores for the same occasion.")
  }
  dat$occasion_index <- match(dat$occasion_id, occasions$occasion_id)

  # Estimate participant variability from unique affect reports, not duplicated rows.
  intercept_model <- lmer(
    valence_z ~ 1 + (1 | user_id), data = occasions,
    REML = TRUE, control = model_control
  )
  check_convergence(intercept_model)
  participant_sd <- sqrt(as.numeric(VarCorr(intercept_model)$user_id[1, 1]))

  models <- lapply(theory_features, function(feature) {
    feature_data <- dat %>%
      filter(is.finite(.data[[feature]])) %>%
      mutate(user_id = droplevels(user_id)) %>%
      group_by(user_id, context) %>%
      mutate(
        feature_person_mean = mean(.data[[feature]]),
        feature_within = .data[[feature]] - feature_person_mean
      ) %>%
      ungroup()

    between_means <- feature_data %>% distinct(user_id, context, feature_person_mean)
    feature_group <- interaction(feature_data$user_id, feature_data$context, drop = TRUE)
    language_sd <- pooled_within_sd(feature_data[[feature]], feature_group)
    between_sd <- sd(between_means$feature_person_mean)
    if (!is.finite(language_sd) || language_sd <= 0 ||
        !is.finite(between_sd) || between_sd <= 0) {
      stop("Insufficient language variation for ", feature)
    }
    feature_data <- feature_data %>%
      mutate(
        between_z = (feature_person_mean - mean(between_means$feature_person_mean)) / between_sd,
        within_z = feature_within / language_sd
      )
    design <- model.matrix(~ between_z * context + within_z * context, data = feature_data)
    if (qr(design)$rank < ncol(design)) stop("Slopes are not separately estimable for ", feature)

    model <- lmer(
      valence_z ~ between_z * context + within_z * context + (1 | user_id),
      data = feature_data, REML = FALSE, na.action = na.fail, control = model_control
    )
    check_convergence(model)

    coefficient_names <- names(fixef(model))
    contrasts <- matrix(
      0, nrow = 2, ncol = length(coefficient_names),
      dimnames = list(c("private", "public"), coefficient_names)
    )
    contrasts[, "within_z"] <- 1
    interaction_term <- intersect(
      c("within_z:contextpublic", "contextpublic:within_z"), coefficient_names
    )
    stopifnot(length(interaction_term) == 1)
    contrasts["public", interaction_term] <- 1

    list(data = feature_data, model = model, contrasts = contrasts, language_sd = language_sd)
  })
  names(models) <- theory_features

  list(
    data = dat, occasions = occasions, models = models, outcome_sd = outcome_sd,
    participant_sd = participant_sd,
    user_index = match(occasions$user_id, levels(dat$user_id))
  )
}

state_data <- list(
  daily = prepare_power_data(keyboard_data_day, "valence_day_mean", "date"),
  momentary = prepare_power_data(keyboard_data_moment, "valence", "es_questionnaire_id")
)

############################
#### 3) DEFINE SIMULATED EFFECTS
############################

make_signal <- function(data, feature, effect) {
  dat <- data$models[[feature]]$data
  language <- matrix(0, nrow(data$occasions), 2,
                     dimnames = list(NULL, c("private", "public")))
  for (context in colnames(language)) {
    index <- which(dat$context == context)
    language[dat$occasion_index[index], context] <- dat$within_z[index]
  }

  # Account for shared occasions: simply adding the private/public signals would
  # change each context's slope when the two language streams are correlated.
  gram <- crossprod(language)
  if (rcond(gram) < 1e-10) stop("Cannot calibrate both context slopes for ", feature)
  signal <- drop(language %*% solve(gram, diag(gram) * effect))
  implied_slopes <- drop(crossprod(language, signal)) / diag(gram)
  stopifnot(all(abs(implied_slopes - effect) < 1e-8))

  # Keep expected within-person affect variance at one on the reference scale.
  group <- interaction(data$data$user_id, data$data$context, drop = TRUE)
  signal_variance <- pooled_within_sd(signal[data$data$occasion_index], group)^2
  if (!is.finite(signal_variance) || signal_variance >= 1) {
    stop("The specified signal exceeds the available affect variance for ", feature)
  }
  list(mean = signal, residual_sd = sqrt(1 - signal_variance))
}

get_p_values <- function(data, response) {
  p_values <- matrix(NA_real_, length(theory_features), 2,
                     dimnames = list(theory_features, c("private", "public")))
  for (feature in theory_features) {
    template <- data$models[[feature]]
    model <- suppressMessages(refit(
      template$model, newresp = response[template$data$occasion_index], control = model_control
    ))
    check_convergence(model)
    V_CR2 <- vcovCR(model, cluster = template$data$user_id, type = "CR2")
    estimates <- as.data.frame(linear_contrast(
      model, vcov = V_CR2, contrasts = template$contrasts, test = "Satterthwaite"
    ))
    if (any(!is.finite(as.matrix(estimates[c("Est", "SE", "df")]))) ||
        any(estimates$SE <= 0) || any(estimates$df <= 0)) {
      stop("Uncertainty could not be estimated for ", feature)
    }
    p_values[feature, ] <- 2 * pt(abs(estimates$Est / estimates$SE), estimates$df,
                                 lower.tail = FALSE)
  }
  p_values
}

############################
#### 4) SIMULATE DETECTION POWER
############################

run_detection_power <- function(data, feature, effect, n_sim) {
  stopifnot(length(n_sim) == 1, is.finite(n_sim), n_sim > 0, n_sim == as.integer(n_sim))
  signal <- make_signal(data, feature, effect)
  rejected <- matrix(FALSE, n_sim, 4)
  failed <- logical(n_sim)
  first_error <- ""

  for (i in seq_len(n_sim)) {
    participant_effect <- rnorm(nlevels(data$data$user_id), sd = data$participant_sd)
    response <- signal$mean + participant_effect[data$user_index] +
      rnorm(nrow(data$occasions), sd = signal$residual_sd)

    p_values <- tryCatch(get_p_values(data, response), error = function(e) {
      if (first_error == "") first_error <<- conditionMessage(e)
      NULL
    })
    if (is.null(p_values)) {
      # Count failed families as non-rejections; do not discard them from n_sim.
      failed[i] <- TRUE
      next
    }
    p_Holm <- apply(p_values, 2, p.adjust, method = "holm", n = 4)
    rejected[i, ] <- c(p_values[feature, ] < alpha, p_Holm[feature, ] < alpha)
    if (i %% 500 == 0) message("  Completed ", i, " of ", n_sim, " repetitions")
  }

  successes <- colSums(rejected)
  power <- successes / n_sim
  mc_intervals <- t(vapply(successes, function(k) {
    binom.test(k, n_sim)$conf.int
  }, numeric(2)))

  counts <- data$models[[feature]]$data %>%
    group_by(user_id, context) %>%
    summarise(n_observations = n(), language_sd = sd(.data[[feature]]), .groups = "drop") %>%
    group_by(context) %>%
    summarise(
      n_rows = sum(n_observations), n_participants = n(),
      n_varying_participants = sum(language_sd > 0, na.rm = TRUE), .groups = "drop"
    )

  if (any(failed)) warning(sum(failed), " failed repetitions for ", feature,
                            " at beta = ", effect, "; counted as non-rejections.")

  tibble(
    feature = feature, feature_label = unname(theory_feature_labels[feature]),
    context = c("private", "public"), true_beta = effect,
    n_sim = n_sim, alpha = alpha, n_failed = sum(failed), first_error = first_error,
    power_unadjusted = power[1:2],
    mc95_lower_unadjusted = mc_intervals[1:2, 1],
    mc95_upper_unadjusted = mc_intervals[1:2, 2],
    power_Holm = power[3:4],
    mc95_lower_Holm = mc_intervals[3:4, 1],
    mc95_upper_Holm = mc_intervals[3:4, 2],
    language_within_sd = data$models[[feature]]$language_sd,
    outcome_within_sd = data$outcome_sd,
    participant_sd_z = data$participant_sd, residual_sd_z = signal$residual_sd
  ) %>%
    left_join(counts %>% mutate(context = as.character(context)), by = "context")
}

power_results <- list()
scenario <- 0L

for (timescale in names(state_data)) {
  for (feature in theory_features) {
    for (effect in effect_sizes) {
      scenario <- scenario + 1L
      seed <- 123L + scenario
      set.seed(seed)
      message("Simulating ", timescale, ": ", feature, ", beta = ", effect)
      power_results[[scenario]] <- run_detection_power(
        state_data[[timescale]], feature, effect, n_sim
      ) %>% mutate(timescale = timescale, seed = seed, .before = 1)
    }
  }
}

############################
#### 5) SAVE RESULTS
############################

power_results <- bind_rows(power_results)
stopifnot(nrow(power_results) == 32)

dir.create("results", recursive = TRUE, showWarnings = FALSE)
write.csv(power_results, "results/liwc_within_person_detection_power.csv", row.names = FALSE)

power_results %>%
  select(timescale, feature_label, context, true_beta, n_participants,
         power_unadjusted, power_Holm, n_failed) %>%
  print(n = 32)

# finish
