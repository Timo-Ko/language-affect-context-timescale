##################################################
#### SUPPLEMENT: VOLUME-MATCHED PRIVATE/PUBLIC
#### TRAIT-AFFECT PREDICTION
##################################################

############################
#### 0) PREPARATION ####
############################

packages <- c(
  "dplyr",
  "purrr",
  "tibble",
  "ggplot2",
  "mlr3",
  "mlr3learners",
  "mlr3pipelines",
  "ranger",
  "glmnet",
  "future",
  "stringr",
  "lgr",
  "scales",
  "readr"
)
invisible(lapply(packages, library, character.only = TRUE))

source("code/analyses/helper/plot_theme.R")
source("code/analyses/helper/msr_pearson.R")
source("code/data_processing/keyboard_features/extract_keyboard_features.R")

set.seed(123, kind = "L'Ecuyer")

dir.create("results", recursive = TRUE, showWarnings = FALSE)
dir.create("figures", recursive = TRUE, showWarnings = FALSE)

B <- 10L

workers <- as.integer(Sys.getenv("VOLUME_MATCHED_WORKERS", unset = "4"))
if (is.na(workers) || workers < 1) workers <- 4L

store_bmr <- tolower(Sys.getenv("VOLUME_MATCHED_STORE_BMR", unset = "false")) %in%
  c("true", "t", "1", "yes", "y")

private_values <- c("Messaging")
public_values <- c("Posting", "Commenting")
minimum_trait_words <- 100

############################
#### 1) READ REFERENCE DATA ####
############################

reference_ml_path <- "data/results/keyboard_data_trait_ml.rds"

if (!file.exists(reference_ml_path)) {
  stop(
    "Reference ML file not found: ", reference_ml_path, "\n",
    "Run code/analyses/3a_mlpreprocessing.R after creating ",
    "data/results/keyboard_data_trait_final.rds."
  )
}

keyboard_data_trait_ml <- readRDS(reference_ml_path) %>%
  as.data.frame() %>%
  mutate(user_id = as.character(user_id))

required_reference_cols <- c("user_id", "scope", "pa_panas", "na_panas")
missing_reference_cols <- setdiff(required_reference_cols, names(keyboard_data_trait_ml))

if (length(missing_reference_cols) > 0) {
  stop(
    "Reference trait ML data are missing required columns: ",
    paste(missing_reference_cols, collapse = ", ")
  )
}

original_private_ref <- keyboard_data_trait_ml %>%
  filter(scope == "private", !is.na(pa_panas), !is.na(na_panas))

original_public_ref <- keyboard_data_trait_ml %>%
  filter(scope == "public", !is.na(pa_panas), !is.na(na_panas))

eligible_ids_reference <- intersect(
  original_private_ref$user_id,
  original_public_ref$user_id
)

if (length(eligible_ids_reference) < 30) {
  stop(
    "Fewer than 30 participants have both private and public trait-level rows ",
    "with valid PA and NA in ", reference_ml_path, "."
  )
}

trait_outcomes <- keyboard_data_trait_ml %>%
  filter(user_id %in% eligible_ids_reference) %>%
  group_by(user_id) %>%
  summarise(
    age = dplyr::first(age[!is.na(age)], default = NA_real_),
    gender = dplyr::first(as.character(gender[!is.na(gender)]), default = NA_character_),
    pa_panas = dplyr::first(pa_panas[!is.na(pa_panas)], default = NA_real_),
    na_panas = dplyr::first(na_panas[!is.na(na_panas)], default = NA_real_),
    .groups = "drop"
  )

if (sd(trait_outcomes$pa_panas, na.rm = TRUE) == 0 ||
    sd(trait_outcomes$na_panas, na.rm = TRUE) == 0) {
  stop("Trait PA or trait NA has zero variance in the eligible reference sample.")
}

############################
#### 2) FIND SESSION-LEVEL INPUTS ####
############################

# Upstream construction of trait-level features starts from one preprocessed
# session-level RDS file per participant. See:
# code/data_processing/02_SOURCE_feature_extraction.R, lines defining in_dir.

session_dir <- Sys.getenv(
  "KEYBOARD_SESSION_DIR",
  unset = "/home/rstudio/data/ps_keyboard"
)

if (!dir.exists(session_dir)) {
  stop(
    "Session-level keyboard directory not found: ", session_dir, "\n",
    "Set KEYBOARD_SESSION_DIR to the directory containing the per-user .rds ",
    "files created upstream from the keyboard preprocessing pipeline."
  )
}

session_files <- list.files(session_dir, pattern = "\\.rds$", full.names = TRUE)

if (length(session_files) == 0) {
  stop("No .rds session-level keyboard files found in ", session_dir)
}

session_file_lookup <- tibble(
  user_id = stringr::str_remove(basename(session_files), "\\.rds$"),
  file = session_files
)

############################
#### 3) LOAD FEATURE LOOKUPS ####
############################

read_first_existing_rds <- function(paths, object_name) {
  path <- paths[file.exists(paths)][1]
  if (is.na(path)) {
    warning(
      "Could not find ", object_name, " lookup file. Features depending on ",
      object_name, " may be unavailable and will be excluded consistently."
    )
    return(NULL)
  }
  
  if (grepl("\\.rds$", path, ignore.case = TRUE)) {
    return(readRDS(path))
  }
  
  load_env <- new.env(parent = emptyenv())
  loaded_objects <- load(path, envir = load_env)
  
  if (object_name %in% loaded_objects) {
    return(get(object_name, envir = load_env))
  }
  
  if (length(loaded_objects) == 1) {
    return(get(loaded_objects, envir = load_env))
  }
  
  stop(
    "Could not identify ", object_name, " inside ", path,
    ". Objects found: ", paste(loaded_objects, collapse = ", ")
  )
}

emoji_df <- read_first_existing_rds(
  c("data/helper/emoji_df.rds", "data/helper/emoji_df.RData"),
  "emoji_df"
)

emoticons_df <- read_first_existing_rds(
  c("data/helper/emoticons_df.rds", "data/helper/emoticons_df.RData"),
  "emoticons_df"
)

if (file.exists("data/helper/DE-LIWC2015.rimealiases")) {
  liwc.names <- readr::read_delim(
    "data/helper/DE-LIWC2015.rimealiases",
    delim = "\t",
    col_names = c("LIWC.cat", "C.cat")
  )
  liwc.names <- dplyr::bind_rows(
    liwc.names,
    tibble::tibble(LIWC.cat = "unknown", C.cat = "unknown")
  )
  liwc.names$LIWC.name <- paste0("LIWC_", liwc.names$LIWC.cat)
} else {
  warning(
    "Could not find data/helper/DE-LIWC2015.rimealiases. LIWC features will ",
    "only be recomputed if extract_keyboard_features can infer them otherwise."
  )
  liwc.names <- NULL
}

############################
#### 4) HELPER FUNCTIONS ####
############################

non_feature_cols_trait <- c(
  "user_id", "scope", "age", "gender", "pa_panas", "na_panas"
)

reference_feature_cols <- setdiff(
  names(keyboard_data_trait_ml),
  non_feature_cols_trait
)

reference_feature_cols <- reference_feature_cols[
  vapply(keyboard_data_trait_ml[reference_feature_cols], is.numeric, logical(1))
]

safe_median <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  median(x, na.rm = TRUE)
}

safe_sd <- function(x) {
  if (sum(!is.na(x)) < 2) return(NA_real_)
  sd(x, na.rm = TRUE)
}

safe_quantile <- function(x, p) {
  if (all(is.na(x))) return(NA_real_)
  as.numeric(quantile(x, p, na.rm = TRUE))
}

stable_user_seed_offset <- function(user_id) {
  sum(utf8ToInt(as.character(user_id))) %% 100000L
}

learner_label <- function(x) {
  dplyr::case_when(
    stringr::str_detect(x, "featureless") ~ "FL",
    stringr::str_detect(x, "ranger") ~ "RF",
    stringr::str_detect(x, "glmnet") ~ "EN",
    TRUE ~ x
  )
}

condition_label <- function(x) {
  dplyr::case_when(
    stringr::str_detect(x, "original_private") ~ "Original private",
    stringr::str_detect(x, "matched_private") ~ "Matched private",
    stringr::str_detect(x, "public") ~ "Public",
    TRUE ~ x
  )
}

outcome_label <- function(x) {
  dplyr::case_when(
    stringr::str_detect(x, "_pa$") ~ "Trait PA",
    stringr::str_detect(x, "_na$") ~ "Trait NA",
    TRUE ~ x
  )
}

load_user_sessions <- function(user_id, file) {
  df <- readRDS(file) %>%
    as.data.frame()
  
  if (!"user_uuid" %in% names(df)) {
    if ("user_id" %in% names(df)) {
      df$user_uuid <- as.character(df$user_id)
    } else {
      df$user_uuid <- as.character(user_id)
    }
  }
  
  df$user_uuid <- as.character(df$user_uuid)
  
  if (!"chars_typed" %in% names(df) &&
      all(c("character_count_added", "character_count_altered") %in% names(df))) {
    df <- df %>%
      mutate(
        chars_typed = dplyr::coalesce(character_count_added, 0) +
          dplyr::coalesce(character_count_altered, 0)
      )
  }
  
  if (!all(c("action_category", "words_typed") %in% names(df))) {
    stop(
      "Session file for user ", user_id,
      " must contain action_category and words_typed."
    )
  }
  
  df %>%
    filter(!is.na(words_typed), words_typed >= 1)
}

summarise_user_sessions <- function(user_id, file) {
  df <- load_user_sessions(user_id, file)
  
  private_sessions <- df %>%
    filter(action_category %in% private_values)
  
  public_sessions <- df %>%
    filter(action_category %in% public_values)
  
  list(
    user_id = user_id,
    file = file,
    n_private_sessions = nrow(private_sessions),
    private_words = sum(private_sessions$words_typed, na.rm = TRUE),
    public_words = sum(public_sessions$words_typed, na.rm = TRUE)
  )
}

sample_private_sessions_to_words <- function(private_sessions, target_words, seed) {
  set.seed(seed)
  
  if (nrow(private_sessions) == 0 || !is.finite(target_words) || target_words <= 0) {
    return(private_sessions[0, , drop = FALSE])
  }
  
  sampled_order <- sample(seq_len(nrow(private_sessions)), size = nrow(private_sessions))
  ordered <- private_sessions[sampled_order, , drop = FALSE]
  cumulative_words <- cumsum(ordered$words_typed)
  first_reach <- which(cumulative_words >= target_words)[1]
  
  if (is.na(first_reach)) {
    return(ordered)
  }
  
  include_idx <- seq_len(first_reach)
  include_words <- cumulative_words[first_reach]
  
  if (first_reach == 1) {
    return(ordered[include_idx, , drop = FALSE])
  }
  
  exclude_idx <- seq_len(first_reach - 1)
  exclude_words <- cumulative_words[first_reach - 1]
  
  include_mismatch <- abs(include_words - target_words)
  exclude_mismatch <- abs(exclude_words - target_words)
  
  if (exclude_words > 0 && exclude_mismatch < include_mismatch) {
    ordered[exclude_idx, , drop = FALSE]
  } else {
    ordered[include_idx, , drop = FALSE]
  }
}

aggregate_matched_private_all_iterations <- function(session_summaries, B) {
  matched_rows <- lapply(seq_len(B), function(.x) vector("list", length(session_summaries)))
  word_diag_rows <- lapply(seq_len(B), function(.x) vector("list", length(session_summaries)))
  
  for (user_i in seq_along(session_summaries)) {
    x <- session_summaries[[user_i]]
    
    if (user_i %% 10 == 1) {
      message("Precomputing matched aggregates for user ", user_i, " / ",
              length(session_summaries))
    }
    
    private_sessions <- load_user_sessions(x$user_id, x$file) %>%
      filter(action_category %in% private_values)
    
    for (iteration in seq_len(B)) {
      seed_i <- 20260519 + iteration * 100000 + stable_user_seed_offset(x$user_id)
      sampled_sessions <- sample_private_sessions_to_words(
        private_sessions = private_sessions,
        target_words = x$public_words,
        seed = seed_i
      )
      
      matched_words <- sum(sampled_sessions$words_typed, na.rm = TRUE)
      
      word_diag_rows[[iteration]][[user_i]] <- tibble(
        iteration = iteration,
        user_id = x$user_id,
        public_words = x$public_words,
        matched_private_words = matched_words,
        private_total_words = x$private_words,
        n_private_sessions = x$n_private_sessions,
        n_matched_private_sessions = nrow(sampled_sessions),
        word_ratio = matched_words / x$public_words,
        abs_mismatch = abs(matched_words - x$public_words),
        rel_mismatch = abs(matched_words - x$public_words) / x$public_words
      )
      
      if (nrow(sampled_sessions) == 0) {
        next
      }
      
      matched_rows[[iteration]][[user_i]] <- extract_keyboard_features(
        keyboard_data = sampled_sessions,
        window_identifier = "user_uuid",
        filter_var = "private",
        context_var = "action_category",
        private_values = private_values,
        public_values = public_values
      ) %>%
        mutate(
          user_id = as.character(x$user_id),
          scope = "matched_private"
        ) %>%
        select(-any_of("user_uuid"))
      
      rm(sampled_sessions)
    }
    
    rm(private_sessions)
    if (user_i %% 10 == 0) gc(verbose = FALSE)
  }
  
  list(
    matched_private = lapply(seq_len(B), function(iter) bind_rows(matched_rows[[iter]])),
    word_diag = lapply(seq_len(B), function(iter) bind_rows(word_diag_rows[[iter]]))
  )
}

prepare_condition_data <- function(data, condition, common_features) {
  data <- data %>%
    mutate(user_id = as.character(user_id)) %>%
    filter(user_id %in% eligible_ids) %>%
    select(-any_of(c("age", "gender", "pa_panas", "na_panas", "condition")))
  
  missing_features <- setdiff(common_features, names(data))
  if (length(missing_features) > 0) {
    for (feature_i in missing_features) {
      data[[feature_i]] <- NA_real_
    }
  }
  
  data %>%
    left_join(trait_outcomes, by = "user_id") %>%
    mutate(
      condition = condition
    ) %>%
    select(
      user_id, condition, age, gender, pa_panas, na_panas,
      all_of(common_features)
    ) %>%
    mutate(across(all_of(common_features), as.numeric)) %>%
    arrange(user_id)
}

make_task_backend <- function(data, target, task_features) {
  stopifnot(target %in% names(data))
  
  backend <- data[, c("user_id", target, task_features), drop = FALSE]
  
  if (sd(backend[[target]], na.rm = TRUE) == 0) {
    stop("Outcome has zero variance for target ", target)
  }
  
  backend
}

make_regr_task <- function(data, id, target, task_features) {
  backend <- make_task_backend(data, target, task_features)
  
  task <- TaskRegr$new(
    id = id,
    backend = backend,
    target = target
  )
  
  task$col_roles$group <- "user_id"
  task$col_roles$feature <- setdiff(task$col_roles$feature, "user_id")
  
  task
}

score_benchmark <- function(bmr, iteration) {
  measures <- c(
    list(msr_pearson),
    msrs(c("regr.rsq", "regr.mae", "regr.rmse"))
  )
  
  as.data.frame(bmr$score(measures)) %>%
    mutate(
      iteration = iteration,
      learner = learner_label(learner_id),
      condition = condition_label(task_id),
      outcome = outcome_label(task_id)
    )
}

summarise_performance <- function(score_df) {
  score_df %>%
    group_by(iteration, outcome, condition, learner) %>%
    summarise(
      n_folds_total = n(),
      n_folds_valid_pearson = sum(!is.na(pearson)),
      pearson_coverage = n_folds_valid_pearson / n_folds_total,
      r_md = safe_median(pearson),
      r_q25 = safe_quantile(pearson, 0.25),
      r_q75 = safe_quantile(pearson, 0.75),
      r_sd = safe_sd(pearson),
      rsq_md = safe_median(regr.rsq),
      mae_md = safe_median(regr.mae),
      .groups = "drop"
    )
}

run_trait_benchmark <- function(condition_data, iteration, task_features) {
  tasks <- purrr::imap(condition_data, function(data_i, condition_i) {
    list(
      make_regr_task(
        data = data_i,
        id = paste0(condition_i, "_pa"),
        target = "pa_panas",
        task_features = task_features
      ),
      make_regr_task(
        data = data_i,
        id = paste0(condition_i, "_na"),
        target = "na_panas",
        task_features = task_features
      )
    )
  }) %>%
    purrr::flatten()
  
  lrn_fl <- lrn("regr.featureless")
  lrn_rf <- po("imputeoor") %>>% lrn("regr.ranger", num.trees = 1000)
  lrn_rr <- po("imputehist") %>>% lrn("regr.cv_glmnet", alpha = 0.5)
  
  # This matches the final main ML script: 5-fold CV repeated 20 times.
  resampling <- rsmp("repeated_cv", folds = 5L, repeats = 20L)
  
  bmgrid <- benchmark_grid(
    task = tasks,
    learner = list(lrn_fl, lrn_rf, lrn_rr),
    resampling = resampling
  )
  
  bmr <- benchmark(
    bmgrid,
    store_models = FALSE,
    store_backends = FALSE
  )
  scored <- score_benchmark(bmr, iteration)
  
  list(
    bmr = bmr,
    scores = scored,
    summary = summarise_performance(scored)
  )
}

############################
#### 5) LOAD AND FILTER SESSIONS ####
############################

session_lookup_eligible <- session_file_lookup %>%
  filter(user_id %in% eligible_ids_reference)

if (nrow(session_lookup_eligible) == 0) {
  stop(
    "No session-level files matched participants in ", reference_ml_path,
    ". Check KEYBOARD_SESSION_DIR and user ID naming."
  )
}

message("Loading session-level data for ", nrow(session_lookup_eligible), " users.")

session_summaries <- purrr::pmap(
  list(session_lookup_eligible$user_id, session_lookup_eligible$file),
  summarise_user_sessions
)

session_diagnostics_base <- bind_rows(lapply(session_summaries, function(x) {
  tibble(
    user_id = x$user_id,
    public_words = x$public_words,
    private_words = x$private_words,
    n_private_sessions = x$n_private_sessions
  )
}))

eligible_ids <- session_diagnostics_base %>%
  filter(
    public_words >= minimum_trait_words,
    private_words >= public_words,
    n_private_sessions >= 2
  ) %>%
  pull(user_id) %>%
  intersect(eligible_ids_reference)

if (length(eligible_ids) < 30) {
  stop(
    "Fewer than 30 participants have valid PA/NA, private and public trait ",
    "rows, at least ", minimum_trait_words, " public words, and enough ",
    "private text for volume matching."
  )
}

session_summaries <- session_summaries[
  vapply(session_summaries, function(x) x$user_id %in% eligible_ids, logical(1))
]

trait_outcomes <- trait_outcomes %>%
  filter(user_id %in% eligible_ids)

message("Retained ", length(eligible_ids), " participants for volume matching.")

############################
#### 6) RUN VOLUME MATCHING + ML ####
############################

logger <- lgr::get_logger("bbotk")
logger$set_threshold("warn")

message("Precomputing matched-private trait-level aggregates for all iterations.")
matched_precomputed <- aggregate_matched_private_all_iterations(session_summaries, B)

future::plan("multisession", workers = workers)
on.exit(future::plan("sequential"), add = TRUE)

score_all <- list()
summary_all <- list()
bmr_all <- list()
word_diag_all <- list()
task_feature_log <- list()
task_features_global <- NULL
ran_static_benchmark <- FALSE

for (iter in seq_len(B)) {
  message("Volume-matched iteration ", iter, " / ", B)
  
  matched_private_raw <- matched_precomputed$matched_private[[iter]]
  word_diag <- matched_precomputed$word_diag[[iter]]
  
  if (nrow(matched_private_raw) != length(eligible_ids)) {
    stop(
      "Matched-private aggregate has ", nrow(matched_private_raw),
      " rows, but expected ", length(eligible_ids), " participants."
    )
  }
  
  matched_features_available <- intersect(reference_feature_cols, names(matched_private_raw))
  matched_features_available <- matched_features_available[
    vapply(matched_private_raw[matched_features_available], is.numeric, logical(1))
  ]
  
  if (is.null(task_features_global)) {
    task_features_global <- matched_features_available
  }
  
  task_features <- task_features_global
  
  if (length(task_features) < 5) {
    stop(
      "Fewer than five reference ML features could be recomputed from the ",
      "matched-private session data."
    )
  }
  
  original_private <- original_private_ref %>%
    filter(user_id %in% eligible_ids)
  
  public <- original_public_ref %>%
    filter(user_id %in% eligible_ids)
  
  condition_data <- list(
    original_private = prepare_condition_data(
      original_private,
      "Original private",
      task_features
    ),
    public = prepare_condition_data(
      public,
      "Public",
      task_features
    ),
    matched_private = prepare_condition_data(
      matched_private_raw,
      "Matched private",
      task_features
    )
  )
  
  feature_sets_identical <- identical(
    names(condition_data$original_private)[
      names(condition_data$original_private) %in% task_features
    ],
    names(condition_data$matched_private)[
      names(condition_data$matched_private) %in% task_features
    ]
  ) &&
    identical(
      names(condition_data$public)[names(condition_data$public) %in% task_features],
      names(condition_data$matched_private)[
        names(condition_data$matched_private) %in% task_features
      ]
    )
  
  if (!feature_sets_identical) {
    stop("Feature columns differ across original private, matched private, and public.")
  }
  
  task_feature_log[[iter]] <- tibble(
    iteration = iter,
    n_task_features = length(task_features),
    n_reference_features = length(reference_feature_cols),
    n_excluded_reference_features = length(setdiff(reference_feature_cols, task_features))
  )
  
  if (!ran_static_benchmark) {
    message("Running original private/public benchmarks once.")
    bench_static <- run_trait_benchmark(
      condition_data = condition_data[c("original_private", "public")],
      iteration = iter,
      task_features = task_features
    )
    
    score_all[[length(score_all) + 1]] <- bench_static$scores
    summary_all[[length(summary_all) + 1]] <- bench_static$summary
    
    if (store_bmr) {
      bmr_all[[length(bmr_all) + 1]] <- bench_static$bmr
    }
    
    ran_static_benchmark <- TRUE
  }
  
  bench_i <- run_trait_benchmark(
    condition_data = condition_data["matched_private"],
    iteration = iter,
    task_features = task_features
  )
  
  score_all[[length(score_all) + 1]] <- bench_i$scores
  summary_all[[length(summary_all) + 1]] <- bench_i$summary
  word_diag_all[[iter]] <- word_diag
  
  if (store_bmr) {
    bmr_all[[length(bmr_all) + 1]] <- bench_i$bmr
  }
}

score_all_df <- bind_rows(score_all)
performance_by_iteration <- bind_rows(summary_all)
word_count_diagnostics <- bind_rows(word_diag_all)
feature_log <- bind_rows(task_feature_log)

if (any(performance_by_iteration$pearson_coverage < 0.80, na.rm = TRUE)) {
  warning("Some condition x outcome x learner cells have Pearson coverage below 0.80.")
}

median_ratio <- median(word_count_diagnostics$word_ratio, na.rm = TRUE)

if (is.finite(median_ratio) && abs(median_ratio - 1) > 0.10) {
  warning(
    "Median matched/private-public word-count ratio is ",
    round(median_ratio, 3),
    "; inspect results/supp_volume_matched_word_count_diagnostics.csv."
  )
}
############################
#### 7) SUMMARISE PERFORMANCE ####
############################

word_ratio_by_iteration <- word_count_diagnostics %>%
  group_by(iteration) %>%
  summarise(
    mean_word_ratio = mean(word_ratio, na.rm = TRUE),
    median_word_ratio = median(word_ratio, na.rm = TRUE),
    .groups = "drop"
  )

performance_by_iteration <- performance_by_iteration %>%
  left_join(word_ratio_by_iteration, by = "iteration") %>%
  mutate(
    condition = factor(
      condition,
      levels = c("Original private", "Matched private", "Public")
    ),
    outcome = factor(
      outcome,
      levels = c("Trait PA", "Trait NA")
    ),
    learner = factor(
      learner,
      levels = c("FL", "RF", "EN")
    )
  ) %>%
  arrange(iteration, outcome, condition, learner)

score_all_df <- score_all_df %>%
  mutate(
    condition = factor(
      condition,
      levels = c("Original private", "Matched private", "Public")
    ),
    outcome = factor(
      outcome,
      levels = c("Trait PA", "Trait NA")
    ),
    learner = factor(
      learner,
      levels = c("FL", "RF", "EN")
    )
  )

performance_summary <- score_all_df %>%
  group_by(outcome, condition, learner) %>%
  summarise(
    n_iterations = n_distinct(iteration),
    n_scores = sum(!is.na(pearson)),
    median_r = safe_median(pearson),
    r_q25 = safe_quantile(pearson, 0.25),
    r_q75 = safe_quantile(pearson, 0.75),
    mean_pearson_coverage = mean(!is.na(pearson)),
    .groups = "drop"
  ) %>%
  arrange(outcome, condition, learner)

############################
#### 7.5) CREATE TABLE S3:
#### TEXT-VOLUME MATCHED TRAIT PREDICTION
############################

fmt_median_iqr <- function(md, q25, q75, digits = 2) {
  ifelse(
    is.na(md) | is.na(q25) | is.na(q75),
    NA_character_,
    paste0(
      formatC(md, format = "f", digits = digits),
      " [",
      formatC(q25, format = "f", digits = digits),
      ", ",
      formatC(q75, format = "f", digits = digits),
      "]"
    )
  )
}

table_s3_volume_matched <- performance_summary %>%
  filter(
    learner == "RF",
    outcome %in% c("Trait PA", "Trait NA"),
    condition %in% c("Original private", "Matched private", "Public")
  ) %>%
  mutate(
    `Outcome variable` = case_when(
      outcome == "Trait PA" ~ "Trait positive affect",
      outcome == "Trait NA" ~ "Trait negative affect",
      TRUE ~ as.character(outcome)
    ),
    Context = case_when(
      condition %in% c("Original private", "Matched private") ~ "Private",
      condition == "Public" ~ "Public",
      TRUE ~ as.character(condition)
    ),
    `Text volume` = case_when(
      condition == "Original private" ~ "Full",
      condition == "Matched private" ~ "Matched to public",
      condition == "Public" ~ "Full",
      TRUE ~ as.character(condition)
    ),
    `Median r [IQR]` = fmt_median_iqr(
      median_r,
      r_q25,
      r_q75,
      digits = 2
    ),
    outcome_order = factor(
      `Outcome variable`,
      levels = c("Trait positive affect", "Trait negative affect")
    ),
    context_order = factor(
      Context,
      levels = c("Private", "Public")
    ),
    volume_order = case_when(
      Context == "Private" & `Text volume` == "Full" ~ 1,
      Context == "Private" & `Text volume` == "Matched to public" ~ 2,
      Context == "Public" & `Text volume` == "Full" ~ 3,
      TRUE ~ 99
    )
  ) %>%
  arrange(outcome_order, volume_order) %>%
  select(
    `Outcome variable`,
    Context,
    `Text volume`,
    `Median r [IQR]`
  )

write.csv(
  table_s3_volume_matched,
  "results/table_s3_text_volume_matched_trait_prediction.csv",
  row.names = FALSE,
  na = ""
)

table_s3_volume_matched


saveRDS(
  list(
    bmr_by_iteration = if (store_bmr) bmr_all else NULL,
    score_by_fold = score_all_df,
    performance_by_iteration = performance_by_iteration,
    performance_summary = performance_summary,
    table_s3 = table_s3_volume_matched,
    word_count_diagnostics = word_count_diagnostics,
    feature_log = feature_log,
    settings = list(
      B = B,
      session_dir = session_dir,
      private_values = private_values,
      public_values = public_values,
      minimum_trait_words = minimum_trait_words,
      store_bmr_objects = store_bmr,
      resampling = "repeated_cv: folds = 5, repeats = 20",
      learners = c(
        "regr.featureless",
        "imputeoor.regr.ranger",
        "imputehist.regr.cv_glmnet"
      )
    )
  ),
  "results/bmr_volume_matched_private_trait.rds"
)

# finish