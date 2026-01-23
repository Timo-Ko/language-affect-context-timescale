# ============================================================
# Correlation utilities (Spearman + bootstrap CIs)
# ============================================================

#' Spearman correlation with bootstrap confidence interval
#'
#' Computes Spearman's rho between two variables and obtains a 95% CI
#' via nonparametric bootstrap. This is robust to skewness, zero inflation,
#' and tied ranks (for which base R often does not provide Spearman CIs).
#'
#' @param x Numeric vector.
#' @param y Numeric vector.
#' @param R Integer. Number of bootstrap resamples (default = 2000).
#' @param conf Numeric. Confidence level (default = 0.95).
#' @param min_n Integer. Minimum number of complete pairs required (default = 5).
#' @param seed Optional integer for reproducibility (default = NULL).
#' @return Named numeric vector: rho, ci_lower, ci_upper, p_value, n.
spearman_boot_ci <- function(x, y, R = 1000, conf = 0.95, min_n = 5, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  ok <- stats::complete.cases(x, y)
  x <- x[ok]
  y <- y[ok]
  n <- length(x)
  
  # Not enough data
  if (n < min_n) {
    return(c(rho = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_, p_value = NA_real_, n = n))
  }
  
  # Correlation undefined if constant
  if (length(unique(x)) < 2 || length(unique(y)) < 2) {
    return(c(rho = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_, p_value = NA_real_, n = n))
  }
  
  # Spearman estimate
  rho_hat <- suppressWarnings(stats::cor(x, y, method = "spearman"))
  
  # p-value from cor.test (ties => exact p-value not possible; use asymptotic)
  ct <- suppressWarnings(stats::cor.test(x, y, method = "spearman", exact = FALSE))
  p_val <- ct$p.value
  
  # Bootstrap distribution for rho
  boot_rho <- replicate(R, {
    idx <- sample.int(n, size = n, replace = TRUE)
    # Guard against rare constant resamples
    xb <- x[idx]; yb <- y[idx]
    if (length(unique(xb)) < 2 || length(unique(yb)) < 2) return(NA_real_)
    suppressWarnings(stats::cor(xb, yb, method = "spearman"))
  })
  
  alpha <- (1 - conf) / 2
  ci <- stats::quantile(boot_rho, probs = c(alpha, 1 - alpha), na.rm = TRUE, names = FALSE)
  
  c(rho = rho_hat, ci_lower = ci[1], ci_upper = ci[2], p_value = p_val, n = n)
}

#' Correlation function that extracts relevant variables
#'
#' @param var1 Character. Target variable name.
#' @param var2 Character. Feature variable name.
#' @param data Data frame containing both variables.
#' @param R Integer. Number of bootstrap resamples (default = 2000).
#' @param conf Numeric. Confidence level (default = 0.95).
#' @param min_n Integer. Minimum number of complete pairs required (default = 5).
#' @param seed Optional integer for reproducibility (default = NULL).
#' @return One-row data.frame with correlation estimate and bootstrap CI.
corrFunc <- function(var1, var2, data, R = 2000, conf = 0.95, min_n = 5, seed = NULL) {
  x <- data[[var1]]
  y <- data[[var2]]
  
  res <- spearman_boot_ci(x, y, R = R, conf = conf, min_n = min_n, seed = seed)
  
  data.frame(
    target   = var1,
    feature  = var2,
    r        = unname(res["rho"]),
    ci_lower = unname(res["ci_lower"]),
    ci_upper = unname(res["ci_upper"]),
    p_value  = unname(res["p_value"]),
    n        = unname(res["n"]),
    stringsAsFactors = FALSE
  )
}

#' Create correlation table (targets x features)
#'
#' Computes Spearman correlations between each target and each feature,
#' including bootstrap 95% CIs.
#'
#' @param data_frame A data frame.
#' @param targets Character vector of target variable names.
#' @param features Character vector of feature variable names.
#' @param R Integer. Number of bootstrap resamples (default = 2000).
#' @param conf Numeric. Confidence level (default = 0.95).
#' @param min_n Integer. Minimum number of complete pairs required (default = 5).
#' @param seed Optional integer for reproducibility of bootstrap (default = NULL).
#' @return Data frame with columns: target, feature, r, ci_lower, ci_upper, p_value, n.
cor_table <- function(data_frame, targets, features, R = 1000, conf = 0.95, min_n = 5, seed = NULL) {
  
  # Pairs of variables for which we want correlations
  vars <- expand.grid(target = targets, feature = features, stringsAsFactors = FALSE)
  
  # Apply corrFunc to all pairs
  cor_list <- mapply(
    FUN = function(tgt, feat) corrFunc(tgt, feat, data = data_frame, R = R, conf = conf, min_n = min_n, seed = seed),
    vars$target,
    vars$feature,
    SIMPLIFY = FALSE
  )
  
  cor_df <- do.call(rbind, cor_list)
  rownames(cor_df) <- NULL
  cor_df
}


# ============================================================
# Cluster (participant)-bootstrap Spearman CI for nested state data
# ============================================================
# Goal (between-person inference with repeated observations):
# - Keep the point estimate as the pooled Spearman across all rows
#   (as you do now), BUT
# - Compute bootstrap CIs by resampling PARTICIPANTS (clusters),
#   including all their rows each time.
#
# This yields valid uncertainty under within-person dependence.
# ============================================================

#' Spearman correlation with participant-blocked bootstrap CI
#'
#' @param x Numeric vector (row-level).
#' @param y Numeric vector (row-level).
#' @param id Vector of participant IDs, same length as x/y.
#' @param R Integer. Number of bootstrap resamples.
#' @param conf Numeric. Confidence level.
#' @param min_n Integer. Minimum number of complete pairs required.
#' @param seed Optional integer for reproducibility.
#' @return Named numeric vector: rho, ci_lower, ci_upper, p_value, n.
spearman_boot_ci_cluster <- function(x, y, id, R = 1000, conf = 0.95, min_n = 5, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  ok <- stats::complete.cases(x, y, id)
  x <- x[ok]; y <- y[ok]; id <- id[ok]
  n <- length(x)
  
  # Not enough data
  if (n < min_n) {
    return(c(rho = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_, p_value = NA_real_, n = n))
  }
  
  # Correlation undefined if constant
  if (length(unique(x)) < 2 || length(unique(y)) < 2) {
    return(c(rho = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_, p_value = NA_real_, n = n))
  }
  
  # Point estimate (same as your pooled analysis)
  rho_hat <- suppressWarnings(stats::cor(x, y, method = "spearman"))
  
  # p-value (keep as in your current pipeline; note: not cluster-robust)
  ct <- suppressWarnings(stats::cor.test(x, y, method = "spearman", exact = FALSE))
  p_val <- ct$p.value
  
  # Participant-level bootstrap
  ids <- unique(id)
  m <- length(ids)
  
  boot_rho <- replicate(R, {
    sampled_ids <- sample(ids, size = m, replace = TRUE)
    
    # collect all rows for each sampled participant (with multiplicity)
    idx_list <- lapply(sampled_ids, function(sid) which(id == sid))
    idx <- unlist(idx_list, use.names = FALSE)
    
    xb <- x[idx]; yb <- y[idx]
    if (length(xb) < min_n) return(NA_real_)
    if (length(unique(xb)) < 2 || length(unique(yb)) < 2) return(NA_real_)
    
    suppressWarnings(stats::cor(xb, yb, method = "spearman"))
  })
  
  alpha <- (1 - conf) / 2
  ci <- stats::quantile(boot_rho, probs = c(alpha, 1 - alpha), na.rm = TRUE, names = FALSE)
  
  c(rho = rho_hat, ci_lower = ci[1], ci_upper = ci[2], p_value = p_val, n = n)
}

#' corrFunc variant for nested state data (cluster bootstrap)
#'
#' @param var1 Character. Target variable name.
#' @param var2 Character. Feature variable name.
#' @param id_var Character. Participant id column name.
#' @param data Data frame.
#' @return One-row data.frame.
corrFunc_cluster <- function(var1, var2, id_var, data,
                             R = 2000, conf = 0.95, min_n = 5, seed = NULL) {
  x  <- data[[var1]]
  y  <- data[[var2]]
  id <- data[[id_var]]
  
  res <- spearman_boot_ci_cluster(x, y, id = id, R = R, conf = conf, min_n = min_n, seed = seed)
  
  data.frame(
    target   = var1,
    feature  = var2,
    r        = unname(res["rho"]),
    ci_lower = unname(res["ci_lower"]),
    ci_upper = unname(res["ci_upper"]),
    p_value  = unname(res["p_value"]),
    n        = unname(res["n"]),
    stringsAsFactors = FALSE
  )
}

#' Create correlation table (targets x features) with participant-blocked bootstrap
#'
#' @param id_var Character. Participant id column name.
#' @return Data frame with columns: target, feature, r, ci_lower, ci_upper, p_value, n.
cor_table_cluster <- function(data_frame, targets, features, id_var,
                              R = 1000, conf = 0.95, min_n = 5, seed = NULL) {
  
  stopifnot(id_var %in% names(data_frame))
  
  vars <- expand.grid(target = targets, feature = features, stringsAsFactors = FALSE)
  
  cor_list <- mapply(
    FUN = function(tgt, feat) corrFunc_cluster(tgt, feat, id_var = id_var,
                                               data = data_frame, R = R, conf = conf,
                                               min_n = min_n, seed = seed),
    vars$target,
    vars$feature,
    SIMPLIFY = FALSE
  )
  
  cor_df <- do.call(rbind, cor_list)
  rownames(cor_df) <- NULL
  cor_df
}


## cor table per self-reported gender

cor_table_by_gender <- function(data_frame, targets, features,
                                gender_var = "gender",
                                R = 1000, conf = 0.95, min_n = 5, seed = NULL,
                                drop_levels = NULL) {
  stopifnot(gender_var %in% names(data_frame))
  
  df <- data_frame
  
  # optional: drop specific gender levels (e.g., "diverse", "other", NA)
  if (!is.null(drop_levels)) {
    df <- df %>% dplyr::filter(!.data[[gender_var]] %in% drop_levels)
  }
  
  # ensure factor
  df <- df %>% dplyr::mutate(!!gender_var := as.factor(.data[[gender_var]]))
  
  genders <- levels(droplevels(df[[gender_var]]))
  genders <- genders[!is.na(genders)]
  
  out_list <- vector("list", length(genders))
  names(out_list) <- genders
  
  for (g in genders) {
    df_g <- df %>% dplyr::filter(.data[[gender_var]] == g)
    
    # run your exact pipeline within this gender
    res_g <- cor_table(
      data_frame = df_g,
      targets = targets,
      features = features,
      R = R, conf = conf, min_n = min_n, seed = seed
    )
    
    # add group meta info
    res_g$gender <- g
    res_g$n_gender_total <- nrow(df_g)
    
    out_list[[g]] <- res_g
  }
  
  dplyr::bind_rows(out_list)
}

