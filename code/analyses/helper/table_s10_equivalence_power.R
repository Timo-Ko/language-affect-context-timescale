# Format the combined supplementary table from the two detailed result exports.
# Call after either analysis; the table is written once both inputs are available.
write_table_s10_equivalence_power <- function(results_dir = "results") {
  input_paths <- file.path(results_dir, c(
    "liwc_within_person_equivalence.csv", "liwc_within_person_detection_power.csv"
  ))
  if (!all(file.exists(input_paths))) {
    message("Combined Table S10 awaits: ",
            paste(basename(input_paths[!file.exists(input_paths)]), collapse = ", "))
    return(invisible(NULL))
  }
  equivalence <- read.csv(input_paths[1], stringsAsFactors = FALSE)
  power <- read.csv(input_paths[2], stringsAsFactors = FALSE)
  stopifnot(
    all(c("timescale", "feature", "context", "beta_within", "ci90_lower",
          "ci90_upper", "p_TOST_Holm") %in% names(equivalence)),
    all(c("timescale", "feature", "context", "true_beta", "power_Holm") %in% names(power))
  )

  labels <- c(liwc_posemo = "Positive emotion", liwc_negemo = "Negative emotion",
              liwc_i = "I", liwc_we = "We")
  rows <- expand.grid(context = c("private", "public"), feature = names(labels),
                      timescale = c("daily", "momentary"), stringsAsFactors = FALSE)
  key <- function(x) paste(x$timescale, x$feature, x$context, sep = ":")
  align_rows <- function(x) {
    stopifnot(nrow(x) == 16, !anyDuplicated(key(x)), setequal(key(x), key(rows)))
    x[match(key(rows), key(x)), , drop = FALSE]
  }
  equivalence <- align_rows(equivalence)
  stopifnot(nrow(power) == 32, all(power$true_beta %in% c(-0.10, 0.10)))
  negative <- align_rows(power[power$true_beta == -0.10, ])
  positive <- align_rows(power[power$true_beta == 0.10, ])
  stopifnot(
    all(is.finite(as.matrix(equivalence[c("beta_within", "ci90_lower", "ci90_upper", "p_TOST_Holm")]))),
    all(equivalence$ci90_lower <= equivalence$beta_within),
    all(equivalence$ci90_upper >= equivalence$beta_within),
    all(is.finite(c(negative$power_Holm, positive$power_Holm))),
    all(c(negative$power_Holm, positive$power_Holm, equivalence$p_TOST_Holm) >= 0),
    all(c(negative$power_Holm, positive$power_Holm, equivalence$p_TOST_Holm) <= 1)
  )

  # Two decimal places for estimates and percentages; three for p values.
  # Avoid displaying rounded negative zero or a p value of zero.
  fmt_beta <- function(x) {
    x <- round(x, 2)
    x[x == 0] <- 0
    sub("^-", "−", formatC(x, format = "f", digits = 2))
  }
  table_s10 <- data.frame(
    Timescale = ifelse(rows$timescale == "daily", "Daily", "Momentary"),
    `Language marker` = unname(labels[rows$feature]),
    Context = ifelse(rows$context == "private", "Private", "Public"),
    `β [90% CI]` = paste0(fmt_beta(equivalence$beta_within), " [",
                         fmt_beta(equivalence$ci90_lower), ", ",
                         fmt_beta(equivalence$ci90_upper), "]"),
    `p TOST, Holm` = ifelse(equivalence$p_TOST_Holm < 0.001, "< .001",
                           sub("^0", "", formatC(equivalence$p_TOST_Holm,
                                                   format = "f", digits = 3))),
    `Power (%), β = −.10` = formatC(100 * negative$power_Holm, format = "f", digits = 2),
    `Power (%), β = +.10` = formatC(100 * positive$power_Holm, format = "f", digits = 2),
    check.names = FALSE
  )
  output_stem <- file.path(results_dir, "table_s10_equivalence_and_detection_power")
  write.csv(table_s10, paste0(output_stem, ".csv"), row.names = FALSE, fileEncoding = "UTF-8")
  # Tab-separated version for copying cells or converting text to a Word table.
  write.table(table_s10, paste0(output_stem, ".tsv"), sep = "\t", quote = FALSE,
              row.names = FALSE, fileEncoding = "UTF-8")
  message("Combined Table S10 saved as CSV and TSV: ", output_stem)
  invisible(table_s10)
}
