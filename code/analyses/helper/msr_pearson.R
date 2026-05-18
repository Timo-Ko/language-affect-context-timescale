library(mlr3)
library(R6)

MeasureRegrPearson <- R6::R6Class(
  "MeasureRegrPearson",
  inherit = mlr3::MeasureRegr,
  
  public = list(
    initialize = function() {
      super$initialize(
        id = "pearson",
        packages = character(),
        predict_type = "response",
        properties = character(),
        range = c(-1, 1),
        minimize = FALSE
      )
    }
  ),
  
  private = list(
    .score = function(prediction, ...) {
      truth <- prediction$truth
      response <- prediction$response
      
      ok <- is.finite(truth) & is.finite(response)
      truth <- truth[ok]
      response <- response[ok]
      
      if (length(truth) < 3L) {
        return(NA_real_)
      }
      
      if (sd(truth) == 0 || sd(response) == 0) {
        return(NA_real_)
      }
      
      unname(cor(truth, response, method = "pearson"))
    }
  )
)

msr_pearson <- MeasureRegrPearson$new()