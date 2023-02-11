.varimp <- function(object, ...) {
  UseMethod(".varimp")
}


.varimp.default <- function(object, self = NULL, prediction_type = "prob", ...) {
  response_var <- all.vars(self$formula)[1]
  data <- object$call$data
  data <- data.frame(data)
  mf <- stats::model.frame(self$formula, data)
  
  if (self$mode == "regression") {
    mod <- iml::Predictor$new(object, data = mf |> dplyr::select(-all_of(response_var)),
                              y = mf[[response_var]])
    imp <- iml::FeatureImp$new(mod, loss = "mae")
    estimates <- imp$results |>
      dplyr::select(term = "feature",
                    estimate = "importance") |>
      dplyr::as_tibble()
  }
  
  if (self$mode == "classification") {
    # separately calculate permutation feature importance for each class
    estimates <- purrr::map_dfr(unique(mf[[response_var]]),
                                function(cls) {
                                  
                                  mf_class <- mf |> dplyr::filter(!!dplyr::sym(response_var) == cls)
                                  mod <- iml::Predictor$new(object, data = mf_class |> dplyr::select(-all_of(response_var)),
                                                            y = mf_class[[response_var]], type = prediction_type)
                                  imp <- iml::FeatureImp$new(mod, loss = "ce", compare = "difference")
                                  imp$results |>
                                    dplyr::select(term = "feature",
                                                  estimate = "importance") |>
                                    dplyr::mutate(class = cls) |>
                                    dplyr::as_tibble()
                                })
    
  }
  
  return(estimates)
}