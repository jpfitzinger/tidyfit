#' @importFrom crayon bold italic red green yellow
#' @importFrom stats as.formula setNames

# R6 class for model object
model_definition <- R6::R6Class(
  "tidyFit",
  public = list(
    method = NULL,
    original_formula = NULL,
    formula = NULL,
    data = NULL,
    args = NULL,
    cv = NULL,
    has_predict_method = NULL,
    object = NULL,
    estimator = NULL,
    fit_info = NULL,
    names_map = NULL,
    force_syntactic_names = FALSE,
    error = NULL,
    warnings = NULL,
    messages = NULL,
    grid_id = NULL,
    inner_grid = NULL,
    mode = NULL,

    initialize = function(method, formula, settings, grid_id) {
      .check_method(method, "exists", TRUE)
      self$method <- method
      self$original_formula <- formula
      if (!is.null(formula)) self$formula <- .prepare_formula(formula)
      self$args <- settings
      self$grid_id <- grid_id
      self$cv <- .check_method(method, "cv")
      self$has_predict_method <- .check_method(method, "has_predict_method")
      self$mode <- "regression"
    },
    fit = function(data = NULL, ...) {
      class(self) <- c(class(self), self$method)
      if (is.null(self$formula)) self$formula <- .prepare_formula(self$original_formula)
      self$data <- data
      data <- .prepare_data(self, data, TRUE)
      .fit(self, data, ...)
    },
    predict = function(data, ...) {
      if (!self$has_predict_method) {
        warning(paste0("No prediction method for type '", self$method, "'."))
        return(NULL)
      }
      all_args <- list(object = self$object,
                       data = .prepare_data(self, data),
                       self = self)
      all_args <- append(all_args, list(...))
      do.call(.predict, all_args)
    },
    coef = function(...) {
      if (!.check_method(self$method, "has_coef_method")) {
        warning(paste0("No coef method for type '", self$method, "'. Try using 'explain()'"))
        return(tibble(term=character(), estimate=numeric()))
      }
      all_args <- list(object = self$object, self = self)
      coef_df <- do.call(.coef, all_args)
      coef_df <- coef_df %>%
        dplyr::mutate(term = dplyr::if_else(.data$term %in% names(self$names_map), self$names_map[.data$term], .data$term))
      return(coef_df)
    },
    resid = function(...) {
      all_args <- list(object = self$object, self = self)
      do.call(.resid, all_args)
    },
    fitted = function(...) {
      all_args <- list(object = self$object, self = self)
      do.call(.fitted, all_args)
    },
    explain = function(use_package, use_method, additional_args) {
      if (!.check_method(self$method, "has_importance_method")) {
        warning(paste0("No explain method for type '", self$method, "'."))
        return(dplyr::tibble(term = character(), importance = double()))
      }
      all_args <- list(object = self$object, self = self, use_package = use_package, use_method = use_method)
      all_args <- append(all_args, additional_args)
      var_imp_df <- do.call(.explain, all_args)
      if (nrow(var_imp_df) > 0) {
        var_imp_df <- var_imp_df %>%
          dplyr::mutate(term = dplyr::if_else(.data$term %in% names(self$names_map), self$names_map[.data$term], .data$term))
      }

      return(var_imp_df)
    },
    print = function(...) {
      cat("<tidyFit> object\n", crayon::italic("method:"),
          crayon::bold(self$method), "|",
          crayon::italic("mode:"), crayon::bold(self$mode), "|",
          crayon::italic("fitted:"), crayon::bold(ifelse(is.null(self$object), "no", "yes")), "\n",
          ifelse(is.null(self$error), crayon::green("no errors \u2714"), crayon::red("check errors \u2716")), "|",
          ifelse(is.null(self$warnings), crayon::green("no warnings \u2714"), crayon::yellow("check warnings \u2716")))
    },
    set_args = function(..., overwrite = TRUE) {
      new_args <- lapply(list(...), unlist)
      if (overwrite) {
        self$args <- append(
          self$args[!names(self$args) %in% names(new_args)],
          new_args
        )
      } else {
        self$args <- append(
          self$args,
          new_args[!names(new_args) %in% names(self$args)]
        )
      }
      invisible(self)
    },
    clear = function(...) {
      self$object = NULL
      self$error = NULL
      self$warnings = NULL
      self$messages = NULL
      self$fit_info = NULL
      self$inner_grid = NULL
      invisible(self)
    }
  ),
  private = list(
    fit_ = NULL
  )
)

# Capture errors, warnings and messages from purrr safely/quietly function
.store_on_self <- function(self, model) {
  self$object <- model$result$result
  self$error <- model$error[[1]]
  if (length(model$result$messages)>0) self$messages <- paste(model$result$messages, collapse = " | ")
  if (length(model$result$warnings)>0) self$warnings <- paste(model$result$warnings, collapse = " | ")
  invisible(self)
}

.prepare_data <- function(self, data, write_names_map = FALSE) {
  # method to store names mapping and convert names to syntactic terms
  prepared_data <- data
  var_names <- colnames(data)
  syn_var_names <- make.names(var_names)
  colnames(prepared_data) <- syn_var_names
  names_map <- c(stats::setNames(var_names, syn_var_names))

  if (!.check_method(self$method, "nonstandard_formula")) {
    # add response variable if it is missing
    prepared_data_temp <- prepared_data
    response_var <- all.vars(self$original_formula)[1]
    if (!response_var %in% colnames(data)) {
      data[, response_var] = NA
      prepared_data_temp[, response_var] <- NA
    }

    model_mat <- stats::model.matrix(self$original_formula, data)
    prepared_model_mat <- stats::model.matrix(self$formula, prepared_data_temp)
    var_names_mm <- colnames(model_mat)
    prepared_var_names_mm <- colnames(prepared_model_mat)
    syn_var_names_mm <- make.names(prepared_var_names_mm)
    names_map <- c(names_map,
                   stats::setNames(var_names_mm, prepared_var_names_mm),
                   stats::setNames(var_names_mm, syn_var_names_mm))
  }
  if (write_names_map) {
    self$names_map <- names_map[!duplicated(names(names_map))]
  }
  return(prepared_data)
}

.prepare_formula <- function(formula) {
  # method to convert formula to syntactic terms
  var_names <- all.vars(formula)
  syn_var_names <- make.names(var_names)
  mapper <- stats::setNames(syn_var_names, var_names)
  mapper <- lapply(mapper, dplyr::sym)
  new_formula <- do.call("substitute", list(formula, mapper))
  new_formula <- stats::as.formula(new_formula, env = environment(formula))
  return(new_formula)
}
