#' @importFrom crayon bold italic red green yellow

# R6 class for model object
model_definition <- R6::R6Class(
  "tidyFit",
  public = list(
    method = NULL,
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
      self$formula <- formula
      self$args <- settings
      self$grid_id <- grid_id
      self$cv <- .check_method(method, "cv")
      self$has_predict_method <- .check_method(method, "has_predict_method")
      self$mode <- "regression"
    },
    fit = function(data = NULL, ...) {
      class(self) <- c(class(self), self$method)
      self$data <- data
      self$names_map <- .get_names_map_from_data(formula = self$formula, data = data, method = self$method)
      .fit(self, data, ...)
      },
    predict = function(data, ...) {
      if (!self$has_predict_method) {
        warning(paste0("No prediction method for type '", self$method, "'."))
        return(NULL)
      }
      all_args <- list(object = self$object, data = data, self = self)
      all_args <- append(all_args, list(...))
      do.call(.predict, all_args)
    },
    coef = function(...) {
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
