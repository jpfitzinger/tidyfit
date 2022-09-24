#' @importFrom crayon bold italic red green yellow

# R6 class for model object
model_definition <- R6::R6Class(
  "tidyFit",
  public = list(
    method = NULL,
    formula = NULL,
    args = NULL,
    cv = NULL,
    object = NULL,
    estimator = NULL,
    fit_info = NULL,
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
      fitter <- get(paste0(".model.", method))
      private$fit_ <- fitter
      self$cv <- .check_method(method, "cv")
      self$mode <- "regression"
    },
    fit = function(...) {private$fit_(self, ...)},
    predict = function(data) {
      all_args <- append(append(
        list(object = self$object, data = data, formula = self$formula, inner_grid = self$inner_grid, mode = self$mode),
        self$args), self$fit_info
      )
      do.call(.predict, all_args)
    },
    coef = function(...) {
      all_args <- append(append(
        list(object = self$object, formula = self$formula, inner_grid = self$inner_grid, mode = self$mode),
        self$args), self$fit_info
      )
      do.call(.coef, all_args)
    },
    resid = function(...) {
      all_args <- append(append(
        list(object = self$object, formula = self$formula, inner_grid = self$inner_grid, mode = self$mode),
        self$args), self$fit_info
      )
      do.call(.resid, all_args)
    },
    fitted = function(...) {
      all_args <- append(append(
        list(object = self$object, formula = self$formula, inner_grid = self$inner_grid, mode = self$mode),
        self$args), self$fit_info
      )
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
  if (length(model$result$messages)>0) self$messages <- model$result$messages
  if (length(model$result$warnings)>0) self$warnings <- model$result$warnings
  invisible(self)
}
