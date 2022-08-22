# Internal helper functions

#' @importFrom purrr cross map transpose
#' @importFrom dplyr pull select filter last
#' @importFrom tidyr spread
#' @importFrom lme4 findbars lFormula
#' @importFrom stats model.frame model.matrix model.response
#' @importFrom rlang .data

# Wrap vector/array arguments in list and return a grid of settings
.args_to_grid <- function(model_method, control) {

  # Exceptions for vectors that are handled at a lower level
  if (!is.null(control$weights)) {
    control$weights <- list(control$weights)
  }
  if (!is.null(control$index_col)) {
    control$index_col <- list(control$index_col)
  }
  if (!is.null(control$lambda) & model_method %in% c("lasso", "enet", "ridge", "adalasso")) {
    control$lambda <- list(control$lambda)
  }
  if (!is.null(control$kappa) & model_method == "hfr") {
    control$kappa <- list(control$kappa)
  }

  control <- .func_to_list(control)
  grid <- purrr::cross(control)
  return(grid)

}

# Wrap functions in list
.func_to_list <- function(control) {

  control <- lapply(control, function(arg) {
    if (inherits(arg, c("function", "family"))) {
      list(arg)
    } else {
      arg
    }
  })

  return(control)

}

# Create a matrix of fitted values using coefficients and data
.fit_from_frame <- function(coefs_df, x, idx, idx_cols) {

  f <- coefs_df %>%
    dplyr::pull(.data$family) %>%
    unique %>%
    .[[1]]

  is_multinomial <- "class" %in% colnames(coefs_df)

  if (is.null(idx_cols) & !is.null(idx)) {
    idx_cols <- names(idx[[1]])
  }

  if (!is.null(idx)) {
    unique_idx <- unique(idx)
    rows_selector <- coefs_df %>%
      dplyr::select(dplyr::all_of(idx_cols)) %>%
      purrr::transpose()
  }

  .predict_inner <- function(df, x) {
    beta <- df %>%
      dplyr::select(.data$grid_id, .data$beta, .data$variable) %>%
      tidyr::spread(.data$grid_id, .data$beta)
    x <- x[, beta$variable]
    beta <- beta[, -1]
    fitted_values <- x %*% data.matrix(beta)
    return(fitted_values)
  }

  if (!is_multinomial) {

    if (!is.null(idx)) {

      fitted_values <- unique_idx %>%
        purrr::map_dfr(function(i) {
          which_rows_x <- sapply(idx, function(j) all(j %in% i))
          which_rows_beta <- sapply(rows_selector, function(j) all(j %in% i))
          if (!any(which_rows_x)) return(NULL)
          coefs_df %>%
            dplyr::filter(which_rows_beta) %>%
            .predict_inner(x[which_rows_x,]) %>%
            dplyr::as_tibble() %>%
            dplyr::mutate(row_n = c(1:nrow(x))[which_rows_x])
        })
      fitted_values <- fitted_values %>%
        dplyr::arrange(.data$row_n) %>%
        dplyr::select(-.data$row_n) %>%
        data.frame()

    } else {

      fitted_values <- coefs_df %>%
        .predict_inner(x)

    }

    fitted_values <- f$linkinv(fitted_values)
    fitted_values <- list(fitted_values)

  } else {

    class_vals <- unique(coefs_df$class)

    fitted_values_list <- class_vals %>%
      purrr::map(function(cls) {
        if (!is.null(idx)) {
          fitted_values <- unique_idx %>%
            purrr::map_dfr(function(i) {
              which_rows_x <- sapply(idx, function(j) all(j %in% i))
              which_rows_beta <- sapply(rows_selector, function(j) all(j %in% i))
              if (!any(which_rows_x)) return(NULL)
              coefs_df %>%
                dplyr::filter(which_rows_beta) %>%
                dplyr::filter(class == cls) %>%
                .predict_inner(x[which_rows_x,]) %>%
                dplyr::as_tibble() %>%
                dplyr::mutate(row_n = c(1:nrow(x))[which_rows_x])
            })

          fitted_values <- fitted_values %>%
            dplyr::arrange(.data$row_n) %>%
            dplyr::select(-.data$row_n) %>%
            data.frame()

        } else {

          fitted_values <- coefs_df %>%
            dplyr::filter(class == cls) %>%
            .predict_inner(x)

        }

        fitted_values <- exp(fitted_values)

        return(fitted_values)

      })
    fitted_values_sum <- fitted_values_list[[1]]
    for (i in 2:length(class_vals)) {
      fitted_values_sum <- fitted_values_sum + fitted_values_list[[i]]
    }
    fitted_values <- list()
    for (i in 1:length(class_vals)) {
      fitted_values[[i]] <- fitted_values_list[[i]] / fitted_values_sum
    }
    names(fitted_values) <- class_vals
  }

  return(fitted_values)

}

# Extract index vector from data
.get_index_from_df <- function(.data, .index, formula) {

  # Check if formula contains random effects
  re_terms <- lme4::findbars(formula)
  if (!is.null(.index) & length(re_terms) > 0)
    stop("do not supply an '.index' argument when using random effects")

  if (!is.null(.index)) {
    idx <- .data %>%
      dplyr::pull(!!.index)
  }
  if (is.null(.index) & length(re_terms) == 0) {
    idx <- NULL
  }
  if (length(re_terms) > 0) {
    idx_names <- sapply(re_terms, function(re) dplyr::last(all.vars(re)))
    idx <- .data %>%
      dplyr::select(dplyr::all_of(idx_names)) %>%
      dplyr::mutate_if(is.factor, as.character) %>%
      purrr::transpose()
  }

  return(idx)

}

# Returns model.frame using formula
.model_frame <- function(formula, data) {

  # Check if formula contains random effects
  re_terms <- lme4::findbars(formula)
  has_re <- length(re_terms) > 0

  if (!has_re) {
    mat <- stats::model.frame(formula, data)
  } else {
    model_terms <- lme4::lFormula(formula, data)
    mat <- model_terms$fr
  }

  colnames(mat) <- gsub("`", "", colnames(mat))

  return(mat)

}

# Returns model.matrix using formula
.model_matrix <- function(formula, data) {

  # Check if formula contains random effects
  re_terms <- lme4::findbars(formula)
  has_re <- length(re_terms) > 0

  if (!has_re) {
    mat <- stats::model.matrix(formula, data)
  } else {
    model_terms <- lme4::lFormula(formula, data)
    mat <- model_terms$X
  }

  colnames(mat) <- gsub("`", "", colnames(mat))

  return(mat)

}

# Returns model.response using model.frame
.model_response <- function(data) {

  mat <- data[, 1]

  return(mat)

}
