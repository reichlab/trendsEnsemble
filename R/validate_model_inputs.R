#' Perform simple validations on the model variations dataframe used to define a
#' given baseline model
#'
#' @param model_variations a `data.frame` where each row specifies a set of
#'   hyperparameters to use for a single baseline model fit, with the following
#'   columns: `transformation`, `symmetrize`, and `window_size`. See the details
#'   for more information.
#' @details The types, and possible values for each of the columns in
#' `model_variations` are as follows:
#'   - transformation (character): "none" or "sqrt"
#'   - symmetric (boolean)
#'   - window_size (numeric): a non-negative integer
#' Additional validations that check each column for the correct type are performed
#' by `validate_variation_inputs()`, which will always be called following a call to
#' `validate_model_inputs()`.
#'
#' @return no return value
#'
#' @noRd

validate_model_variations <- function(model_variations) {
  if (is.null(model_variations)) {
    cli::cli_abort("{.arg model_variations} is missing")
  }

  variation_col <- c("transformation", "symmetrize", "window_size")
  validate_colnames(model_variations, variation_col, "model_variations")

  if (any(duplicated(model_variations))) {
    cli::cli_abort("{.arg model_variations} contains duplicate rows.")
  }
}


#' Perform simple validations on the individual variables defining a single
#' baseline model
#'
#' @param transformation string specifying the transformation used on the
#'   distribution which determines its shape; can be one of "none" or "sqrt".
#' @param symmetrize boolean specifying whether to make the distribution symmetric;
#'   can be one of `TRUE` or `FALSE`.
#' @param window_size integer specifying how many previous observations in the
#'   target data should be used to inform the forecasts
#'
#' @return no return value
#'
#' @noRd
validate_variation_inputs <- function(transformation, symmetrize, window_size) {
  # check variation inputs have length 1
  if (length(transformation) != 1) {
    cli::cli_abort("{.arg transformation} must be length 1")
  }

  if (length(symmetrize) != 1) {
    cli::cli_abort("{.arg symmetrize} must be length 1")
  }

  if (length(window_size) != 1) {
    cli::cli_abort("{.arg window_size} must be length 1")
  }

  # check variation inputs contain only valid values
  valid_transformations <- c("none", "sqrt")
  if (!transformation %in% valid_transformations) {
    cli::cli_abort("{.arg transformation} must only contain values {.val {valid_transformations}}")
  }

  if (!inherits(symmetrize, "logical")) {
    cli::cli_abort("{.arg symmetrize} must only contain logical values, e.g. TRUE or FALSE.")
  }

  if (window_size != trunc(window_size) || window_size < 0) {
    cli::cli_abort("{.arg window_size} must only contain non-negative integer values.")
  }
}


#' Perform simple validations on the target data (time series) dataframe
#'
#' @param target_ts a `data.frame` of target data in a time series format
#'   (contains columns `time_index`, `location`, and `observation`)
#'
#' @return no return value
#'
#' @noRd
validate_target_ts <- function(target_ts) {
  target_col <- c("time_index", "location", "observation")
  validate_colnames(target_ts, target_col, "target_ts")

  if (any(duplicated(target_ts))) {
    cli::cli_abort("{.arg target_ts} contains duplicate rows.")
  }
}


#' Validate that a dataframe's columns are (named) as expected
#'
#' @param df a `data.frame` whose columns are to be validated
#' @param expected_col a character vector of expected column names
#' @param arg_name character string name of the argument being validated to be
#'   printed in the error message(generally the name of the `df` object)
#'
#' @return no return value
#'
#' @noRd
validate_colnames <- function(df, expected_col, arg_name) {
  actual_col <- colnames(df)
  if (!all(expected_col %in% actual_col)) {
    cli::cli_abort("{.arg {arg_name}} is missing the column{?s}: {.val {setdiff(expected_col, actual_col)}}.")
  }
  if (!all(actual_col %in% expected_col)) {
    cli::cli_abort(c(
      x = "{.arg {arg_name}} contains the extra column{?s}: {.val {setdiff(actual_col, expected_col)}}."
    ))
  }
}


#' Validate that an integer is as expected and non-negative
#'
#' @param int a single integer to be validated
#' @param arg_name character string name of the argument being validated to be
#'   printed in the error message(generally the name of the `int` object)
#'
#' @return no return value
#'
#' @noRd
validate_integer <- function(int, arg_name) {
  if (!is.numeric(int) || int < 0 || int != trunc(int) || length(int) != 1) {
    cli::cli_abort("{.arg {arg_name}} must be a single, non-negative integer value.")
  }
}


#' Validate value to be converted into a ymd date
#'
#' @param date value to be converted into a ymd date
#' @param arg_name character string name of the argument being validated to be
#'   printed in the error message (generally the name of the `date` object)
#'
#' @return a validated Date object (or vector) in the ymd format
#'
#' @noRd
validate_ymd_date <- function(date, arg_name) {
  if (is.null(date)) {
    cli::cli_abort("{.arg {arg_name}} is missing")
  }

  ymd_date <- lubridate::ymd(date, quiet = TRUE)
  if (is.na(ymd_date)) {
    cli::cli_abort("{.arg {arg_name}} could not be correctly parsed. Please use the ymd format")
  } else {
    ymd_date
  }
}
