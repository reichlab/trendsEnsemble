#' Generate predictions for all baseline models, for a single location
#'
#' @param model_variations a `data.frame` where each row specifies a set of
#'   hyperparameters to use for a single baseline model fit, with columns
#'   `transformation`, `symmetrize`, and `window_size`. See details for more
#'   information
#' @param target_ts a `data.frame` of target data in a time series format
#'   (contains columns `time_index`, `location`, and `observation`) for a single
#'   location
#' @param reference_date string of the reference date for the forecasts, i.e.
#'   the date relative to which the targets are defined (usually Saturday for
#'   weekly targets)
#' @param temporal_resolution 'daily' or 'weekly'; specifies timescale of
#'   `target_ts` and `horizons`
#' @param horizons numeric vector of prediction horizons relative to
#'   the reference_date, e.g. 0:3 or 1:4
#' @param quantile_levels numeric vector of quantile levels to output; set to NULL
#'   if quantile outputs not requested. Defaults to NULL.
#' @param n_samples integer of amount of samples to output (and predict);
#'   set to NULL if sample outputs not requested (in this case 100000 samples
#'   are generated from which to extract quantiles). Defaults to NULL.
#' @param round_predictions boolean specifying whether to round the output
#'   predictions to the nearest whole number. Defaults to FALSE
#' @param seed integer specifying a seed to set for reproducible results.
#'   Defaults to NULL, in which case no seed is set.
#'
#' @details The `model_variations` data frame has the following columns and
#'   possible values for each:
#'   - transformation (character): "none" or "sqrt", determines distribution shape
#'   - symmetrize (boolean), determines if distribution is symmetric
#'   - window_size (integer), determines how many previous observations inform
#'     the forecast
#'
#' Additionally, this function will return slightly different output forecasts
#' depending on the relationship between the `reference_date`, requested `horizons`,
#' and dates contained within `target_ts`. There are three possible cases:
#'   1. The requested forecasts begin exactly one time unit (given by the
#'      temporal_resolution) after the last observed value. Here, no changes are
#'      made to the returned forecasts.
#'   2. The requested forecasts begin two or more time units after the last
#'      observed value. Here, we make predictions starting from the next time unit
#'      after the last observed value until the last requested date, returning
#'      only forecasts for the requested dates.
#'   3. The dates for the requested forecasts overlap partially or completely with
#'      observed values contained within `target_ts`. Here, any forecasted values
#'      for overlapping dates are replaced by the associated observed values.
#'      (Note that we warn for this case.)
#'
#' @return data frame of a baseline forecast for one location, all models with
#'   columns `transformation`, `symmetrize`, `window_size`, `horizon`,
#'   `output_type`, `output_type_id`, and `value`
#'
#' @importFrom rlang .data
#'
fit_baselines_one_location <- function(model_variations,
                                       target_ts,
                                       reference_date,
                                       temporal_resolution,
                                       horizons,
                                       quantile_levels,
                                       n_samples,
                                       round_predictions = FALSE,
                                       seed = NULL) {

  valid_temp_res <- c("daily", "weekly")
  if (!(temporal_resolution %in% valid_temp_res && length(temporal_resolution)) == 1) {
    cli::cli_abort("{.arg temporal_resolution} must be only one of {.val valid_temp_res}")
  }
  temporal_resolution <- match.arg(temporal_resolution, valid_temp_res)

  validate_target_ts(target_ts)
  ts_dates_desc <- sort(unique(target_ts$time_index), decreasing = TRUE)
  ts_temp_res <- as.integer(ts_dates_desc[1] - ts_dates_desc[2])
  if ((ts_temp_res == 1 && temporal_resolution != "daily") || (ts_temp_res == 7 && temporal_resolution != "weekly")) {
    cli::cli_abort("The provided {.arg temporal_resolution} does not match that of the provided {.arg target_ts}")
  }

  # figure out horizons to forecast
  reference_date <- lubridate::ymd(reference_date) # date to which horizons are relative
  last_data_date <- max(target_ts$time_index) # last day of target data
  actual_target_dates <- reference_date + ts_temp_res * horizons
  effective_horizons <- as.integer(actual_target_dates - last_data_date) / ts_temp_res
  horizons_to_forecast <- 1:max(effective_horizons)
  h_adjustments <- min(effective_horizons) - 1

  # get predictions for all model_variations
  predictions <- purrr::pmap_dfr( #tibble, each 1x1 row contains predictions for 1 model
    model_variations,
    get_baseline_predictions,
    target_ts = target_ts,
    effective_horizons = horizons_to_forecast,
    origin = ifelse(temporal_resolution == "weekly", "obs", "median"),
    n_sim = 100000,
    quantile_levels = quantile_levels,
    n_samples = n_samples,
    round_predictions = round_predictions,
    seed = seed
  )

  # extract forecasts
  extracted_outputs <-
    dplyr::bind_cols(model_variations, predictions) |>
    tidyr::unnest(cols = "forecasts") |>
    dplyr::mutate(
      location = target_ts$location[1], # note, target_ts contains data for just one location
      target_end_date = last_data_date + ts_temp_res * .data[["horizon"]],
      .before = "horizon"
    )

  if (h_adjustments == 0) { # min(actual_target_dates) occurs period after last observed value
    model_outputs <- extracted_outputs |>
      dplyr::mutate(
        reference_date = reference_date,
        horizon = as.numeric((.data[["target_end_date"]] - as.Date(reference_date)) / ts_temp_res),
        .before = "horizon"
      )
  } else if (h_adjustments > 0) { # all(effective_horizons) >= 2
    # here extracted_outputs contains extra forecasts the user did not request
    # (since we predict forward from the last observed value in target_ts),
    # so we drop the unnecessary ones before returning the data frame
    model_outputs <- extracted_outputs |>
      dplyr::filter(.data[["horizon"]] %in% effective_horizons) |>
      dplyr::mutate(
        reference_date = reference_date,
        horizon = as.numeric((.data[["target_end_date"]] - as.Date(reference_date)) / ts_temp_res),
        .before = "horizon"
      )
  } else if (h_adjustments < 0) {
    # here extracted_outputs only contains forecasts not replaced by observed values
    # `complete()` adds the remaining rows to avoid unnecessary computations and
    # `mutate(value = ...)` adds observed values for rows we didn't forecast
    model_outputs <- extracted_outputs |>
      dplyr::group_by(dplyr::across(c("transformation", "symmetrize", "window_size", "location", "output_type"))) |>
      tidyr::complete(target_end_date = actual_target_dates, .data[["output_type_id"]]) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        reference_date = reference_date,
        horizon = as.numeric((.data[["target_end_date"]] - as.Date(reference_date)) / ts_temp_res),
        .before = "horizon"
      ) |>
      dplyr::left_join(
        dplyr::filter(target_ts, .data[["time_index"]] %in% actual_target_dates),
        by = c("location", "target_end_date" = "time_index")
      ) |>
      dplyr::mutate(
        value = ifelse(is.na(.data[["value"]]), .data[["observation"]], .data[["value"]])
      )
    if (max(actual_target_dates) <= last_data_date) {
      cli::cli_warn(
        "all requested forecasts are for a time index within the provided {.arg target_ts},
          replacing overlapping forecasts with {.val {length(horizons)}} target observations"
      )
    } else {
      cli::cli_warn(
        "forecasts requested for a time index within the provided {.arg target_ts},
          replacing overlapping forecasts with {.val {abs(h_adjustments)}} target observations"
      )
    }
  }

  model_outputs |>
    dplyr::select(dplyr::all_of(c(
      "transformation", "symmetrize", "window_size", "location", "reference_date",
      "horizon", "target_end_date", "output_type", "output_type_id", "value"
    )))
}
