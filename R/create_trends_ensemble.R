#' Generate predictions for the trends ensemble, a quantile median of component
#' baseline models
#'
#' @param component_variations a `data.frame` where each row specifies a set of
#'   hyperparameters to use for a single baseline model fit, with columns
#'   `transformation`, `symmetrize`, `window_size`, and `temporal_resolution`.
#'   See details for more information
#' @param target_ts a `data.frame` of target data in a time series format
#'   (contains columns `time_index`, `location`, and `observation`) for a single
#'   location
#' @param reference_date string of the reference date for the forecasts, i.e.
#'   the date relative to which the targets are defined (usually Saturday for
#'   weekly targets). Must be in the ymd format, with yyyy-mm-dd format recommended.
#' @param horizons numeric vector of prediction horizons relative to
#'   the reference_date, e.g. 0:3 or 1:4, and interpreted to be in terms of the
#'   same temporal resolution as the provided `target_ts`.
#' @param target character string specifying the name of the prediction target
#' @param n_sim integer number of simulations to predict. Defaults to 100000.
#' @param quantile_levels numeric vector of quantile levels to output; set to NULL
#'   if quantile outputs not requested. Defaults to NULL.
#' @param n_samples integer of amount of samples to output (and predict);
#'   set to NULL if sample outputs not requested (in this case 100000 samples
#'   are generated from which to extract quantiles). Defaults to NULL.
#' @param round_predictions boolean specifying whether to round the output
#'   predictions to the nearest whole number. Defaults to FALSE
#' @param seed integer specifying a seed to set for reproducible results.
#'   Defaults to NULL, in which case no seed is set.
#' @param return_baseline_predictions boolean specifying whether to the component
#'   baseline models' forecasts in addition to the trends ensemble forecasts.
#'   If TRUE, a two-item list will be returned containing a labeled model_out_tbl
#'   of each. Defaults to FALSE.
#'
#' @details The `component_variations` data frame has the following columns and
#'   possible values for each:
#'   - transformation (character): "none" or "sqrt", determines distribution shape
#'   - symmetrize (boolean), determines if distribution is symmetric
#'   - window_size (integer), determines how many previous observations inform
#'     the forecast
#'   - temporal_resolution (character): "daily" or "weekly"
#'
#' Note that it must be possible to aggregate the `target_ts` data to the
#' temporal resolution values given in `component_variations`. For example, if
#' `target_ts` contains weekly observations but `component_variations` requests
#' models with a "daily" temporal resolution, an error will be thrown
#'
#' @return `model_out_tbl` of trends ensemble forecasts with columns:
#'   `model_id`, `reference_date`, `location`, `horizon`, `target`,
#'   `target_end_date`, `output_type`, `output_type_id`, and `value`.
#'
#' @importFrom rlang .data
#'
#' @export
create_trends_ensemble <- function(component_variations,
                                   target_ts,
                                   reference_date,
                                   horizons,
                                   target,
                                   n_sim = 10000,
                                   quantile_levels,
                                   n_samples = NULL,
                                   round_predictions = FALSE,
                                   seed = NULL,
                                   return_baseline_predictions = FALSE) {
  cv_col <- c("transformation", "symmetrize", "window_size", "temporal_resolution")
  validate_colnames(component_variations, cv_col, "component_variations")

  valid_temp_res <- c("daily", "weekly")
  temp_res_variations <- component_variations |>
    dplyr::distinct(name = .data[["temporal_resolution"]], .keep_all = FALSE) |>
    dplyr::mutate(num_days = dplyr::case_when(
      .data[["name"]] == "daily" ~ 1,
      .data[["name"]] == "weekly" ~ 7,
      .default = NA
    ))
  if (!all(temp_res_variations$name %in% valid_temp_res)) {
    cli::cli_abort("{.arg component_variations} must only include temporal resolution values {.val valid_temp_res}")
  }
  if (nrow(temp_res_variations) > 1) {
    cli::cli_abort("Currently {.arg component_variations} may only contain one unique temporal resolution value")
  }

  validate_target_ts(target_ts)
  ts_dates_desc <- sort(unique(target_ts$time_index), decreasing = TRUE)
  ts_temp_res <- as.integer(ts_dates_desc[1] - ts_dates_desc[2])
  if (any(temp_res_variations$num_days %% ts_temp_res != 0)) {
    cli::cli_abort(c(
      x = "Cannot match temporal resolution of provided {.arg target_ts}
          to those requested in {.arg component_variations}.",
      i = "{.arg target_ts} must aggregate to all requested temporal resolutions."
    ))
  }

  # calculate baseline models' forecasts
  split_variations <- component_variations |>
    split(f = component_variations$temporal_resolution)
  component_outputs <- split_variations |>
    purrr::map(.f = function(model_variations) {
      current_temp_res <- temp_res_variations[temp_res_variations$name == model_variations$temporal_resolution[1], ]
      if (current_temp_res$num_days > ts_temp_res) {
        new_horizon_min <- floor(min(horizons) / current_temp_res$num_days)
        new_horizon_max <- ceiling(max(horizons) / current_temp_res$num_days)
        model_variations |>
          dplyr::select(-"temporal_resolution") |>
          fit_baseline_models(aggregate_daily_to_weekly(target_ts),
                              reference_date,
                              current_temp_res$name,
                              new_horizon_min:new_horizon_max,
                              target,
                              n_sim = n_sim,
                              quantile_levels,
                              n_samples,
                              round_predictions,
                              seed)
      } else {
        model_variations |>
          dplyr::select(-"temporal_resolution") |>
          fit_baseline_models(target_ts,
                              reference_date,
                              current_temp_res$name[1],
                              horizons,
                              target,
                              n_sim = n_sim,
                              quantile_levels,
                              n_samples,
                              round_predictions,
                              seed)
      }
    }) |>
    purrr::list_rbind()

  # build ensemble
  split_components <- split(component_outputs,
    f = component_outputs$output_type
  )
  ensemble_outputs <- split_components |>
    purrr::map(.f = function(split_outputs) {
      type <- split_outputs$output_type[1]
      if (type == "quantile") {
        hubEnsembles::simple_ensemble(
          split_outputs,
          agg_fun = "median",
          model_id = "UMass-trends_ensemble"
        )
      } else if (type == "sample") {
        set.seed(seed)
        hubEnsembles::linear_pool(
          split_outputs,
          model_id = "UMass-trends_ensemble",
          compound_taskid_set = c("location", "reference_date", "target"),
          derived_task_ids = "target_end_date",
          n_output_samples = n_samples
        )
      }
    }) |>
    purrr::list_rbind() |>
    dplyr::mutate(
      reference_date = as.Date(reference_date),
      target_end_date = as.Date(.data[["target_end_date"]])
    )

  if (return_baseline_predictions == TRUE) {
    list(ensemble = ensemble_outputs, baselines = component_outputs)
  } else {
    ensemble_outputs
  }
}
