#' Generate predictions for all baseline models for the given reference date
#'
#' @inheritParams fit_baselines_one_location
#' @param target character string specifying the name of the prediction target
#'
#' @details The `model_variations` data frame has the following columns and
#'   possible values for each:
#'   - transformation (character): "none" or "sqrt", determines distribution shape
#'   - symmetrize (boolean), determines if distribution is symmetric
#'   - window_size (integer), determines how many previous observations inform
#'     the forecast
#'
#' @return `model_out_tbl` of forecasts for all baseline models with columns:
#'   `model_id`, `reference_date`, `location`, `horizon`, `target`,
#'   `target_end_date`, `output_type`, `output_type_id`, and `value`
#'
#' @importFrom rlang .data
#'
fit_baseline_models <- function(model_variations,
                                target_ts,
                                reference_date,
                                temporal_resolution,
                                horizons,
                                target,
                                quantile_levels,
                                n_samples,
                                round_predictions = FALSE,
                                seed = NULL) {
  if (is.null(target)) {
    cli::cli_abort("{.arg target} is missing; please provide one")
  }

  # fit baseline models
  #furrr::future_pmap_dfr(
  purrr::map(
    unique(target_ts$location),
    function(fips_code) {
      fit_baselines_one_location(
        model_variations = model_variations,
        target_ts = dplyr::filter(target_ts, .data[["location"]] == fips_code),
        reference_date = reference_date,
        temporal_resolution = temporal_resolution,
        horizons = horizons,
        quantile_levels = quantile_levels,
        n_samples = n_samples,
        round_predictions = round_predictions,
        seed = seed
      )
    }
  ) |>
    purrr::list_rbind() |>
    dplyr::mutate(
      model_id = paste(
        "UMass-baseline",
        .data[["transformation"]],
        ifelse(.data[["symmetrize"]], "sym", "nonsym"),
        .data[["window_size"]],
        temporal_resolution,
        sep = "_"
      ),
      target = target,
      .before = 1
    ) |>
    dplyr::select(dplyr::all_of(c(
      "model_id", "location", "reference_date", "horizon", "target",
      "target_end_date", "output_type", "output_type_id", "value"
    ))) |>
    hubUtils::as_model_out_tbl()
}
