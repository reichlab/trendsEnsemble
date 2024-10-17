# set up simple data for test cases
model_variations <- data.frame(stringsAsFactors = FALSE,
                               transformation = c("none", "none"),
                               symmetrize = c(TRUE, TRUE),
                               window_size = c(3, 4))
target_ts <- data.frame(stringsAsFactors = FALSE,
                        location = rep("ak", 10),
                        time_index = as.Date("2022-11-05") + seq(0, 63, 7),
                        observation = c(15, 14, 38, 69, 53, 73, 51, 43, 43, 32))

# Invalid/misformatted target data throws an error
test_that("multiple locations in target data throws an error", {
  fit_baselines_one_location(model_variations,
                             target_ts |>
                               dplyr::mutate(location = "ak2") |>
                               dplyr::bind_rows(target_ts),
                             reference_date = "2023-01-14",
                             temporal_resolution = "weekly",
                             horizons = 0:3,
                             quantile_levels = c(.1, .5, .9),
                             n_samples = NULL) |>
    expect_error(
      regexp = " but only one may be provided.", fixed = TRUE
    )
})

test_that("invalid temporal resolution throws an error", {
  fit_baselines_one_location(model_variations,
                             target_ts,
                             reference_date = "2023-01-14",
                             temporal_resolution = "monthly",
                             horizons = 0:3,
                             quantile_levels = c(.1, .5, .9),
                             n_samples = NULL) |>
    expect_error(
      regexp = "`temporal_resolution` must be only one of ", fixed = TRUE
    )
})

test_that("provided temporal_resolution not matching that of target_ts throws an error", {
  fit_baselines_one_location(model_variations,
                             target_ts,
                             reference_date = "2023-01-14",
                             temporal_resolution = "daily",
                             horizons = 0:3,
                             quantile_levels = c(.1, .5, .9),
                             n_samples = NULL) |>
    expect_error(
      regexp = "The provided `temporal_resolution` does not match that of the provided `target_ts`",
      fixed = TRUE
    )
})


test_that(
  "predictions are forecasted starting from the last observed value;
  those not requested by the user are removed", {
    expected_outputs <- model_variations |>
      fit_baselines_one_location(
        target_ts,
        reference_date = "2023-01-14",
        temporal_resolution = "weekly",
        horizons = 0:4,
        quantile_levels = c(.1, .5, .9),
        n_samples = NULL,
        seed = 1234
      ) |>
      dplyr::mutate(
        reference_date = as.Date("2023-01-21"),
        horizon = .data[["horizon"]] - 1
      ) |>
      dplyr::filter(.data[["horizon"]] %in% 0:3)
    actual_outputs <- model_variations |>
      fit_baselines_one_location(
        target_ts,
        reference_date = "2023-01-21",
        temporal_resolution = "weekly",
        horizons = 0:3,
        quantile_levels = c(.1, .5, .9),
        n_samples = NULL,
        seed = 1234
      )
    expect_equal(actual_outputs, expected_outputs, tolerance = 1e-3)
  }
)

test_that("overlapping forecasts are replaced with observed values and throws a warning", {
  expected_outputs <-
    rbind(
      expand.grid(
        stringsAsFactors = FALSE,
        transformation = "none", symmetrize = TRUE, window_size = c(3, 4),
        location = "ak", reference_date = as.Date("2023-01-07"),
        horizon = 0, target_end_date = as.Date("2023-01-07"),
        output_type = "quantile", output_type_id = c(0.1, 0.5, 0.9), value = 32
      ),
      fit_baselines_one_location(
        model_variations,
        target_ts,
        reference_date = "2023-01-14",
        temporal_resolution = "weekly",
        horizons = 0:2,
        quantile_levels = c(.1, .5, .9),
        n_samples = NULL,
        seed = 1234
      ) |>
        dplyr::mutate(
          reference_date = reference_date - 7,
          horizon = horizon + 1
        )
    ) |>
    dplyr::arrange(transformation, symmetrize, window_size) |>
    dplyr::tibble()
  attr(expected_outputs, "out.attrs") <- NULL
  expect_warning(
    actual_outputs <- model_variations |>
      fit_baselines_one_location(
        target_ts,
        reference_date = "2023-01-07",
        temporal_resolution = "weekly",
        horizons = 0:3,
        quantile_levels = c(.1, .5, .9),
        n_samples = NULL,
        seed = 1234
      ),
    regexp = "forecasts requested for a time index within the provided `target_ts`, replacing overlapping forecasts",
    fixed = TRUE
  )
  expect_equal(actual_outputs, expected_outputs, tolerance = 1e-3)
})
