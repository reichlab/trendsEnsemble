# set up simple data for test cases
model_variations <- data.frame(stringsAsFactors = FALSE,
                               transformation = c("none", "none"),
                               symmetrize = c(TRUE, TRUE),
                               window_size = c(3, 4))
target_ts <- expand.grid(stringsAsFactors = FALSE,
                         location = c("ak", "al"),
                         time_index = as.Date("2022-11-05") + seq(0, 63, 7),
                         observation = NA)
target_ts$observation[target_ts$location == "ak"] <- c(15, 14, 38, 69, 53, 73, 51, 43, 43, 32)
target_ts$observation[target_ts$location == "al"] <- c(350, 312, 236, 237, 360, 234, 153, 153, 148, 125)

test_that("missing target throws an error", {
  fit_baseline_models(model_variations,
                      target_ts,
                      reference_date = "2023-01-14",
                      temporal_resolution = "weekly",
                      horizons = 0:3,
                      target = NULL,
                      quantile_levels = c(.1, .5, .9),
                      n_samples = NULL) |>
    expect_error(
      regexp = "`target` is missing; please provide one", fixed = TRUE
    )
})

test_that("output predictions is a model_out_tbl", {
  baseline_outputs <- fit_baseline_models(model_variations,
                                          target_ts,
                                          reference_date = "2023-01-14",
                                          temporal_resolution = "weekly",
                                          horizons = 0:3,
                                          target = "inc hosp",
                                          quantile_levels = c(.1, .5, .9),
                                          n_samples = NULL)
  expect_s3_class(baseline_outputs, "model_out_tbl")
})

test_that("model IDs are as expected", {
  expected_model_ids <- paste("UMass-baseline", "none", "sym", c(3, 4), "weekly", sep = "_")
  actual_model_ids <- fit_baseline_models(model_variations,
                                          target_ts,
                                          reference_date = "2023-01-14",
                                          temporal_resolution = "weekly",
                                          horizons = 0:3,
                                          target = "inc hosp",
                                          quantile_levels = c(.1, .5, .9),
                                          n_samples = NULL) |>
    dplyr::pull(.data[["model_id"]]) |>
    unique()
  expect_equal(actual_model_ids, expected_model_ids)
})

test_that("mapping over locations works as expected", {
  expected_outputs <-
    rbind(
      fit_baselines_one_location(
        model_variations,
        target_ts |> dplyr::filter(.data[["location"]] == "ak"),
        reference_date = "2023-01-14",
        temporal_resolution = "weekly",
        horizons = 0:3,
        quantile_levels = c(.1, .5, .9),
        n_samples = NULL,
        seed = 1234
      ),
      fit_baselines_one_location(
        model_variations,
        target_ts |> dplyr::filter(.data[["location"]] == "al"),
        reference_date = "2023-01-14",
        temporal_resolution = "weekly",
        horizons = 0:3,
        quantile_levels = c(.1, .5, .9),
        n_samples = NULL,
        seed = 1234
      )
    ) |>
    dplyr::mutate(
      model_id = paste(
        "UMass-baseline",
        .data[["transformation"]],
        ifelse(.data[["symmetrize"]], "sym", "nonsym"),
        .data[["window_size"]],
        "weekly",
        sep = "_"
      ),
      target = "inc hosp",
      .before = 1
    ) |>
    dplyr::select(dplyr::all_of(c(
      "model_id", "location", "reference_date", "horizon", "target",
      "target_end_date", "output_type", "output_type_id", "value"
    ))) |>
    hubUtils::as_model_out_tbl()

  actual_outputs <- fit_baseline_models(model_variations,
                                        target_ts,
                                        reference_date = "2023-01-14",
                                        temporal_resolution = "weekly",
                                        horizons = 0:3,
                                        target = "inc hosp",
                                        quantile_levels = c(.1, .5, .9),
                                        n_samples = NULL,
                                        seed = 1234)
  expect_equal(actual_outputs, expected_outputs, tolerance = 1e-3)
})
