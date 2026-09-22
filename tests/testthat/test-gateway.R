test_that("gateway dispatches func='model_run' and returns JSON", {
  js <- gateway(func = "model_run", model_input = get_default_input())
  expect_type(js, "character")
  parsed <- jsonlite::fromJSON(js)
  expect_equal(nrow(parsed), 3L)
  expect_setequal(parsed$model, c("core", "extended", "lab"))
})

test_that("gateway defaults func to model_run and strips control fields", {
  js <- gateway(model_input = get_default_input(), api_key = "x",
                session_id = "y", execution_id = "z",
                callback_url = "https://example.invalid/cb")
  expect_equal(jsonlite::fromJSON(js)$mortality,
               model_run(get_default_input())$mortality, tolerance = 1e-12)
})

test_that("gateway with no input falls back to the default patient", {
  expect_equal(nrow(jsonlite::fromJSON(gateway())), 3L)
})

test_that("gateway preserves full numeric precision", {
  # jsonlite::toJSON() rounds to 4 significant digits by default, which would
  # turn 0.0389405554475 into 0.03894 and lose the rest.
  parsed <- jsonlite::fromJSON(gateway(func = "model_run",
                                       model_input = get_default_input()))
  expect_equal(parsed$mortality[parsed$model == "lab"],
               0.0389405554475, tolerance = 1e-9)
})

test_that("gateway handles no-arg dispatch to the helpers", {
  expect_true("age" %in% names(jsonlite::fromJSON(gateway(func = "get_default_input"))))
  expect_true("points" %in% names(jsonlite::fromJSON(gateway(func = "score_chart"))))
})
