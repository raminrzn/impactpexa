# Reference values were validated against the authors' own calculator at
# tbi-impact.org across a grid of 48 model/outcome combinations spanning eight
# patients. Every comparison agreed within 1.0 percentage point, which is the
# most that can be expected: the calculator reports integer percentages and the
# published coefficients are rounded to three decimals. The fixtures below are
# this implementation's full-precision values for those validated cases.

test_that("core model reproduces the calculator for the default patient", {
  r <- impact(age = 30, motor = "localizes", pupils = "both")
  expect_equal(nrow(r), 1L)              # only core is computable
  expect_equal(r$model, "core")
  expect_equal(r$mortality,    0.1101705691679, tolerance = 1e-9)  # oracle 11%
  expect_equal(r$unfavourable, 0.1818296956717, tolerance = 1e-9)  # oracle 18%
})

test_that("all three models compute when every predictor is supplied", {
  r <- impact(age = 30, motor = "localizes", pupils = "both",
              hypoxia = "no", hypotension = "no", ct_class = "II",
              tsah = "no", edh = "no", glucose = 6, hb = 14)
  expect_equal(r$model, c("core", "extended", "lab"))
  expect_equal(r$mortality,
               c(0.1101705691679, 0.0558824660558, 0.0389405554475),
               tolerance = 1e-9)
  expect_equal(r$unfavourable,
               c(0.1818296956717, 0.1157814395847, 0.0848657639549),
               tolerance = 1e-9)
})

test_that("a severe presentation reproduces the calculator", {
  r <- impact(age = 78, motor = "none", pupils = "none", hypoxia = "yes",
              hypotension = "yes", ct_class = "VI", tsah = "yes", edh = "no",
              glucose = 16, hb = 8)
  expect_equal(r$mortality[r$model == "core"], 0.902823661250, tolerance = 1e-9)
  expect_equal(r$mortality[r$model == "lab"],  0.953068603433, tolerance = 1e-9)
})

test_that("unfavourable outcome is never less likely than death", {
  # Death is one way to have an unfavourable outcome, so the second must
  # dominate the first for any patient.
  s <- get_sample_input()
  for (i in seq_len(nrow(s))) {
    r <- model_run(as.list(s[i, ]))
    expect_true(all(r$unfavourable >= r$mortality),
                info = paste("row", i))
  }
})

test_that("risk rises monotonically with age", {
  ages <- seq(20, 90, by = 10)
  m <- vapply(ages, function(a) impact(a, "localizes", "both")$mortality, numeric(1))
  expect_true(all(diff(m) > 0))
})

test_that("motor and pupil labels, GCS numbers and aliases agree", {
  a <- impact(age = 40, motor = "extension", pupils = "one")
  b <- impact(age = 40, motor = 2, pupils = 1)          # GCS motor 2; 1 reacting
  expect_equal(a$mortality, b$mortality)
  # localizes and obeys are both the reference category
  expect_equal(impact(40, "localizes", "both")$mortality,
               impact(40, "obeys", "both")$mortality)
  expect_error(impact(40, "shrugging", "both"), "Unrecognised `motor`")
  expect_error(impact(40, "localizes", "winking"), "Unrecognised `pupils`")
})

test_that("Marshall CT classes III/IV and V/VI share coefficients", {
  base <- list(age = 50, motor = "normal_flexion", pupils = "one",
               hypoxia = "no", hypotension = "no", tsah = "no", edh = "no")
  iii <- do.call(impact, c(base, list(ct_class = "III")))
  iv  <- do.call(impact, c(base, list(ct_class = "IV")))
  v   <- do.call(impact, c(base, list(ct_class = 5)))
  vi  <- do.call(impact, c(base, list(ct_class = "nonevacuated mass lesion")))
  expect_equal(iii$mortality, iv$mortality)
  expect_equal(v$mortality, vi$mortality)
  expect_error(do.call(impact, c(base, list(ct_class = "VII"))), "Unrecognised")
})

test_that("epidural haematoma lowers risk and tSAH raises it", {
  # Both signs are counter-intuitive enough to be worth pinning: an epidural
  # haematoma carries a *better* prognosis in these models.
  base <- list(age = 50, motor = "normal_flexion", pupils = "one",
               hypoxia = "no", hypotension = "no", ct_class = "II", tsah = "no")
  with_edh    <- do.call(impact, c(base, list(edh = "yes")))
  without_edh <- do.call(impact, c(base, list(edh = "no")))
  expect_lt(with_edh$mortality[with_edh$model == "extended"],
            without_edh$mortality[without_edh$model == "extended"])

  base2 <- modifyList(base, list(tsah = NULL, edh = "no"))
  with_sah    <- do.call(impact, c(base2, list(tsah = "yes")))
  without_sah <- do.call(impact, c(base2, list(tsah = "no")))
  expect_gt(with_sah$mortality[with_sah$model == "extended"],
            without_sah$mortality[without_sah$model == "extended"])
})

test_that("the score chart is exposed but is not what impact() computes", {
  sc <- score_chart()
  expect_s3_class(sc, "data.frame")
  expect_equal(sum(sc$points[sc$characteristic == "Age (years)"]), 15)
  # A 30-year-old who localises with both pupils reacting scores 0 on the
  # chart, which maps to 7.2% mortality. The continuous model gives 11.0%,
  # and the continuous model is what the official calculator uses.
  chart_p <- stats::plogis(-2.55 + 0.275 * 0)
  expect_equal(round(chart_p, 3), 0.072)
  expect_gt(impact(30, "localizes", "both")$mortality, chart_p)
})

test_that("missing required predictors are named in the error", {
  expect_error(model_run(list(age = 40)), "Missing required variable")
  expect_error(model_run(list(age = 40, motor = "localizes")), "pupils")
})

test_that("a JSON null is treated as an absent optional field", {
  expect_no_error(
    r <- model_run(list(age = 30, motor = "localizes", pupils = "both",
                        glucose = NULL, hb = NULL, ct_class = NULL))
  )
  expect_equal(r$model, "core")
})

test_that("an empty string is treated as not supplied, as the web form does", {
  r <- model_run(list(age = 30, motor = "localizes", pupils = "both",
                      glucose = "", hb = ""))
  expect_equal(r$model, "core")
})

test_that("implausible ages and units are rejected", {
  expect_error(impact(200, "localizes", "both"), "plausible age")
  expect_error(impact(30, "localizes", "both", glucose = -1), "glucose")
})

test_that("get_sample_input(n) limits rows and validates n", {
  expect_equal(nrow(get_sample_input(2)), 2L)
  expect_error(get_sample_input(0), "positive integer")
})
