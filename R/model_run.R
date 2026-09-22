# ---------------------------------------------------------------------------
# ModelsCloud API surface for the IMPACT TBI models.
#
# This file only adapts the calling convention: JSON fields in, a tidy result
# out. The model itself is in impact.R.
# ---------------------------------------------------------------------------

.impact_required <- c("age", "motor", "pupils")
.impact_optional <- c("hypoxia", "hypotension", "ct_class", "tsah", "edh",
                      "glucose", "hb")
.impact_vars <- c(.impact_required, .impact_optional)

.impact_alias <- c(
  age_years = "age",
  motor_score = "motor", gcs_motor = "motor", motorscore = "motor",
  pupil = "pupils", pupillary_reactivity = "pupils", pupils_reacting = "pupils",
  ct = "ct_class", ct_classification = "ct_class", marshall = "ct_class",
  marshall_ct = "ct_class", marshall_class = "ct_class",
  sah = "tsah", subarachnoid = "tsah", traumatic_sah = "tsah",
  epidural = "edh", epidural_haematoma = "edh", epidural_hematoma = "edh",
  hypotensive = "hypotension", hypoxic = "hypoxia",
  haemoglobin = "hb", hemoglobin = "hb", hgb = "hb",
  blood_glucose = "glucose", glucose_mmol = "glucose"
)

.impact_normalize <- function(model_input, dots) {
  if (is.null(model_input)) {
    if (length(dots) == 0) return(NULL)
    model_input <- dots
  }
  # The platform serialises absent optional fields as JSON null. Absent and
  # null mean the same thing here, and a NULL would break as.data.frame().
  if (is.list(model_input) && !is.data.frame(model_input)) {
    model_input <- model_input[!vapply(model_input, is.null, logical(1))]
    if (length(model_input) == 0) return(NULL)
  }
  out <- as.list(model_input)
  names(out) <- tolower(names(out))
  for (a in intersect(names(out), names(.impact_alias))) {
    canon <- .impact_alias[[a]]
    if (!canon %in% names(out)) names(out)[match(a, names(out))] <- canon
  }
  out <- out[intersect(names(out), .impact_vars)]
  # An empty string is how the web form spells "not supplied"; treat it that way
  # rather than failing the whole call.
  out[!vapply(out, function(v) is.character(v) && length(v) == 1L && !nzchar(trimws(v)),
              logical(1))]
}

#' IMPACT prognosis (ModelsCloud entry point)
#'
#' Estimates the probability of 6-month mortality and 6-month unfavourable
#' outcome after moderate to severe traumatic brain injury, using the IMPACT
#' models of Steyerberg et al. (*PLoS Med* 2008).
#'
#' @details
#' Inputs may arrive **wrapped** under `model_input` or **unwrapped** as named
#' arguments, and common aliases are mapped to canonical names (`marshall` ->
#' `ct_class`, `gcs_motor` -> `motor`, `hemoglobin` -> `hb`).
#'
#' The three models are nested and are returned together, so a caller who
#' supplies only the core predictors gets the core model, and one who supplies
#' everything gets all three. Results match the authors' own calculator at
#' tbi-impact.org rather than the rounded score chart printed in the paper —
#' see [impact()] for why those two differ.
#'
#' @param model_input A named list (or one-row data frame). Required: `age`
#'   (years), `motor` and `pupils`. Optional, and needed for the richer
#'   models: `hypoxia`, `hypotension`, `ct_class`, `tsah`, `edh` (Extended);
#'   `glucose` (mmol/L) and `hb` (g/dL) (Lab). If `NULL` and nothing is
#'   supplied via `...`, [get_default_input()] is used.
#' @param ... Alternative to `model_input`: the fields as named arguments.
#'
#' @return A data frame with one row per computable model, and columns
#'   `model`, `mortality` and `unfavourable` (probabilities on 0-1).
#'
#' @references
#' Steyerberg EW, Mushkudiani N, Perel P, et al. Predicting outcome after
#' traumatic brain injury: development and international validation of
#' prognostic scores based on admission characteristics. *PLoS Med.*
#' 2008;5(8):e165. \doi{10.1371/journal.pmed.0050165}
#'
#' @examples
#' model_run(get_default_input())
#' model_run(age = 30, motor = "localizes", pupils = "both")
#' @export
model_run <- function(model_input = NULL, ...) {
  args <- .impact_normalize(model_input, list(...))
  if (is.null(args) || length(args) == 0) args <- get_default_input()

  missing <- setdiff(.impact_required, names(args))
  if (length(missing) > 0) {
    stop("Missing required variable(s): ", paste(missing, collapse = ", "),
         ". Accepted names (incl. aliases) are documented in ?model_run.",
         call. = FALSE)
  }
  do.call(impact, args)
}

#' Example IMPACT input cohort
#'
#' Four patients spanning the range the models are used over, from a young
#' patient with a reassuring examination to an elderly one with none of the
#' protective features.
#'
#' @param n Optional positive integer; if supplied, the first `n` rows are
#'   returned. Defaults to all rows.
#' @param ... Additional fields supplied by the platform; ignored.
#' @return A data frame of example patients.
#' @seealso [model_run()], [get_default_input()]
#' @examples
#' get_sample_input()
#' @export
get_sample_input <- function(n = NULL, ...) {
  df <- data.frame(
    age         = c(30, 65, 22, 78),
    motor       = c("localizes", "extension", "obeys", "none"),
    pupils      = c("both", "one", "both", "none"),
    hypoxia     = c("no", "yes", "no", "yes"),
    hypotension = c("no", "yes", "no", "yes"),
    ct_class    = c("II", "IV", "I", "VI"),
    tsah        = c("no", "yes", "no", "yes"),
    edh         = c("no", "no", "yes", "no"),
    glucose     = c(6, 11, 5, 16),
    hb          = c(14, 10, 15, 8),
    stringsAsFactors = FALSE
  )
  if (!is.null(n)) {
    if (!is.numeric(n) || length(n) != 1L || n < 1L) {
      stop("`n` must be a single positive integer.", call. = FALSE)
    }
    df <- utils::head(df, n)
  }
  df
}

#' Default IMPACT input
#'
#' A 30-year-old who localises to pain with both pupils reacting, with the
#' CT and laboratory findings filled in so all three models are computable.
#'
#' @param ... Additional fields supplied by the platform; ignored.
#' @return A named list of default predictor values.
#' @seealso [model_run()], [get_sample_input()]
#' @examples
#' get_default_input()
#' @export
get_default_input <- function(...) {
  list(
    age         = 30,
    motor       = "localizes",
    pupils      = "both",
    hypoxia     = "no",
    hypotension = "no",
    ct_class    = "II",
    tsah        = "no",
    edh         = "no",
    glucose     = 6,
    hb          = 14
  )
}
