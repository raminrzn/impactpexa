# ---------------------------------------------------------------------------
# IMPACT prognostic models for traumatic brain injury.
#
# Steyerberg EW, Mushkudiani N, Perel P, et al. Predicting outcome after
# traumatic brain injury: development and international validation of
# prognostic scores based on admission characteristics.
# PLoS Med. 2008;5(8):e165. doi:10.1371/journal.pmed.0050165
#
# WHICH PARAMETERISATION THIS USES
#
# The paper presents the models two ways. Figure 1 is a rounded *score chart*
# (age in 10-year bands, integer points per predictor). The authors' own
# calculator at tbi-impact.org instead uses the underlying *continuous*
# regression, published separately as "Coefficients model fit 2006"
# (documents/Coefs_model_fit_2006_v2.pdf on that site).
#
# The two disagree materially. A 30-year-old who localises with both pupils
# reacting scores 0 on the chart, giving 7.2% mortality; the continuous model
# gives 11.0%, which is what the official calculator returns. This package
# implements the continuous model, so its numbers match the calculator a
# clinician would check against. The score chart is reproduced in
# score_chart() for reference but is not used in the prediction path.
#
# Coefficients below are transcribed from the authors' PDF, which uses
# European decimal commas ("-3,109" = -3.109).
# ---------------------------------------------------------------------------

# Reference (zero-coefficient) categories: motor = localizes/obeys,
# pupils = both reactive, Marshall CT class = II.
.impact_coef <- list(
  core = list(
    mortality = c(intercept = -3.109, age = 0.034,
                  motor_none = 1.447, motor_extension = 1.397,
                  motor_abnormal_flexion = 0.797, motor_normal_flexion = 0.390,
                  motor_untestable = 0.522,
                  pupils_one = 0.514, pupils_none = 1.239),
    unfavourable = c(intercept = -2.644, age = 0.038,
                     motor_none = 1.393, motor_extension = 2.078,
                     motor_abnormal_flexion = 1.266, motor_normal_flexion = 0.632,
                     motor_untestable = 0.885,
                     pupils_one = 0.592, pupils_none = 1.216)
  ),
  extended = list(
    mortality = c(intercept = -3.787, age = 0.032,
                  motor_none = 1.205, motor_extension = 1.207,
                  motor_abnormal_flexion = 0.746, motor_normal_flexion = 0.313,
                  motor_untestable = 0.425,
                  pupils_one = 0.334, pupils_none = 0.970,
                  ct_I = -0.298, ct_III_IV = 0.774, ct_V_VI = 0.651,
                  tsah = 0.606, edh = -0.379,
                  hypoxia = 0.237, hypotension = 0.667),
    unfavourable = c(intercept = -3.023, age = 0.033,
                     motor_none = 1.218, motor_extension = 1.852,
                     motor_abnormal_flexion = 1.185, motor_normal_flexion = 0.575,
                     motor_untestable = 0.837,
                     pupils_one = 0.442, pupils_none = 1.003,
                     ct_I = -0.530, ct_III_IV = 0.543, ct_V_VI = 0.497,
                     tsah = 0.567, edh = -0.572,
                     hypoxia = 0.316, hypotension = 0.614)
  ),
  lab = list(
    mortality = c(intercept = -3.184, age = 0.020,
                  motor_none = 0.965, motor_extension = 1.109,
                  motor_abnormal_flexion = 0.778, motor_normal_flexion = 0.310,
                  motor_untestable = 0.462,
                  pupils_one = 0.090, pupils_none = 0.533,
                  ct_I = -0.150, ct_III_IV = 0.715, ct_V_VI = 0.807,
                  tsah = 0.740, edh = -0.510,
                  hypoxia = 0.360, hypotension = 0.366,
                  glucose = 0.097, hb = -0.086),
    unfavourable = c(intercept = -2.470, age = 0.029,
                     motor_none = 1.105, motor_extension = 1.801,
                     motor_abnormal_flexion = 1.186, motor_normal_flexion = 0.548,
                     motor_untestable = 0.059,
                     pupils_one = 0.201, pupils_none = 0.712,
                     ct_I = -0.567, ct_III_IV = 0.508, ct_V_VI = 0.571,
                     tsah = 0.666, edh = -0.687,
                     hypoxia = 0.396, hypotension = 0.440,
                     glucose = 0.085, hb = -0.092)
  )
)

# Motor score. "Localizes" and "obeys" are both the reference category, so
# they carry no term; the GCS motor numbering (1 = none ... 6 = obeys) is
# accepted alongside the labels because that is how it is charted.
.impact_motor <- function(x) {
  if (is.numeric(x) && length(x) == 1L && !is.na(x)) {
    map <- c("1" = "none", "2" = "extension", "3" = "abnormal_flexion",
             "4" = "normal_flexion", "5" = "localizes", "6" = "obeys",
             "9" = "untestable")
    key <- map[[as.character(as.integer(x))]]
    if (is.null(key)) {
      stop("Numeric `motor` must be 1-6 on the GCS motor scale, or 9 for ",
           "untestable/missing. Got: ", x, call. = FALSE)
    }
    return(key)
  }
  key <- gsub("[^a-z]+", "_", tolower(trimws(as.character(x))))
  key <- gsub("^_|_$", "", key)
  switch(key,
    none = , no_response = , nil = "none",
    extension = , extensor = , decerebrate = "extension",
    abnormal_flexion = , abnormal = , decorticate = "abnormal_flexion",
    normal_flexion = , normal = , withdraws = , withdrawal = "normal_flexion",
    localizes = , localises = , localizing = "localizes",
    obeys = , obeys_commands = , following_commands = "obeys",
    untestable = , untestable_missing = , missing = , unknown = "untestable",
    stop('Unrecognised `motor` value: "', x, '". Use one of: none, extension, ',
         'abnormal_flexion, normal_flexion, localizes, obeys, untestable.',
         call. = FALSE)
  )
}

.impact_pupils <- function(x) {
  if (is.numeric(x) && length(x) == 1L && !is.na(x)) {
    # Number of *reacting* pupils is the natural numeric reading.
    key <- c("2" = "both", "1" = "one", "0" = "none")[[as.character(as.integer(x))]]
    if (is.null(key)) {
      stop("Numeric `pupils` must be the number reacting: 2, 1 or 0. Got: ", x,
           call. = FALSE)
    }
    return(key)
  }
  key <- gsub("[^a-z]+", "_", tolower(trimws(as.character(x))))
  key <- gsub("^_|_$", "", key)
  switch(key,
    both = , both_reacting = , both_reactive = , two = "both",
    one = , one_reacting = , one_reactive = "one",
    none = , none_reacting = , none_reactive = , neither = , no = "none",
    stop('Unrecognised `pupils` value: "', x, '". Use "both", "one" or "none" ',
         '(reacting pupils).', call. = FALSE)
  )
}

# Marshall CT classification. I-VI, where V is an evacuated mass lesion and
# VI a non-evacuated one. Class II is the reference.
.impact_ct <- function(x) {
  if (is.numeric(x) && length(x) == 1L && !is.na(x)) {
    n <- as.integer(x)
    if (!n %in% 1:6) {
      stop("Numeric `ct_class` must be 1-6 (Marshall classification). Got: ", x,
           call. = FALSE)
    }
    return(n)
  }
  key <- gsub("[^a-z0-9]+", "_", tolower(trimws(as.character(x))))
  key <- gsub("^_|_$", "", key)
  n <- switch(key,
    i = , `1` = , diffuse_injury_i = 1L,
    ii = , `2` = , diffuse_injury_ii = 2L,
    iii = , `3` = , diffuse_injury_iii = 3L,
    iv = , `4` = , diffuse_injury_iv = 4L,
    v = , `5` = , evacuated_mass_lesion = , evacuated = 5L,
    vi = , `6` = , nonevacuated_mass_lesion = , non_evacuated_mass_lesion = ,
    nonevacuated = , non_evacuated = 6L,
    NULL
  )
  if (is.null(n)) {
    stop('Unrecognised `ct_class` value: "', x, '". Use I-VI, 1-6, or ',
         '"evacuated mass lesion" / "nonevacuated mass lesion".', call. = FALSE)
  }
  n
}

.impact_yesno <- function(x, field) {
  if (is.logical(x) && length(x) == 1L && !is.na(x)) return(as.integer(x))
  if (is.numeric(x) && length(x) == 1L && !is.na(x)) {
    if (!x %in% c(0, 1)) {
      stop("Numeric `", field, "` must be 0 or 1. Got: ", x, call. = FALSE)
    }
    return(as.integer(x))
  }
  key <- tolower(trimws(as.character(x)))
  if (key %in% c("yes", "y", "true", "1", "suspected", "yes or suspected")) return(1L)
  if (key %in% c("no", "n", "false", "0")) return(0L)
  stop('Unrecognised `', field, '` value: "', x, '". Use yes/no or 1/0.',
       call. = FALSE)
}

# Build the design vector shared by all three models, then add the terms the
# richer models need. Names must match .impact_coef exactly.
.impact_terms <- function(age, motor, pupils, hypoxia = NULL, hypotension = NULL,
                          ct_class = NULL, tsah = NULL, edh = NULL,
                          glucose = NULL, hb = NULL) {
  tms <- c(intercept = 1, age = age)
  tms["motor_none"]            <- as.numeric(motor == "none")
  tms["motor_extension"]       <- as.numeric(motor == "extension")
  tms["motor_abnormal_flexion"] <- as.numeric(motor == "abnormal_flexion")
  tms["motor_normal_flexion"]  <- as.numeric(motor == "normal_flexion")
  tms["motor_untestable"]      <- as.numeric(motor == "untestable")
  tms["pupils_one"]            <- as.numeric(pupils == "one")
  tms["pupils_none"]           <- as.numeric(pupils == "none")

  if (!is.null(ct_class)) {
    tms["ct_I"]      <- as.numeric(ct_class == 1L)
    tms["ct_III_IV"] <- as.numeric(ct_class %in% c(3L, 4L))
    tms["ct_V_VI"]   <- as.numeric(ct_class %in% c(5L, 6L))
  }
  if (!is.null(tsah))        tms["tsah"]        <- tsah
  if (!is.null(edh))         tms["edh"]         <- edh
  if (!is.null(hypoxia))     tms["hypoxia"]     <- hypoxia
  if (!is.null(hypotension)) tms["hypotension"] <- hypotension
  if (!is.null(glucose))     tms["glucose"]     <- glucose
  if (!is.null(hb))          tms["hb"]          <- hb
  tms
}

.impact_prob <- function(coefs, tms) {
  used <- intersect(names(coefs), names(tms))
  missing <- setdiff(names(coefs), names(tms))
  if (length(missing) > 0) return(NA_real_)
  lp <- sum(coefs[used] * tms[used])
  stats::plogis(lp)
}

#' IMPACT score chart (Figure 1 of the paper)
#'
#' The rounded points-based presentation of the models. Provided for reference
#' and teaching; [impact()] does **not** use it, because the authors' own
#' calculator uses the continuous coefficients instead and the two disagree by
#' several percentage points.
#'
#' @return A data frame with columns `characteristic`, `value` and `points`.
#' @examples
#' score_chart()
#' @export
score_chart <- function() {
  data.frame(
    characteristic = c(rep("Age (years)", 6), rep("Motor score", 5),
                       rep("Pupillary reactivity", 3),
                       rep("Hypoxia", 2), rep("Hypotension", 2),
                       rep("CT classification", 4),
                       rep("Traumatic subarachnoid haemorrhage", 2),
                       rep("Epidural haematoma", 2),
                       rep("Glucose (mmol/L)", 5), rep("Hb (g/dL)", 4)),
    value = c("<=30", "30-39", "40-49", "50-59", "60-69", "70+",
              "None/extension", "Abnormal flexion", "Normal flexion",
              "Localizes/obeys", "Untestable/missing",
              "Both pupils reacted", "One pupil reacted", "No pupil reacted",
              "Yes or suspected", "No", "Yes or suspected", "No",
              "I", "II", "III/IV", "V/VI",
              "Yes", "No", "Yes", "No",
              "<6", "6-8.9", "9-11.9", "12-14.9", "15+",
              "<9", "9-11.9", "12-14.9", "15+"),
    points = c(0, 1, 2, 3, 4, 5,
               6, 4, 2, 0, 3,
               0, 2, 4,
               1, 0, 2, 0,
               -2, 0, 2, 2,
               2, 0, -2, 0,
               0, 1, 2, 3, 4,
               3, 2, 1, 0),
    stringsAsFactors = FALSE
  )
}

#' IMPACT prognosis for one patient
#'
#' Computes 6-month mortality and 6-month unfavourable outcome probabilities
#' under whichever of the three nested IMPACT models the supplied predictors
#' support.
#'
#' @param age Age in years. The models were developed in adults; the authors'
#'   calculator accepts 14-99.
#' @param motor Motor score: `"none"`, `"extension"`, `"abnormal_flexion"`,
#'   `"normal_flexion"`, `"localizes"`, `"obeys"` or `"untestable"`. GCS motor
#'   numbers 1-6 (and 9 for untestable) are also accepted.
#' @param pupils Pupillary reactivity: `"both"`, `"one"` or `"none"` reacting,
#'   or the number reacting (2, 1, 0).
#' @param hypoxia,hypotension Secondary insults, yes/no. Needed for the
#'   Extended and Lab models.
#' @param ct_class Marshall CT classification, `"I"`-`"VI"` or 1-6 (V =
#'   evacuated mass lesion, VI = non-evacuated). Needed for Extended and Lab.
#' @param tsah Traumatic subarachnoid haemorrhage on CT, yes/no.
#' @param edh Epidural haematoma on CT, yes/no.
#' @param glucose Glucose in mmol/L. Needed for the Lab model.
#' @param hb Haemoglobin in g/dL. Needed for the Lab model.
#'
#' @return A data frame with one row per computable model (`"core"`,
#'   `"extended"`, `"lab"`) and columns `model`, `mortality` and
#'   `unfavourable` (probabilities on 0-1).
#'
#' @references
#' Steyerberg EW, Mushkudiani N, Perel P, et al. Predicting outcome after
#' traumatic brain injury: development and international validation of
#' prognostic scores based on admission characteristics. *PLoS Med.*
#' 2008;5(8):e165. \doi{10.1371/journal.pmed.0050165}
#'
#' @examples
#' impact(age = 30, motor = "localizes", pupils = "both")
#' impact(age = 65, motor = "extension", pupils = "one",
#'        hypoxia = "no", hypotension = "yes", ct_class = "III",
#'        tsah = "yes", edh = "no")
#' @export
impact <- function(age, motor, pupils,
                   hypoxia = NULL, hypotension = NULL, ct_class = NULL,
                   tsah = NULL, edh = NULL, glucose = NULL, hb = NULL) {
  if (!is.numeric(age) || length(age) != 1L || is.na(age)) {
    stop("`age` must be a single number, in years.", call. = FALSE)
  }
  if (age < 0 || age > 120) {
    stop("`age` must be a plausible age in years. Got: ", age, call. = FALSE)
  }
  motor  <- .impact_motor(motor)
  pupils <- .impact_pupils(pupils)

  ct <- if (is.null(ct_class)) NULL else .impact_ct(ct_class)
  ts <- if (is.null(tsah)) NULL else .impact_yesno(tsah, "tsah")
  ed <- if (is.null(edh)) NULL else .impact_yesno(edh, "edh")
  hx <- if (is.null(hypoxia)) NULL else .impact_yesno(hypoxia, "hypoxia")
  ht <- if (is.null(hypotension)) NULL else .impact_yesno(hypotension, "hypotension")

  if (!is.null(glucose) && (!is.numeric(glucose) || glucose <= 0)) {
    stop("`glucose` must be a positive number in mmol/L.", call. = FALSE)
  }
  if (!is.null(hb) && (!is.numeric(hb) || hb <= 0)) {
    stop("`hb` must be a positive number in g/dL.", call. = FALSE)
  }

  tms <- .impact_terms(age, motor, pupils, hx, ht, ct, ts, ed, glucose, hb)

  rows <- lapply(names(.impact_coef), function(m) {
    data.frame(
      model        = m,
      mortality    = .impact_prob(.impact_coef[[m]]$mortality, tms),
      unfavourable = .impact_prob(.impact_coef[[m]]$unfavourable, tms),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out <- out[!is.na(out$mortality), , drop = FALSE]
  rownames(out) <- NULL
  out
}
