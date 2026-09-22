# impactpexa

<!-- badges: start -->
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end -->

The **IMPACT prognostic models** for traumatic brain injury (Steyerberg et al.,
*PLoS Medicine* 2008), packaged as a ModelsCloud service.

Given admission characteristics for an adult with moderate to severe TBI
(Glasgow Coma Scale ≤ 12), it estimates the probability of **6-month
mortality** and **6-month unfavourable outcome** under three nested models of
increasing complexity.

| Model | Predictors |
|---|---|
| **Core** | Age, motor score, pupillary reactivity |
| **Extended** (Core+CT) | Core + hypoxia, hypotension, Marshall CT class, tSAH, epidural haematoma |
| **Lab** (Core+CT+Lab) | Extended + glucose, haemoglobin |

---

## Which parameterisation this uses — and why it matters

The paper presents the models **two different ways**, and they do not agree.

*Figure 1* is a rounded **score chart**: age in 10-year bands, integer points
per predictor, six published linear predictors. It is the version most people
transcribe, because it is the one printed in the paper.

The authors' own calculator at [tbi-impact.org](http://www.tbi-impact.org/?p=impact/calc)
uses something else — the underlying **continuous regression**, published
separately as *"Coefficients model fit 2006"*
([PDF](http://www.tbi-impact.org/documents/Coefs_model_fit_2006_v2.pdf)).

The gap is not small. A 30-year-old who localises to pain with both pupils
reacting scores 0 on the chart, giving **7.2%** mortality. The continuous model
gives **11.0%** — and 11% is what the official calculator returns.

**This package implements the continuous model**, so its numbers match the
calculator a clinician would check against. The score chart is still available
via `score_chart()` for reference and teaching, but it is not used in the
prediction path.

> Transcribing Figure 1 would have produced a defensible-looking implementation
> that disagreed with the authors' own tool by several percentage points on
> every patient. That is the failure mode this note exists to prevent.

---

## Validation

Checked against the official calculator across **48 model/outcome combinations**
spanning eight patients (ages 19–88, every motor and pupil category, all six
Marshall classes):

| | Agreement |
|---|---|
| Within 1.0 percentage point | **48 / 48** |
| Within 0.5 percentage point | 40 / 48 |
| Mean absolute difference | 0.32 pp |

That is the tightest agreement obtainable: the calculator reports integer
percentages (±0.5 pp of rounding on its side) and the published coefficients
are given to three decimals.

---

## Installation

```r
# install.packages("remotes")
remotes::install_git("https://github.com/raminrzn/impactpexa", ref = "main")
```

## Quick start

```r
library(impactpexa)

impact(age = 30, motor = "localizes", pupils = "both")
#>   model mortality unfavourable
#> 1  core 0.1101706    0.1818297

# Supply more, get more models back
model_run(get_default_input())
#>      model  mortality unfavourable
#> 1     core 0.11017057   0.18182970
#> 2 extended 0.05588247   0.11578144
#> 3      lab 0.03894056   0.08486576
```

Only the models your inputs support are returned — supply the core three and
you get the core model, supply everything and you get all three.

---

## Inputs

| Field | Meaning | Coding |
|---|---|---|
| `age` | Age | years (calculator accepts 14–99) |
| `motor` | Motor score | `"none"`, `"extension"`, `"abnormal_flexion"`, `"normal_flexion"`, `"localizes"`, `"obeys"`, `"untestable"` — or GCS motor `1`–`6`, `9` |
| `pupils` | Pupillary reactivity | `"both"`, `"one"`, `"none"` reacting — or the count `2`/`1`/`0` |
| `hypoxia`, `hypotension` | Secondary insults | `"yes"`/`"no"` |
| `ct_class` | Marshall CT classification | `"I"`–`"VI"` or `1`–`6` (V = evacuated mass lesion, VI = non-evacuated) |
| `tsah` | Traumatic subarachnoid haemorrhage | `"yes"`/`"no"` |
| `edh` | Epidural haematoma | `"yes"`/`"no"` |
| `glucose` | Glucose | **mmol/L** |
| `hb` | Haemoglobin | **g/dL** |

Aliases are accepted (`marshall`→`ct_class`, `gcs_motor`→`motor`,
`hemoglobin`→`hb`), unknown extra fields are ignored, and an empty string is
treated as "not supplied" — which is how the web form spells it.

Two coefficient signs are worth knowing because they look like bugs and are
not: an **epidural haematoma lowers** predicted risk, and `localizes` and
`obeys` are a single reference category, so they give identical answers.

---

## Output

One row per computable model:

| Column | Meaning |
|---|---|
| `model` | `"core"`, `"extended"` or `"lab"` |
| `mortality` | P(death by 6 months), 0–1 |
| `unfavourable` | P(unfavourable outcome at 6 months), 0–1 |

---

## ModelsCloud entry points

| Function | Description |
|---|---|
| `model_run(model_input)` | Prognosis for one patient. |
| `impact(...)` | The same, as direct arguments. |
| `score_chart()` | Figure 1's points table, for reference. |
| `get_sample_input(n)` | Example patients. |
| `get_default_input()` | One patient to modify. |
| `gateway(...)` | The platform's dispatcher; defaults to `model_run`. |

### Raw HTTP

```bash
curl -X POST https://core.modelscloud.resp.core.ubc.ca/call/v2/<ns>/impactpexa \
  -H "Authorization: Bearer <ACCESS_KEY_TOKEN>" \
  -H "Content-Type: application/json" \
  -d '{"funcInput": {"model_input": {
        "age": 30, "motor": "localizes", "pupils": "both",
        "ct_class": "II", "tsah": "no", "edh": "no",
        "hypoxia": "no", "hypotension": "no",
        "glucose": 6, "hb": 14
      }}}'
```

---

## Clinical interpretation

The IMPACT investigators are explicit that these models are **particularly
suited to classification and characterisation of large cohorts**, and that
*"extreme caution is required when applying the estimated prognosis to
individual patients."* This service returns the raw probabilities and applies
no threshold.

> For research use. Not a medical device and not a substitute for clinical
> judgement.

---

## Reference

> Steyerberg EW, Mushkudiani N, Perel P, Butcher I, Lu J, McHugh GS, Murray GD,
> Marmarou A, Roberts I, Habbema JDF, Maas AIR. Predicting outcome after
> traumatic brain injury: development and international validation of
> prognostic scores based on admission characteristics. *PLoS Med.*
> 2008;5(8):e165.
> doi:[10.1371/journal.pmed.0050165](https://doi.org/10.1371/journal.pmed.0050165)

The paper is open access under CC-BY. The continuous coefficients are published
by the IMPACT investigators on [tbi-impact.org](http://www.tbi-impact.org/)
for exactly this purpose.

## License

GPL-3. Model © its original authors; wrapper implementation © Ramin
Rezaeianzadeh.
