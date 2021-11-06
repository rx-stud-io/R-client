#' Meropenem » Bayesian » \% fT > MIC
#' 
#' Meropenem » Bayesian adaptive dosing » Percent time of free concentration above MIC
#' 
#' \strong{Drug}:
#' Meropenem
#' 
#' \strong{Method}:
#' Estimate the pharmacokinetic parameters of the patient from past concentrations and creatinine levels with Bayesian inverse modeling, then use that information to predict the steady state concentrations for multiple dosing regimens and select the optimal one, with regard to the target pharmacodynamic index.
#' 
#' \strong{PK/PD target}:
#' The percent of time that the free concentration is above the minimum inhibitory concentration.
#' 
#' @param PATID Patient Identifier. User-provided free text (such as patient id, name or alias) to identify related simulations. Must be provided as string.
#' @param AGE Age. Age of the patient in years. Must be provided as numeric (min. 18, max. 120 year).
#' @param HEIGHT Height. Height of the patient. Must be provided as numeric (min. 100, max. 250 cm).
#' @param WEIGHT Weight. Actual body weight of the patient. Must be provided as numeric (min. 20, max. 500 kg).
#' @param GENDER Sex. Patient's sex for clinical decision-making. Must be provided as string ('Male' or 'Female').
#' @param MODEL Model for population of interest. Pharmacokinetic model to be used for specific patient type during simulations. Must be provided as string ('Crandon et al. (2011) - ICU', 'Li, C. et. al. (2006) - General ward' or 'Doh, K. et al. (2010) - Burn patients').
#' @param EDEMA Edema. Presence of edema in case of Burn Patients. Must be provided as string ('No' or 'Yes').
#' @param MIC MIC. Minimum Inhibitory Concentration (MIC). Must be provided as numeric (min. 0.01, max. 1024 mg/L).
#' @param PCTABOVEMIC Percent of time target that the drug concentration is above MIC. The PK/PD target can be provided as the percent of time that the drug concentration is above the minimum inhibitory concentration (\% T > MIC). Must be provided as numeric (min. 5, max. 100 \%).
#' @param CRCLCAP Capping Creatinine Clearance. Whether to use capping for creatinine clearance. Must be provided as string ('No cap', '120 ml/min', '130 ml/min', '140 ml/min' or '150 ml/min').
#' @param HISTORY Historical Records.  Must be provided as list of 3-24 'HISTCREATININE', 'HISTDOSE' or 'HISTCONCENTRATION' values.
#' @param REGIMENS Dosing Regimens. List of dosing regimens to be used in simulating target attainment, from which the dosing regimen with the smallest absolute difference from the desired target will be automatically selected. Must be provided as list of 1-20 'REGIMEN' values. Use the \code{regimen} helper function to define the REGIMEN values.
#' 
#' @examples \dontrun{
#' simulate_meropenem_bayesian_ftime_above_mic(PATID = "Anonymous", 
#'     AGE = 65, HEIGHT = 175, 
#'     WEIGHT = 75, GENDER = "Male", 
#'     MODEL = "Crandon et al. (2011) - ICU", 
#'     EDEMA = "No", MIC = 1, 
#'     PCTABOVEMIC = 40, 
#'     CRCLCAP = "No cap", 
#'     HISTORY = list(list(
#'         DATETIME = structure(1601870400, class = c("POSIXct", 
#'         "POSIXt"), tzone = ""), 
#'         DOSE = 1000, 
#'         TINF = 2, set = "HISTDOSE"), 
#'         list(DATETIME = structure(1601899200, class = c("POSIXct", 
#'         "POSIXt"), tzone = ""), 
#'             DOSE = 1000, 
#'             TINF = 1, 
#'             set = "HISTDOSE"), 
#'         list(DATETIME = structure(1601942400, class = c("POSIXct", 
#'         "POSIXt"), tzone = ""), 
#'             DOSE = 750, 
#'             TINF = 1, 
#'             set = "HISTDOSE"), 
#'         list(DATETIME = structure(1601866800, class = c("POSIXct", 
#'         "POSIXt"), tzone = ""), 
#'             CREATININE = 0.9, 
#'             set = "HISTCREATININE"), 
#'         list(DATETIME = structure(1601906400, class = c("POSIXct", 
#'         "POSIXt"), tzone = ""), 
#'             CREATININE = 0.7, 
#'             set = "HISTCREATININE"), 
#'         list(DATETIME = structure(1601886600, class = c("POSIXct", 
#'         "POSIXt"), tzone = ""), 
#'             CONCENTRATION = 12, 
#'             set = "HISTCONCENTRATION"), 
#'         list(DATETIME = structure(1601890200, class = c("POSIXct", 
#'         "POSIXt"), tzone = ""), 
#'             CONCENTRATION = 8, 
#'             set = "HISTCONCENTRATION"), 
#'         list(DATETIME = structure(1601906400, class = c("POSIXct", 
#'         "POSIXt"), tzone = ""), 
#'             CONCENTRATION = 20, 
#'             set = "HISTCONCENTRATION")), 
#'     REGIMENS = list(list(
#'         set = "REGIMEN", 
#'         DOSE = 500, INTERVAL = 6, 
#'         TINF = 0.5), 
#'         list(set = "REGIMEN", 
#'             DOSE = 500, 
#'             INTERVAL = 8, 
#'             TINF = 0.5), 
#'         list(set = "REGIMEN", 
#'             DOSE = 500, 
#'             INTERVAL = 12, 
#'             TINF = 0.5), 
#'         list(set = "REGIMEN", 
#'             DOSE = 500, 
#'             INTERVAL = 24, 
#'             TINF = 0.5), 
#'         list(set = "REGIMEN", 
#'             DOSE = 1000, 
#'             INTERVAL = 6, 
#'             TINF = 1), 
#'         list(set = "REGIMEN", 
#'             DOSE = 1000, 
#'             INTERVAL = 8, 
#'             TINF = 1), 
#'         list(set = "REGIMEN", 
#'             DOSE = 1000, 
#'             INTERVAL = 12, 
#'             TINF = 1), 
#'         list(set = "REGIMEN", 
#'             DOSE = 1000, 
#'             INTERVAL = 24, 
#'             TINF = 1), 
#'         list(set = "REGIMEN", 
#'             DOSE = 2000, 
#'             INTERVAL = 8, 
#'             TINF = 2), 
#'         list(set = "REGIMEN", 
#'             DOSE = 2000, 
#'             INTERVAL = 12, 
#'             TINF = 2), 
#'         list(set = "REGIMEN", 
#'             DOSE = 2000, 
#'             INTERVAL = 24, 
#'             TINF = 2)))
#' }
#' 
#' @references \itemize{
#'   \item Jared L Crandon et al. (2011): Optimization of meropenem dosage in the critically ill population based on renal function. In. Intensive Care Medicine. https://pubmed.ncbi.nlm.nih.gov/21136037
#'   \item Li, C. et. al (2006): Population Pharmacokinetic Analysis and Dosing Regimen Optimization of Meropenem in Adult Patients. In. The Journal of Clinical Pharmacology. https://accp1.onlinelibrary.wiley.com/doi/10.1177/0091270006291035
#'   \item Doh, K. et al. (2010): Population pharmacokinetics of meropenem in burn patients. In. Journal of Antimicrobial Chemotherapy. https://academic.oup.com/jac/article/65/11/2428/762112
#' }
simulate_meropenem_bayesian_ftime_above_mic <- function(PATID, AGE, HEIGHT, WEIGHT, GENDER, MODEL, EDEMA, MIC, PCTABOVEMIC, CRCLCAP, HISTORY, REGIMENS) {
  ## check args
  assert_string(PATID)
  assert_number(AGE,
    lower = 18,
    upper = 120
  )
  assert_number(HEIGHT,
    lower = 100,
    upper = 250
  )
  assert_number(WEIGHT,
    lower = 20,
    upper = 500
  )
  assert_string(GENDER)
  assert_choice(GENDER, c("Male", "Female"))
  assert_string(MODEL)
  assert_choice(MODEL, c("Crandon et al. (2011) - ICU", "Li, C. et. al. (2006) - General ward", "Doh, K. et al. (2010) - Burn patients"))
  assert_string(EDEMA)
  assert_choice(EDEMA, c("No", "Yes"))
  assert_number(MIC,
    lower = 0.01,
    upper = 1024
  )
  assert_number(PCTABOVEMIC,
    lower = 5,
    upper = 100
  )
  assert_string(CRCLCAP)
  assert_choice(CRCLCAP, c("No cap", "120 ml/min", "130 ml/min", "140 ml/min", "150 ml/min"))
  ## API call
  simulate("meropenem-bayesian-ftime-above-mic", PATID = PATID, AGE = AGE, HEIGHT = HEIGHT, WEIGHT = WEIGHT, GENDER = GENDER, MODEL = MODEL, EDEMA = EDEMA, MIC = MIC, PCTABOVEMIC = PCTABOVEMIC, CRCLCAP = CRCLCAP, HISTORY = HISTORY, REGIMENS = REGIMENS)
}
