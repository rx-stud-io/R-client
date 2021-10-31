#' Piperacillin & Tazobactam » Bayesian » fCmin/MIC
#' 
#' Piperacillin & Tazobactam » Bayesian adaptive dosing » Free minimum concentration to MIC ratio
#' 
#' @section Drug:
#' Piperacillin & Tazobactam
#' 
#' @section Method:
#' Estimate the pharmacokinetic parameters of the patient from past concentrations and creatinine levels with Bayesian inverse modeling, then use that information to predict the steady state concentrations for multiple dosing regimens and select the optimal one, with regard to the target pharmacodynamic index.
#' 
#' @section PK/PD target:
#' Free minimum concentration (mg/L) to minimum inhibitory concentration ratio (mg/L).
#' 
#' @param PATID Patient Identifier. User-provided free text (such as patient id, name or alias) to identify related simulations. Must be provided as string.
#' @param AGE Age. Age of the patient in years. Must be provided as numeric (min. 18, max. 120 year).
#' @param HEIGHT Height. Height of the patient. Must be provided as numeric (min. 100, max. 250 cm).
#' @param WEIGHT Weight. Actual body weight of the patient. Must be provided as numeric (min. 20, max. 500 kg).
#' @param GENDER Sex. Sex of the patient. Must be provided as string ('Male' or 'Female').
#' @param MODEL Model for population of interest. Pharmacokinetic model to be used for specific patient type during simulations. Must be provided as string ('Felton et al. (2014) - ICU' or 'Patel et. al. (2010) - General ward').
#' @param MIC MIC. Minimum Inhibitory Concentration (MIC). Must be provided as numeric (min. 0.01, max. 1024 mg/L).
#' @param CMINPERMIC Minimum concentration to MIC ratio target. The PK/PD target can be provided as minimum concentration to minimum inhibitory concentration ratio (Cmin/MIC). Must be provided as numeric (min. 0.1, max. 50 ).
#' @param CRCLCAP Capping Creatinine Clearance. Whether to use capping for creatinine clearance. Must be provided as string ('No cap', '120 ml/min', '130 ml/min', '140 ml/min' or '150 ml/min').
#' @param HISTORY Historical Records.  Must be provided as list of 3-24 'HISTCREATININE', 'HISTDOSE' or 'HISTCONCENTRATION' values.
#' @param REGIMENS Dosing Regimens. List of dosing regimens to be used in simulating target attainment, from which the dosing regimen with the smallest absolute difference from the desired target will be automatically selected. Must be provided as list of 1-20 'REGIMEN' values. Use the \code{regimen} helper function to define the REGIMEN values.
#' 
#' @examples \dontrun{
#' simulate_piperacillin_tazobactam_bayesian_fcmin_mic_ratio(PATID = "Anonymous", 
#'     AGE = 65, HEIGHT = 175, 
#'     WEIGHT = 75, GENDER = "Male", 
#'     MODEL = "Patel et. al. (2010) - General ward", 
#'     MIC = 4, CMINPERMIC = 2, 
#'     CRCLCAP = "No cap", 
#'     HISTORY = list(list(
#'         DATETIME = structure(1601870400, class = c("POSIXct", 
#'         "POSIXt"), tzone = ""), 
#'         DOSE = 2250, 
#'         TINF = 0.5, set = "HISTDOSE"), 
#'         list(DATETIME = structure(1601881200, class = c("POSIXct", 
#'         "POSIXt"), tzone = ""), 
#'             DOSE = 3375, 
#'             TINF = 0.5, 
#'             set = "HISTDOSE"), 
#'         list(DATETIME = structure(1601899200, class = c("POSIXct", 
#'         "POSIXt"), tzone = ""), 
#'             DOSE = 4500, 
#'             TINF = 3, 
#'             set = "HISTDOSE"), 
#'         list(DATETIME = structure(1601942400, class = c("POSIXct", 
#'         "POSIXt"), tzone = ""), 
#'             DOSE = 2250, 
#'             TINF = 0.5, 
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
#'             CONCENTRATION = 40, 
#'             set = "HISTCONCENTRATION"), 
#'         list(DATETIME = structure(1601890200, class = c("POSIXct", 
#'         "POSIXt"), tzone = ""), 
#'             CONCENTRATION = 25, 
#'             set = "HISTCONCENTRATION"), 
#'         list(DATETIME = structure(1601917200, class = c("POSIXct", 
#'         "POSIXt"), tzone = ""), 
#'             CONCENTRATION = 20, 
#'             set = "HISTCONCENTRATION")), 
#'     REGIMENS = list(list(
#'         DOSE = 2250, 
#'         INTERVAL = 4, 
#'         TINF = 0.5, set = "REGIMEN"), 
#'         list(DOSE = 2250, 
#'             INTERVAL = 6, 
#'             TINF = 0.5, 
#'             set = "REGIMEN"), 
#'         list(DOSE = 2250, 
#'             INTERVAL = 8, 
#'             TINF = 0.5, 
#'             set = "REGIMEN"), 
#'         list(DOSE = 3375, 
#'             INTERVAL = 4, 
#'             TINF = 0.5, 
#'             set = "REGIMEN"), 
#'         list(DOSE = 3375, 
#'             INTERVAL = 6, 
#'             TINF = 0.5, 
#'             set = "REGIMEN"), 
#'         list(DOSE = 3375, 
#'             INTERVAL = 8, 
#'             TINF = 0.5, 
#'             set = "REGIMEN"), 
#'         list(DOSE = 4500, 
#'             INTERVAL = 4, 
#'             TINF = 0.5, 
#'             set = "REGIMEN"), 
#'         list(DOSE = 4500, 
#'             INTERVAL = 6, 
#'             TINF = 0.5, 
#'             set = "REGIMEN")))
#' }
simulate_piperacillin_tazobactam_bayesian_fcmin_mic_ratio <- function(PATID, AGE, HEIGHT, WEIGHT, GENDER, MODEL, MIC, CMINPERMIC, CRCLCAP, HISTORY, REGIMENS) {
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
  assert_choice(MODEL, c("Felton et al. (2014) - ICU", "Patel et. al. (2010) - General ward"))
  assert_number(MIC,
    lower = 0.01,
    upper = 1024
  )
  assert_number(CMINPERMIC,
    lower = 0.1,
    upper = 50
  )
  assert_string(CRCLCAP)
  assert_choice(CRCLCAP, c("No cap", "120 ml/min", "130 ml/min", "140 ml/min", "150 ml/min"))
  ## API call
  simulate("piperacillin-tazobactam-bayesian-fcmin-mic-ratio", PATID = PATID, AGE = AGE, HEIGHT = HEIGHT, WEIGHT = WEIGHT, GENDER = GENDER, MODEL = MODEL, MIC = MIC, CMINPERMIC = CMINPERMIC, CRCLCAP = CRCLCAP, HISTORY = HISTORY, REGIMENS = REGIMENS)
}
