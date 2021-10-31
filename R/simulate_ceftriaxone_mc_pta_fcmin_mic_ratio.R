#' Ceftriaxone » PTA » fCmin/MIC
#' 
#' Ceftriaxone » Probability of Target Attainment » Free minimum concentration to MIC ratio
#' 
#' \strong{Drug}:
#' Ceftriaxone
#' 
#' \strong{Method}:
#' Monte Carlo simulation on the probabilities that a specific value of a pharmacodynamic index is achieved in case of different minimum inhibitory concentrations (MIC).
#' 
#' \strong{PK/PD target}:
#' Free minimum concentration (mg/L) to minimum inhibitory concentration ratio (mg/L).
#' 
#' @param PATID Patient Identifier. User-provided free text (such as patient id, name or alias) to identify related simulations. Must be provided as string.
#' @param AGE Age. Age of the patient in years. Must be provided as numeric (min. 18, max. 120 year).
#' @param HEIGHT Height. Height of the patient. Must be provided as numeric (min. 100, max. 250 cm).
#' @param WEIGHT Weight. Actual body weight of the patient. Must be provided as numeric (min. 20, max. 500 kg).
#' @param GENDER Sex. Sex of the patient. Must be provided as string ('Male' or 'Female').
#' @param CREATININE Creatinine. Serum creatinine. Must be provided as numeric (min. 0.01, max. 15 mg/dL).
#' @param MODEL Model for population of interest. Pharmacokinetic model to be used for specific patient type during simulations. Must be provided as string ('Garot et al. (2011) - ICU, septic patients').
#' @param CMINPERMIC Minimum concentration to MIC ratio target. The PK/PD target can be provided as minimum concentration to minimum inhibitory concentration ratio (Cmin/MIC). Must be provided as numeric (min. 0.1, max. 50 ).
#' @param CRCLCAP Capping Creatinine Clearance. Whether to use capping for creatinine clearance. Must be provided as string ('No cap', '120 ml/min', '130 ml/min', '140 ml/min' or '150 ml/min').
#' @param REGIMENS Dosing Regimens. List of dosing regimens to be used in simulating target attainment, from which the dosing regimen with the smallest absolute difference from the desired target will be automatically selected. Must be provided as list of 1-8 'REGIMEN' values. Use the \code{regimen} helper function to define the REGIMEN values.
#' 
#' @examples \dontrun{
#' simulate_ceftriaxone_mc_pta_fcmin_mic_ratio(PATID = "Anonymous", 
#'     AGE = 55, HEIGHT = 195, 
#'     WEIGHT = 95, GENDER = "Male", 
#'     CREATININE = 1, MODEL = "Garot et al. (2011) - ICU, septic patients", 
#'     CMINPERMIC = 1, CRCLCAP = "No cap", 
#'     REGIMENS = list(list(
#'         DOSE = 500, INTERVAL = 12, 
#'         TINF = 0.5, set = "REGIMEN"), 
#'         list(DOSE = 500, 
#'             INTERVAL = 24, 
#'             TINF = 0.5, 
#'             set = "REGIMEN"), 
#'         list(DOSE = 1000, 
#'             INTERVAL = 12, 
#'             TINF = 0.5, 
#'             set = "REGIMEN"), 
#'         list(DOSE = 1000, 
#'             INTERVAL = 24, 
#'             TINF = 0.5, 
#'             set = "REGIMEN"), 
#'         list(DOSE = 1500, 
#'             INTERVAL = 12, 
#'             TINF = 0.5, 
#'             set = "REGIMEN"), 
#'         list(DOSE = 1500, 
#'             INTERVAL = 24, 
#'             TINF = 0.5, 
#'             set = "REGIMEN"), 
#'         list(DOSE = 2000, 
#'             INTERVAL = 12, 
#'             TINF = 0.5, 
#'             set = "REGIMEN"), 
#'         list(DOSE = 2000, 
#'             INTERVAL = 24, 
#'             TINF = 0.5, 
#'             set = "REGIMEN")))
#' }
#' 
#' @references \itemize{
#'   \item Garot, Denis et al. (2011): Population pharmacokinetics of ceftriaxone in critically ill septic patients: a reappraisal. In. British Journal of Clinical Pharmacology. https://bpspubs.onlinelibrary.wiley.com/doi/full/10.1111/j.1365-2125.2011.04005.x
#' }
simulate_ceftriaxone_mc_pta_fcmin_mic_ratio <- function(PATID, AGE, HEIGHT, WEIGHT, GENDER, CREATININE, MODEL, CMINPERMIC, CRCLCAP, REGIMENS) {
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
  assert_number(CREATININE,
    lower = 0.01,
    upper = 15
  )
  assert_string(MODEL)
  assert_choice(MODEL, c("Garot et al. (2011) - ICU, septic patients"))
  assert_number(CMINPERMIC,
    lower = 0.1,
    upper = 50
  )
  assert_string(CRCLCAP)
  assert_choice(CRCLCAP, c("No cap", "120 ml/min", "130 ml/min", "140 ml/min", "150 ml/min"))
  ## API call
  simulate("ceftriaxone-mc-pta-fcmin-mic-ratio", PATID = PATID, AGE = AGE, HEIGHT = HEIGHT, WEIGHT = WEIGHT, GENDER = GENDER, CREATININE = CREATININE, MODEL = MODEL, CMINPERMIC = CMINPERMIC, CRCLCAP = CRCLCAP, REGIMENS = REGIMENS)
}
