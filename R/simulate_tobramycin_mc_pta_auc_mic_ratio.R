#' Tobramycin » PTA » AUC/MIC
#' 
#' Tobramycin » Probability of Target Attainment » AUC to MIC ratio
#' 
#' \strong{Drug}:
#' Tobramycin
#' 
#' \strong{Method}:
#' Monte Carlo simulation on the probabilities that a specific value of a pharmacodynamic index is achieved in case of different minimum inhibitory concentrations (MIC).
#' 
#' \strong{PK/PD target}:
#' 24 hour area under the concentration-time curve to minimum inhibitory concentration ratio.
#' 
#' @param PATID Patient Identifier. User-provided free text (such as patient id, name or alias) to identify related simulations. Must be provided as string.
#' @param AGE Age. Age of the patient in years. Must be provided as numeric (min. 18, max. 120 year).
#' @param HEIGHT Height. Height of the patient. Must be provided as numeric (min. 100, max. 250 cm).
#' @param WEIGHT Weight. Actual body weight of the patient. Must be provided as numeric (min. 20, max. 500 kg).
#' @param GENDER Sex. Patient's sex for clinical decision-making. Must be provided as string ('Male' or 'Female').
#' @param MODEL Model for population of interest. Pharmacokinetic model to be used for specific patient type during simulations. Must be provided as string ('Pai et al. (2011) - General ward').
#' @param CREATININE Creatinine. Serum creatinine. Must be provided as numeric (min. 0.01, max. 15 mg/dL).
#' @param AUCPERMIC AUC to MIC ratio target. The PK/PD target can be provided as 24 hour area under the concentration-time curve to minimum inhibitory concentration ratio (AUC/MIC). Must be provided as numeric (min. 10, max. 2000 ).
#' @param REGIMENS Dosing Regimens. List of dosing regimens to be used in simulating target attainment, from which the dosing regimen with the smallest absolute difference from the desired target will be automatically selected. Must be provided as list of 1-8 'REGIMEN' values. Use the \code{regimen} helper function to define the REGIMEN values.
#' 
#' @examples \dontrun{
#' simulate_tobramycin_mc_pta_auc_mic_ratio(PATID = "Anonymous", 
#'     AGE = 35, HEIGHT = 155, 
#'     WEIGHT = 55, GENDER = "Female", 
#'     MODEL = "Pai et al. (2011) - General ward", 
#'     CREATININE = 1.0638498, 
#'     AUCPERMIC = 100, 
#'     REGIMENS = list(list(
#'         DOSE = 200, INTERVAL = 16, 
#'         TINF = 0.5, set = "REGIMEN")))
#' }
#' 
#' @references \itemize{
#'   \item Pai et al. (2011): Simplified Estimation of Aminoglycoside Pharmacokinetics in Underweight and Obese Adult Patients. In. Antimicrobial Agents and Chemotherapy. https://aac.asm.org/content/55/9/4006
#' }
#' @export 
#' @importFrom checkmate assert_number assert_string assert_choice
simulate_tobramycin_mc_pta_auc_mic_ratio <- function(PATID, AGE, HEIGHT, WEIGHT, GENDER, MODEL, CREATININE, AUCPERMIC, REGIMENS) {
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
  assert_choice(MODEL, c("Pai et al. (2011) - General ward"))
  assert_number(CREATININE,
    lower = 0.01,
    upper = 15
  )
  assert_number(AUCPERMIC,
    lower = 10,
    upper = 2000
  )
  ## API call
  simulate("tobramycin-mc-pta-auc-mic-ratio", PATID = PATID, AGE = AGE, HEIGHT = HEIGHT, WEIGHT = WEIGHT, GENDER = GENDER, MODEL = MODEL, CREATININE = CREATININE, AUCPERMIC = AUCPERMIC, REGIMENS = REGIMENS)
}
