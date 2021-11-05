#' Amikacin » PTA » Cmax/MIC
#' 
#' Amikacin » Probability of Target Attainment » Total maximum concentration to MIC ratio
#' 
#' \strong{Drug}:
#' Amikacin
#' 
#' \strong{Method}:
#' Monte Carlo simulation on the probabilities that a specific value of a pharmacodynamic index is achieved in case of different minimum inhibitory concentrations (MIC).
#' 
#' \strong{PK/PD target}:
#' Maximum concentration (mg/L) to minimum inhibitory concentration ratio (mg/L).
#' 
#' @param PATID Patient Identifier. User-provided free text (such as patient id, name or alias) to identify related simulations. Must be provided as string.
#' @param AGE Age. Age of the patient in years. Must be provided as numeric (min. 18, max. 120 year).
#' @param HEIGHT Height. Height of the patient. Must be provided as numeric (min. 100, max. 250 cm).
#' @param WEIGHT Weight. Actual body weight of the patient. Must be provided as numeric (min. 20, max. 500 kg).
#' @param GENDER Sex. Patient's sex for clinical decision-making. Must be provided as string ('Male' or 'Female').
#' @param MODEL Model for population of interest. Pharmacokinetic model to be used for specific patient type during simulations. Must be provided as string ('Saez Fernandez et al. (2019) - General ward').
#' @param CREATININE Creatinine. Serum creatinine. Must be provided as numeric (min. 0.01, max. 15 mg/dL).
#' @param CMAXPERMIC Maximum concentration to MIC ratio target. The PK/PD target can be provided as maximum concentration to minimum inhibitory concentration ratio (Cmax/MIC). Must be provided as numeric (min. 0.1, max. 200 ).
#' @param REGIMENS Dosing Regimens. List of dosing regimens to be used in simulating target attainment, from which the dosing regimen with the smallest absolute difference from the desired target will be automatically selected. Must be provided as list of 1-8 'REGIMEN' values. Use the \code{regimen} helper function to define the REGIMEN values.
#' 
#' @examples \dontrun{
#' simulate_amikacin_mc_pta_cmax_mic_ratio(PATID = "Anonymous", 
#'     AGE = 35, HEIGHT = 155, 
#'     WEIGHT = 55, GENDER = "Female", 
#'     MODEL = "Saez Fernandez et al. (2019) - General ward", 
#'     CREATININE = 1.2, 
#'     CMAXPERMIC = 8, REGIMENS = list(
#'         list(DOSE = 200, 
#'             INTERVAL = 6, 
#'             TINF = 0.5, 
#'             set = "REGIMEN")))
#' }
#' 
#' @references \itemize{
#'   \item Saez Fernandez et al. (2019): Evaluation of renal function equations to predict amikacin clearance. In. Expert Review of Clinical Pharmacology. https://www.tandfonline.com/doi/full/10.1080/17512433.2019.1637253
#' }
simulate_amikacin_mc_pta_cmax_mic_ratio <- function(PATID, AGE, HEIGHT, WEIGHT, GENDER, MODEL, CREATININE, CMAXPERMIC, REGIMENS) {
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
  assert_choice(MODEL, c("Saez Fernandez et al. (2019) - General ward"))
  assert_number(CREATININE,
    lower = 0.01,
    upper = 15
  )
  assert_number(CMAXPERMIC,
    lower = 0.1,
    upper = 200
  )
  ## API call
  simulate("amikacin-mc-pta-cmax-mic-ratio", PATID = PATID, AGE = AGE, HEIGHT = HEIGHT, WEIGHT = WEIGHT, GENDER = GENDER, MODEL = MODEL, CREATININE = CREATININE, CMAXPERMIC = CMAXPERMIC, REGIMENS = REGIMENS)
}
