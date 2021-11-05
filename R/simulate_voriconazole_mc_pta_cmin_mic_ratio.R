#' Voriconazole » PTA » Cmin/MIC
#' 
#' Voriconazole » Probability of Target Attainment » Total minimum concentration to MIC ratio
#' 
#' \strong{Drug}:
#' Voriconazole
#' 
#' \strong{Method}:
#' Monte Carlo simulation on the probabilities that a specific value of a pharmacodynamic index is achieved in case of different minimum inhibitory concentrations (MIC).
#' 
#' \strong{PK/PD target}:
#' Minimum concentration (mg/L) to minimum inhibitory concentration ratio (mg/L).
#' 
#' @param PATID Patient Identifier. User-provided free text (such as patient id, name or alias) to identify related simulations. Must be provided as string.
#' @param AGE Age. Age of the patient in years. Must be provided as numeric (min. 18, max. 120 year).
#' @param HEIGHT Height. Height of the patient. Must be provided as numeric (min. 100, max. 250 cm).
#' @param WEIGHT Weight. Actual body weight of the patient. Must be provided as numeric (min. 20, max. 500 kg).
#' @param GENDER Sex. Patient's sex for clinical decision-making. Must be provided as string ('Male' or 'Female').
#' @param MODEL Model for population of interest. Pharmacokinetic model to be used for specific patient type during simulations. Must be provided as string ('Pascual et al. (2012) - patients with invasive fungal infections').
#' @param RIFAMPICIN Concurrent rifampicin. Presence of rifampicin coadministration. Must be provided as string ('No' or 'Yes').
#' @param SEVEREHEPATICCHOLESTASIS Severe hepatic cholestasis. Severe hepatic cholestasis is defined as any liver function tests > 20 times the upper limit of normal (e.g. alkaline phosphatase, AST, ALT or GGT). Must be provided as string ('No' or 'Yes').
#' @param CMINPERMIC Minimum concentration to MIC ratio target. The PK/PD target can be provided as minimum concentration to minimum inhibitory concentration ratio (Cmin/MIC). Must be provided as numeric (min. 0.1, max. 50 ).
#' @param ORALORIVORKGREGIMENS Dosing Regimens. List of dosing regimens to be used in simulating target attainment, from which the dosing regimen with the smallest absolute difference from the desired target will be automatically selected. Must be provided as list of 1-8 'KGREGIMEN', 'REGIMEN' or 'ORALREGIMEN' values. Use the \code{regimen} helper function to define the REGIMEN values.
#' 
#' @examples \dontrun{
#' simulate_voriconazole_mc_pta_cmin_mic_ratio(PATID = "Anonymous", 
#'     AGE = 35, HEIGHT = 155, 
#'     WEIGHT = 55, GENDER = "Female", 
#'     MODEL = "Pascual et al. (2012) - patients with invasive fungal infections", 
#'     RIFAMPICIN = "No", 
#'     SEVEREHEPATICCHOLESTASIS = "No", 
#'     CMINPERMIC = 3, ORALORIVORKGREGIMENS = list(
#'         list(set = "KGREGIMEN", 
#'             DOSEPERKG = 4, 
#'             INTERVAL = 12, 
#'             TINF = 1.5), 
#'         list(set = "REGIMEN", 
#'             DOSE = 200, 
#'             INTERVAL = 12, 
#'             TINF = 1.5), 
#'         list(set = "ORALREGIMEN", 
#'             DOSE = 200, 
#'             INTERVAL = 12)))
#' }
#' 
#' @references \itemize{
#'   \item Pascual, Andres et al. (2012): Challenging recommended oral and intravenous voriconazole doses for improved efficacy and safety: population pharmacokinetics-based analysis of adult patients with invasive fungal infections. In. Clinical Infectious Diseases. https://pubmed.ncbi.nlm.nih.gov/22610925
#' }
simulate_voriconazole_mc_pta_cmin_mic_ratio <- function(PATID, AGE, HEIGHT, WEIGHT, GENDER, MODEL, RIFAMPICIN, SEVEREHEPATICCHOLESTASIS, CMINPERMIC, ORALORIVORKGREGIMENS) {
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
  assert_choice(MODEL, c("Pascual et al. (2012) - patients with invasive fungal infections"))
  assert_string(RIFAMPICIN)
  assert_choice(RIFAMPICIN, c("No", "Yes"))
  assert_string(SEVEREHEPATICCHOLESTASIS)
  assert_choice(SEVEREHEPATICCHOLESTASIS, c("No", "Yes"))
  assert_number(CMINPERMIC,
    lower = 0.1,
    upper = 50
  )
  ## API call
  simulate("voriconazole-mc-pta-cmin-mic-ratio", PATID = PATID, AGE = AGE, HEIGHT = HEIGHT, WEIGHT = WEIGHT, GENDER = GENDER, MODEL = MODEL, RIFAMPICIN = RIFAMPICIN, SEVEREHEPATICCHOLESTASIS = SEVEREHEPATICCHOLESTASIS, CMINPERMIC = CMINPERMIC, ORALORIVORKGREGIMENS = ORALORIVORKGREGIMENS)
}
