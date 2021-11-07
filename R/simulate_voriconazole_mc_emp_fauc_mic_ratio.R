#' Voriconazole » Empiric » fAUC/MIC
#' 
#' Voriconazole » Empiric therapy » fAUC to MIC ratio
#' 
#' \strong{Drug}:
#' Voriconazole
#' 
#' \strong{Method}:
#' Simulate concentrations for multiple dosing regimens and select the optimal one, with regard to the target pharmacodynamic index.
#' 
#' \strong{PK/PD target}:
#' 24 hour area under the free concentration-time curve to minimum inhibitory concentration ratio.
#' 
#' @param PATID Patient Identifier. User-provided free text (such as patient id, name or alias) to identify related simulations. Must be provided as string.
#' @param AGE Age. Age of the patient in years. Must be provided as numeric (min. 18, max. 120 year).
#' @param HEIGHT Height. Height of the patient. Must be provided as numeric (min. 100, max. 250 cm).
#' @param WEIGHT Weight. Actual body weight of the patient. Must be provided as numeric (min. 20, max. 500 kg).
#' @param GENDER Sex. Patient's sex for clinical decision-making. Must be provided as string ('Male' or 'Female').
#' @param MODEL Model for population of interest. Pharmacokinetic model to be used for specific patient type during simulations. Must be provided as string ('Pascual et al. (2012) - patients with invasive fungal infections').
#' @param RIFAMPICIN Concurrent rifampicin. Presence of rifampicin coadministration. Must be provided as string ('No' or 'Yes').
#' @param SEVEREHEPATICCHOLESTASIS Severe hepatic cholestasis. Severe hepatic cholestasis is defined as any liver function tests > 20 times the upper limit of normal (e.g. alkaline phosphatase, AST, ALT or GGT). Must be provided as string ('No' or 'Yes').
#' @param AUCPERMIC AUC to MIC ratio target. The PK/PD target can be provided as 24 hour area under the concentration-time curve to minimum inhibitory concentration ratio (AUC/MIC). Must be provided as numeric (min. 10, max. 2000 ).
#' @param LOADINGDOSE Loading dose. Loading dose is desired or not. Must be provided as string ('No' or 'Yes').
#' @param ORALORIVORKGREGIMENS Dosing Regimens. List of dosing regimens to be used in simulating target attainment, from which the dosing regimen with the smallest absolute difference from the desired target will be automatically selected. Must be provided as list of 1-20 'KGREGIMEN', 'REGIMEN' or 'ORALREGIMEN' values. Use the \code{regimen} helper function to define the REGIMEN values.
#' 
#' @examples \dontrun{
#' simulate_voriconazole_mc_emp_fauc_mic_ratio(PATID = "Anonymous", 
#'     AGE = 35, HEIGHT = 155, 
#'     WEIGHT = 55, GENDER = "Female", 
#'     MODEL = "Pascual et al. (2012) - patients with invasive fungal infections", 
#'     RIFAMPICIN = "No", 
#'     SEVEREHEPATICCHOLESTASIS = "No", 
#'     AUCPERMIC = 25, LOADINGDOSE = "No", 
#'     ORALORIVORKGREGIMENS = list(
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
#' @export 
#' @importFrom checkmate assert_number assert_string assert_choice
simulate_voriconazole_mc_emp_fauc_mic_ratio <- function(PATID, AGE, HEIGHT, WEIGHT, GENDER, MODEL, RIFAMPICIN, SEVEREHEPATICCHOLESTASIS, AUCPERMIC, LOADINGDOSE, ORALORIVORKGREGIMENS) {
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
  assert_number(AUCPERMIC,
    lower = 10,
    upper = 2000
  )
  assert_string(LOADINGDOSE)
  assert_choice(LOADINGDOSE, c("No", "Yes"))
  ## API call
  simulate("voriconazole-mc-emp-fauc-mic-ratio", PATID = PATID, AGE = AGE, HEIGHT = HEIGHT, WEIGHT = WEIGHT, GENDER = GENDER, MODEL = MODEL, RIFAMPICIN = RIFAMPICIN, SEVEREHEPATICCHOLESTASIS = SEVEREHEPATICCHOLESTASIS, AUCPERMIC = AUCPERMIC, LOADINGDOSE = LOADINGDOSE, ORALORIVORKGREGIMENS = ORALORIVORKGREGIMENS)
}
