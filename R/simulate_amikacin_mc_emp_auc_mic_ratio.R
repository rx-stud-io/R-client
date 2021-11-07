#' Amikacin » Empiric » AUC/MIC
#' 
#' Amikacin » Empiric therapy » AUC to MIC ratio
#' 
#' \strong{Drug}:
#' Amikacin
#' 
#' \strong{Method}:
#' Simulate concentrations for multiple dosing regimens and select the optimal one, with regard to the target pharmacodynamic index.
#' 
#' \strong{PK/PD target}:
#' 24 hour area under the concentration-time curve to minimum inhibitory concentration ratio.
#' 
#' @param PATID Patient Identifier. User-provided free text (such as patient id, name or alias) to identify related simulations. Must be provided as string.
#' @param AGE Age. Age of the patient in years. Must be provided as numeric (min. 18, max. 120 year).
#' @param HEIGHT Height. Height of the patient. Must be provided as numeric (min. 100, max. 250 cm).
#' @param WEIGHT Weight. Actual body weight of the patient. Must be provided as numeric (min. 20, max. 500 kg).
#' @param GENDER Sex. Patient's sex for clinical decision-making. Must be provided as string ('Male' or 'Female').
#' @param MODEL Model for population of interest. Pharmacokinetic model to be used for specific patient type during simulations. Must be provided as string ('Saez Fernandez et al. (2019) - General ward').
#' @param CREATININE Creatinine. Serum creatinine. Must be provided as numeric (min. 0.01, max. 15 mg/dL).
#' @param MIC MIC. Minimum Inhibitory Concentration (MIC). Must be provided as numeric (min. 0.01, max. 1024 mg/L).
#' @param AUCPERMIC AUC to MIC ratio target. The PK/PD target can be provided as 24 hour area under the concentration-time curve to minimum inhibitory concentration ratio (AUC/MIC). For selecting an appropriate target please refer to the dosing guideline of your institution, e.g. <a href="http://med.stanford.edu/bugsanddrugs/guidebook/_jcr_content/main/panel_builder_584648957/panel_0/download_1194988017/file.res/Aminoglycoside\%20Dosing\%20Guide\%202019-05-20.pdf" target="_new">Stanford Health Care Aminoglycoside Dosing Guide</a>. Must be provided as numeric (min. 10, max. 2000 ).
#' @param LOADINGDOSE Loading dose. Loading dose is desired or not. Must be provided as string ('No' or 'Yes').
#' @param REGIMENS Dosing Regimens. List of dosing regimens to be used in simulating target attainment, from which the dosing regimen with the smallest absolute difference from the desired target will be automatically selected. Must be provided as list of 1-20 'REGIMEN' values. Use the \code{regimen} helper function to define the REGIMEN values.
#' 
#' @examples \dontrun{
#' simulate_amikacin_mc_emp_auc_mic_ratio(PATID = "Anonymous", 
#'     AGE = 65, HEIGHT = 175, 
#'     WEIGHT = 75, GENDER = "Male", 
#'     MODEL = "Saez Fernandez et al. (2019) - General ward", 
#'     CREATININE = 1, MIC = 1, 
#'     AUCPERMIC = 80, LOADINGDOSE = "No", 
#'     REGIMENS = list(list(
#'         set = "REGIMEN", 
#'         DOSE = 100, INTERVAL = 8, 
#'         TINF = 0.5), 
#'         list(set = "REGIMEN", 
#'             DOSE = 100, 
#'             INTERVAL = 12, 
#'             TINF = 0.5), 
#'         list(set = "REGIMEN", 
#'             DOSE = 125, 
#'             INTERVAL = 8, 
#'             TINF = 0.5), 
#'         list(set = "REGIMEN", 
#'             DOSE = 125, 
#'             INTERVAL = 12, 
#'             TINF = 0.5), 
#'         list(set = "REGIMEN", 
#'             DOSE = 150, 
#'             INTERVAL = 8, 
#'             TINF = 0.5), 
#'         list(set = "REGIMEN", 
#'             DOSE = 150, 
#'             INTERVAL = 12, 
#'             TINF = 0.5), 
#'         list(set = "REGIMEN", 
#'             DOSE = 175, 
#'             INTERVAL = 8, 
#'             TINF = 0.5), 
#'         list(set = "REGIMEN", 
#'             DOSE = 175, 
#'             INTERVAL = 12, 
#'             TINF = 0.5), 
#'         list(set = "REGIMEN", 
#'             DOSE = 200, 
#'             INTERVAL = 8, 
#'             TINF = 0.5), 
#'         list(set = "REGIMEN", 
#'             DOSE = 200, 
#'             INTERVAL = 12, 
#'             TINF = 0.5), 
#'         list(set = "REGIMEN", 
#'             DOSE = 400, 
#'             INTERVAL = 24, 
#'             TINF = 0.5), 
#'         list(set = "REGIMEN", 
#'             DOSE = 400, 
#'             INTERVAL = 36, 
#'             TINF = 0.5), 
#'         list(set = "REGIMEN", 
#'             DOSE = 500, 
#'             INTERVAL = 24, 
#'             TINF = 0.5), 
#'         list(set = "REGIMEN", 
#'             DOSE = 500, 
#'             INTERVAL = 36, 
#'             TINF = 0.5), 
#'         list(set = "REGIMEN", 
#'             DOSE = 600, 
#'             INTERVAL = 24, 
#'             TINF = 0.5), 
#'         list(set = "REGIMEN", 
#'             DOSE = 600, 
#'             INTERVAL = 36, 
#'             TINF = 0.5), 
#'         list(set = "REGIMEN", 
#'             DOSE = 700, 
#'             INTERVAL = 24, 
#'             TINF = 0.5), 
#'         list(set = "REGIMEN", 
#'             DOSE = 700, 
#'             INTERVAL = 36, 
#'             TINF = 0.5), 
#'         list(set = "REGIMEN", 
#'             DOSE = 800, 
#'             INTERVAL = 24, 
#'             TINF = 0.5), 
#'         list(set = "REGIMEN", 
#'             DOSE = 800, 
#'             INTERVAL = 36, 
#'             TINF = 0.5)))
#' }
#' 
#' @references \itemize{
#'   \item Saez Fernandez et al. (2019): Evaluation of renal function equations to predict amikacin clearance. In. Expert Review of Clinical Pharmacology. https://www.tandfonline.com/doi/full/10.1080/17512433.2019.1637253
#' }
#' @export 
#' @importFrom checkmate assert_number assert_string assert_choice
simulate_amikacin_mc_emp_auc_mic_ratio <- function(PATID, AGE, HEIGHT, WEIGHT, GENDER, MODEL, CREATININE, MIC, AUCPERMIC, LOADINGDOSE, REGIMENS) {
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
  assert_number(MIC,
    lower = 0.01,
    upper = 1024
  )
  assert_number(AUCPERMIC,
    lower = 10,
    upper = 2000
  )
  assert_string(LOADINGDOSE)
  assert_choice(LOADINGDOSE, c("No", "Yes"))
  ## API call
  simulate("amikacin-mc-emp-auc-mic-ratio", PATID = PATID, AGE = AGE, HEIGHT = HEIGHT, WEIGHT = WEIGHT, GENDER = GENDER, MODEL = MODEL, CREATININE = CREATININE, MIC = MIC, AUCPERMIC = AUCPERMIC, LOADINGDOSE = LOADINGDOSE, REGIMENS = REGIMENS)
}
