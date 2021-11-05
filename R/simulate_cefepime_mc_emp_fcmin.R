#' Cefepime » Empiric » fCmin
#' 
#' Cefepime » Empiric therapy » Free minimum concentration
#' 
#' \strong{Drug}:
#' Cefepime
#' 
#' \strong{Method}:
#' Simulate concentrations for multiple dosing regimens and select the optimal one, with regard to the target pharmacodynamic index.
#' 
#' \strong{PK/PD target}:
#' Free minimum blood plasma concentration (mg/L).
#' 
#' @param PATID Patient Identifier. User-provided free text (such as patient id, name or alias) to identify related simulations. Must be provided as string.
#' @param AGE Age. Age of the patient in years. Must be provided as numeric (min. 18, max. 120 year).
#' @param HEIGHT Height. Height of the patient. Must be provided as numeric (min. 100, max. 250 cm).
#' @param WEIGHT Weight. Actual body weight of the patient. Must be provided as numeric (min. 20, max. 500 kg).
#' @param GENDER Sex. Patient's sex for clinical decision-making. Must be provided as string ('Male' or 'Female').
#' @param CREATININE Creatinine. Serum creatinine. Must be provided as numeric (min. 0.01, max. 15 mg/dL).
#' @param MODEL Model for population of interest. Pharmacokinetic model to be used for specific patient type during simulations. Must be provided as string ('Nicasio et al. (2009) - ICU' or 'Tam et al. (2003) - General ward').
#' @param MIC MIC. Minimum Inhibitory Concentration (MIC). Must be provided as numeric (min. 0.01, max. 1024 mg/L).
#' @param CMIN Minimum concentration target. The PK/PD target can be provided as minimum blood plasma concentration (Cmin). Must be provided as numeric (min. 0.1, max. 200 mg/L).
#' @param LOADINGDOSE Loading dose. Loading dose is desired or not. Must be provided as string ('No' or 'Yes').
#' @param CRCLCAP Capping Creatinine Clearance. Whether to use capping for creatinine clearance. Must be provided as string ('No cap', '120 ml/min', '130 ml/min', '140 ml/min' or '150 ml/min').
#' @param REGIMENS Dosing Regimens. List of dosing regimens to be used in simulating target attainment, from which the dosing regimen with the smallest absolute difference from the desired target will be automatically selected. Must be provided as list of 1-20 'REGIMEN' values. Use the \code{regimen} helper function to define the REGIMEN values.
#' 
#' @examples \dontrun{
#' simulate_cefepime_mc_emp_fcmin(PATID = "Anonymous", 
#'     AGE = 65, HEIGHT = 175, 
#'     WEIGHT = 75, GENDER = "Male", 
#'     CREATININE = 1, MODEL = "Nicasio et al. (2009) - ICU", 
#'     MIC = 1, CMIN = 17.5, 
#'     LOADINGDOSE = "No", 
#'     CRCLCAP = "No cap", 
#'     REGIMENS = list(list(
#'         DOSE = 750, INTERVAL = 8, 
#'         TINF = 1, set = "REGIMEN"), 
#'         list(DOSE = 750, 
#'             INTERVAL = 12, 
#'             TINF = 1, 
#'             set = "REGIMEN"), 
#'         list(DOSE = 750, 
#'             INTERVAL = 24, 
#'             TINF = 1, 
#'             set = "REGIMEN"), 
#'         list(DOSE = 1000, 
#'             INTERVAL = 8, 
#'             TINF = 1, 
#'             set = "REGIMEN"), 
#'         list(DOSE = 1000, 
#'             INTERVAL = 12, 
#'             TINF = 1, 
#'             set = "REGIMEN"), 
#'         list(DOSE = 1000, 
#'             INTERVAL = 24, 
#'             TINF = 1, 
#'             set = "REGIMEN"), 
#'         list(DOSE = 1250, 
#'             INTERVAL = 8, 
#'             TINF = 1.25, 
#'             set = "REGIMEN"), 
#'         list(DOSE = 1250, 
#'             INTERVAL = 12, 
#'             TINF = 1.25, 
#'             set = "REGIMEN"), 
#'         list(DOSE = 1250, 
#'             INTERVAL = 24, 
#'             TINF = 1.25, 
#'             set = "REGIMEN"), 
#'         list(DOSE = 1500, 
#'             INTERVAL = 8, 
#'             TINF = 1.5, 
#'             set = "REGIMEN"), 
#'         list(DOSE = 1500, 
#'             INTERVAL = 12, 
#'             TINF = 1.5, 
#'             set = "REGIMEN"), 
#'         list(DOSE = 1500, 
#'             INTERVAL = 24, 
#'             TINF = 1.5, 
#'             set = "REGIMEN"), 
#'         list(DOSE = 1750, 
#'             INTERVAL = 8, 
#'             TINF = 1.75, 
#'             set = "REGIMEN"), 
#'         list(DOSE = 1750, 
#'             INTERVAL = 12, 
#'             TINF = 1.75, 
#'             set = "REGIMEN"), 
#'         list(DOSE = 1750, 
#'             INTERVAL = 24, 
#'             TINF = 1.75, 
#'             set = "REGIMEN")))
#' }
#' 
#' @references \itemize{
#'   \item Nicasio et al. (2009): Population Pharmacokinetics of High-Dose, Prolonged-Infusion Cefepime in Adult Critically Ill Patients with Ventilator-Associated Pneumonia. In. Antimicrobial Agents and Chemotherapy. https://pubmed.ncbi.nlm.nih.gov/19188394/
#'   \item Tam et al. (2003): Pharmacokinetics and Pharmacodynamics of Cefepime in Patients with Various Degrees of Renal Function. In. Antimicrobial Agents and Chemotherapy. https://pubmed.ncbi.nlm.nih.gov/12760858/
#' }
simulate_cefepime_mc_emp_fcmin <- function(PATID, AGE, HEIGHT, WEIGHT, GENDER, CREATININE, MODEL, MIC, CMIN, LOADINGDOSE, CRCLCAP, REGIMENS) {
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
  assert_choice(MODEL, c("Nicasio et al. (2009) - ICU", "Tam et al. (2003) - General ward"))
  assert_number(MIC,
    lower = 0.01,
    upper = 1024
  )
  assert_number(CMIN,
    lower = 0.1,
    upper = 200
  )
  assert_string(LOADINGDOSE)
  assert_choice(LOADINGDOSE, c("No", "Yes"))
  assert_string(CRCLCAP)
  assert_choice(CRCLCAP, c("No cap", "120 ml/min", "130 ml/min", "140 ml/min", "150 ml/min"))
  ## API call
  simulate("cefepime-mc-emp-fcmin", PATID = PATID, AGE = AGE, HEIGHT = HEIGHT, WEIGHT = WEIGHT, GENDER = GENDER, CREATININE = CREATININE, MODEL = MODEL, MIC = MIC, CMIN = CMIN, LOADINGDOSE = LOADINGDOSE, CRCLCAP = CRCLCAP, REGIMENS = REGIMENS)
}
