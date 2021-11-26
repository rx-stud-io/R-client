#' Cefepime » Bayesian » fCmin/MIC
#' 
#' Cefepime » Bayesian adaptive dosing » Free minimum concentration to MIC ratio
#' 
#' \strong{Drug}:
#' Cefepime
#' 
#' \strong{Method}:
#' Estimate the pharmacokinetic parameters of the patient from past concentrations with Bayesian inverse modeling, then use that information to predict the steady state concentrations for multiple dosing regimens and select the optimal one, with regard to the target pharmacodynamic index.
#' 
#' \strong{PK/PD target}:
#' Free minimum concentration (mg/L) to minimum inhibitory concentration ratio (mg/L).
#' 
#' @param PATID Patient Identifier. User-provided free text (such as patient id, name or alias) to identify related simulations. Must be provided as string.
#' @param AGE Age. Age of the patient in years. Must be provided as numeric (min. 18, max. 120 year).
#' @param HEIGHT Height. Height of the patient. Must be provided as numeric (min. 100, max. 250 cm).
#' @param WEIGHT Weight. Actual body weight of the patient. Must be provided as numeric (min. 20, max. 500 kg).
#' @param GENDER Sex. Patient's sex for clinical decision-making. Must be provided as string ('Male' or 'Female').
#' @param MODEL Model for population of interest. Pharmacokinetic model to be used for specific patient type during simulations. Must be provided as string ('Nicasio et al. (2009) - ICU' or 'Tam et al. (2003) - General ward').
#' @param MIC MIC. Minimum Inhibitory Concentration (MIC). Must be provided as numeric (min. 0.01, max. 1024 mg/L).
#' @param CMINPERMIC Minimum concentration to MIC ratio target. The PK/PD target can be provided as minimum concentration to minimum inhibitory concentration ratio (Cmin/MIC). Must be provided as numeric (min. 0.1, max. 50 ).
#' @param CRCLCAP Capping Creatinine Clearance. Whether to use capping for creatinine clearance. Must be provided as string ('No cap', '120 ml/min', '130 ml/min', '140 ml/min' or '150 ml/min').
#' @param HISTORY Historical Records.  Must be provided as list of 3-48 'HISTCREATININE', 'HISTDOSE' or 'HISTCONCENTRATION' values.
#' @param REGIMENS Dosing Regimens. List of dosing regimens to be used in simulating target attainment, from which the dosing regimen with the smallest absolute difference from the desired target will be automatically selected. Must be provided as list of 1-20 'REGIMEN' values. Use the \code{regimen} helper function to define the REGIMEN values.
#' 
#' @examples \dontrun{
#' simulate_cefepime_bayesian_fcmin_mic_ratio(PATID = "Anonymous", 
#'     AGE = 65, HEIGHT = 175, 
#'     WEIGHT = 75, GENDER = "Male", 
#'     MODEL = "Nicasio et al. (2009) - ICU", 
#'     MIC = 1, CMINPERMIC = 4, 
#'     CRCLCAP = "No cap", 
#'     HISTORY = list(list(
#'         DATETIME = structure(1601870400, class = c("POSIXct", 
#'         "POSIXt"), tzone = ""), 
#'         DOSE = 500, TINF = 2, 
#'         set = "HISTDOSE"), 
#'         list(DATETIME = structure(1601881200, class = c("POSIXct", 
#'         "POSIXt"), tzone = ""), 
#'             DOSE = 600, 
#'             TINF = 1, 
#'             set = "HISTDOSE"), 
#'         list(DATETIME = structure(1601899200, class = c("POSIXct", 
#'         "POSIXt"), tzone = ""), 
#'             DOSE = 1200, 
#'             TINF = 2, 
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
#'         list(DATETIME = structure(1601888400, class = c("POSIXct", 
#'         "POSIXt"), tzone = ""), 
#'             CONCENTRATION = 10, 
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
#'   \item K. Soetaert, T. Petzoldt (2010): Inverse Modelling, Sensitivity and Monte Carlo Analysis in R Using Package FME. In. Journal of Statistical Software. https://www.jstatsoft.org/article/view/v033i03
#' }
#' @export 
#' @importFrom checkmate assert_number assert_string assert_choice
simulate_cefepime_bayesian_fcmin_mic_ratio <- function(PATID, AGE, HEIGHT, WEIGHT, GENDER, MODEL, MIC, CMINPERMIC, CRCLCAP, HISTORY, REGIMENS) {
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
  assert_choice(MODEL, c("Nicasio et al. (2009) - ICU", "Tam et al. (2003) - General ward"))
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
  simulate("cefepime-bayesian-fcmin-mic-ratio", PATID = PATID, AGE = AGE, HEIGHT = HEIGHT, WEIGHT = WEIGHT, GENDER = GENDER, MODEL = MODEL, MIC = MIC, CMINPERMIC = CMINPERMIC, CRCLCAP = CRCLCAP, HISTORY = HISTORY, REGIMENS = REGIMENS)
}
