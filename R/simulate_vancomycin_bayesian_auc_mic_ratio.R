#' Vancomycin » Bayesian » AUC/MIC
#' 
#' Vancomycin » Bayesian adaptive dosing » AUC to MIC ratio
#' 
#' \strong{Drug}:
#' Vancomycin
#' 
#' \strong{Method}:
#' Estimate the pharmacokinetic parameters of the patient from past concentrations and creatinine levels with Bayesian inverse modeling, then use that information to predict the steady state concentrations for multiple dosing regimens and select the optimal one, with regard to the target pharmacodynamic index.
#' 
#' \strong{PK/PD target}:
#' 24 hour area under the concentration-time curve to minimum inhibitory concentration ratio.
#' 
#' @param PATID Patient Identifier. User-provided free text (such as patient id, name or alias) to identify related simulations. Must be provided as string.
#' @param AGE Age. Age of the patient in years. Must be provided as numeric (min. 18, max. 120 year).
#' @param HEIGHT Height. Height of the patient. Must be provided as numeric (min. 100, max. 250 cm).
#' @param WEIGHT Weight. Actual body weight of the patient. Must be provided as numeric (min. 20, max. 500 kg).
#' @param GENDER Sex. Patient's sex for clinical decision-making. Must be provided as string ('Male' or 'Female').
#' @param MODEL Model for population of interest. Pharmacokinetic model to be used for specific patient type during simulations. Must be provided as string ('Goti et al. (2018) - Patients NOT undergoing hemodialysis', 'Goti et al. (2018) - Patients undergoing hemodialysis', 'Buelga et al. (2005) - Patients with hematological malignancies', 'Buelga et al. (2005) - AML patients, Model 1' or 'Buelga et al. (2005) - AML patients, Model 2').
#' @param MIC MIC. Minimum Inhibitory Concentration (MIC). Must be provided as numeric (min. 0.01, max. 1024 mg/L).
#' @param AUCPERMIC AUC to MIC ratio target. The PK/PD target can be provided as 24 hour area under the concentration-time curve to minimum inhibitory concentration ratio (AUC/MIC). Must be provided as numeric (min. 10, max. 2000 ).
#' @param TOXICITYINFO Optional information for estimating the probability of nephrotoxicity. This information - if provided - will be used only for estimating the probability of nephrotoxicity. Must be provided as string ('Not applicable', 'General Ward' or 'ICU').
#' @param CRCLCAP Capping Creatinine Clearance. Whether to use capping for creatinine clearance. Must be provided as string ('No cap', '120 ml/min', '130 ml/min', '140 ml/min' or '150 ml/min').
#' @param HISTORY Historical Records.  Must be provided as list of 3-24 'HISTCREATININE', 'HISTDOSE' or 'HISTCONCENTRATION' values.
#' @param REGIMENS Dosing Regimens. List of dosing regimens to be used in simulating target attainment, from which the dosing regimen with the smallest absolute difference from the desired target will be automatically selected. Must be provided as list of 1-20 'REGIMEN' values. Use the \code{regimen} helper function to define the REGIMEN values.
#' 
#' @examples \dontrun{
#' simulate_vancomycin_bayesian_auc_mic_ratio(PATID = "Anonymous", 
#'     AGE = 65, HEIGHT = 175, 
#'     WEIGHT = 75, GENDER = "Male", 
#'     MODEL = "Goti et al. (2018) - Patients NOT undergoing hemodialysis", 
#'     MIC = 1, AUCPERMIC = 400, 
#'     TOXICITYINFO = "ICU", 
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
#'   \item Goti et al. (2018): Hospitalized Patients With and Without Hemodialysis Have Markedly Different Vancomycin Pharmacokinetics: A Population Pharmacokinetic Model-Based Analysis. In. Therapeutic Drug Monitoring. https://pubmed.ncbi.nlm.nih.gov/29470227/
#'   \item K. Soetaert, T. Petzoldt (2010): Inverse Modelling, Sensitivity and Monte Carlo Analysis in R Using Package FME. In. Journal of Statistical Software. https://www.jstatsoft.org/article/view/v033i03
#'   \item Lodise, Thomas P et al. (2009): Relationship between Initial Vancomycin Concentration-Time Profile and Nephrotoxicity among Hospitalized Patients. In. Clinical Infectious Diseases. https://academic.oup.com/cid/article/49/4/507/379286
#'   \item Buelga, Dolores Santos et al. (2005): Population Pharmacokinetic Analysis of Vancomycin in Patients with Hematological Malignancies. In. Antimicrobial Agents and Chemotherapy. https://aac.asm.org/content/49/12/4934
#'   \item Rybak, Michael J et al. (2020): Therapeutic monitoring of vancomycin for serious methicillin-resistant Staphylococcus aureus infections: A revised consensus guideline and review by the American Society of Health-System Pharmacists, the Infectious Diseases Society of America, the Pediatric Infectious Diseases Society, and the Society of Infectious Diseases Pharmacists. In. American Journal of Health-System Pharmacy. https://academic.oup.com/ajhp/article/77/11/835/5810200
#' }
simulate_vancomycin_bayesian_auc_mic_ratio <- function(PATID, AGE, HEIGHT, WEIGHT, GENDER, MODEL, MIC, AUCPERMIC, TOXICITYINFO, CRCLCAP, HISTORY, REGIMENS) {
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
  assert_choice(MODEL, c("Goti et al. (2018) - Patients NOT undergoing hemodialysis", "Goti et al. (2018) - Patients undergoing hemodialysis", "Buelga et al. (2005) - Patients with hematological malignancies", "Buelga et al. (2005) - AML patients, Model 1", "Buelga et al. (2005) - AML patients, Model 2"))
  assert_number(MIC,
    lower = 0.01,
    upper = 1024
  )
  assert_number(AUCPERMIC,
    lower = 10,
    upper = 2000
  )
  assert_string(TOXICITYINFO)
  assert_choice(TOXICITYINFO, c("Not applicable", "General Ward", "ICU"))
  assert_string(CRCLCAP)
  assert_choice(CRCLCAP, c("No cap", "120 ml/min", "130 ml/min", "140 ml/min", "150 ml/min"))
  ## API call
  simulate("vancomycin-bayesian-auc-mic-ratio", PATID = PATID, AGE = AGE, HEIGHT = HEIGHT, WEIGHT = WEIGHT, GENDER = GENDER, MODEL = MODEL, MIC = MIC, AUCPERMIC = AUCPERMIC, TOXICITYINFO = TOXICITYINFO, CRCLCAP = CRCLCAP, HISTORY = HISTORY, REGIMENS = REGIMENS)
}
