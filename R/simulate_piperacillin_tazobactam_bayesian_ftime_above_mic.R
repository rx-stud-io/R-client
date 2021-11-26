#' Piperacillin & Tazobactam » Bayesian » \% fT > MIC
#' 
#' Piperacillin & Tazobactam » Bayesian adaptive dosing » Percent time of free concentration above MIC
#' 
#' \strong{Drug}:
#' Piperacillin & Tazobactam
#' 
#' \strong{Method}:
#' Estimate the pharmacokinetic parameters of the patient from past concentrations with Bayesian inverse modeling, then use that information to predict the steady state concentrations for multiple dosing regimens and select the optimal one, with regard to the target pharmacodynamic index.
#' 
#' \strong{PK/PD target}:
#' The percent of time that the free concentration is above the minimum inhibitory concentration.
#' 
#' @param PATID Patient Identifier. User-provided free text (such as patient id, name or alias) to identify related simulations. Must be provided as string.
#' @param AGE Age. Age of the patient in years. Must be provided as numeric (min. 18, max. 120 year).
#' @param HEIGHT Height. Height of the patient. Must be provided as numeric (min. 100, max. 250 cm).
#' @param WEIGHT Weight. Actual body weight of the patient. Must be provided as numeric (min. 20, max. 500 kg).
#' @param GENDER Sex. Patient's sex for clinical decision-making. Must be provided as string ('Male' or 'Female').
#' @param MODEL Model for population of interest. Pharmacokinetic model to be used for specific patient type during simulations. Must be provided as string ('Felton et al. (2014) - ICU' or 'Patel et. al. (2010) - General ward').
#' @param MIC MIC. Minimum Inhibitory Concentration (MIC). Must be provided as numeric (min. 0.01, max. 1024 mg/L).
#' @param PCTABOVEMIC Percent of time target that the drug concentration is above MIC. The PK/PD target can be provided as the percent of time that the drug concentration is above the minimum inhibitory concentration (\% T > MIC). Must be provided as numeric (min. 5, max. 100 \%).
#' @param CRCLCAP Capping Creatinine Clearance. Whether to use capping for creatinine clearance. Must be provided as string ('No cap', '120 ml/min', '130 ml/min', '140 ml/min' or '150 ml/min').
#' @param HISTORY Historical Records.  Must be provided as list of 3-48 'HISTCREATININE', 'HISTDOSE' or 'HISTCONCENTRATION' values.
#' @param REGIMENS Dosing Regimens. List of dosing regimens to be used in simulating target attainment, from which the dosing regimen with the smallest absolute difference from the desired target will be automatically selected. Must be provided as list of 1-20 'REGIMEN' values. Use the \code{regimen} helper function to define the REGIMEN values.
#' 
#' @examples \dontrun{
#' simulate_piperacillin_tazobactam_bayesian_ftime_above_mic(PATID = "Anonymous", 
#'     AGE = 65, HEIGHT = 175, 
#'     WEIGHT = 75, GENDER = "Male", 
#'     MODEL = "Patel et. al. (2010) - General ward", 
#'     MIC = 8, PCTABOVEMIC = 50, 
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
#'         list(DATETIME = structure(1601888400, class = c("POSIXct", 
#'         "POSIXt"), tzone = ""), 
#'             CONCENTRATION = 30, 
#'             set = "HISTCONCENTRATION"), 
#'         list(DATETIME = structure(1601890200, class = c("POSIXct", 
#'         "POSIXt"), tzone = ""), 
#'             CONCENTRATION = 25, 
#'             set = "HISTCONCENTRATION"), 
#'         list(DATETIME = structure(1601913600, class = c("POSIXct", 
#'         "POSIXt"), tzone = ""), 
#'             CONCENTRATION = 40, 
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
#' 
#' @references \itemize{
#'   \item Felton, T. W. et al. (2014): Individualization of Piperacillin Dosing for Critically Ill Patients: Dosing Software To Optimize Antimicrobial Therapy. In. Antimicrobial Agents and Chemotherapy. https://aac.asm.org/content/58/7/4094
#'   \item Patel, N. et al. (2010): Identification of Optimal Renal Dosage Adjustments for Traditional and Extended-Infusion Piperacillin-Tazobactam Dosing Regimens in Hospitalized Patients. In. Antimicrobial Agents and Chemotherapy. https://aac.asm.org/content/54/1/460
#'   \item K. Soetaert, T. Petzoldt (2010): Inverse Modelling, Sensitivity and Monte Carlo Analysis in R Using Package FME. In. Journal of Statistical Software. https://www.jstatsoft.org/article/view/v033i03
#' }
#' @export 
#' @importFrom checkmate assert_number assert_string assert_choice
simulate_piperacillin_tazobactam_bayesian_ftime_above_mic <- function(PATID, AGE, HEIGHT, WEIGHT, GENDER, MODEL, MIC, PCTABOVEMIC, CRCLCAP, HISTORY, REGIMENS) {
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
  assert_number(PCTABOVEMIC,
    lower = 5,
    upper = 100
  )
  assert_string(CRCLCAP)
  assert_choice(CRCLCAP, c("No cap", "120 ml/min", "130 ml/min", "140 ml/min", "150 ml/min"))
  ## API call
  simulate("piperacillin-tazobactam-bayesian-ftime-above-mic", PATID = PATID, AGE = AGE, HEIGHT = HEIGHT, WEIGHT = WEIGHT, GENDER = GENDER, MODEL = MODEL, MIC = MIC, PCTABOVEMIC = PCTABOVEMIC, CRCLCAP = CRCLCAP, HISTORY = HISTORY, REGIMENS = REGIMENS)
}
