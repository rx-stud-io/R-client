#' Piperacillin & Tazobactam » PTA » \% fT > MIC
#' 
#' Piperacillin & Tazobactam » Probability of Target Attainment » Percent time of free concentration above MIC
#' 
#' \strong{Drug}:
#' Piperacillin & Tazobactam
#' 
#' \strong{Method}:
#' Monte Carlo simulation on the probabilities that a specific value of a pharmacodynamic index is achieved in case of different minimum inhibitory concentrations (MIC).
#' 
#' \strong{PK/PD target}:
#' The percent of time that the free concentration is above the minimum inhibitory concentration.
#' 
#' @param PATID Patient Identifier. User-provided free text (such as patient id, name or alias) to identify related simulations. Must be provided as string.
#' @param AGE Age. Age of the patient in years. Must be provided as numeric (min. 18, max. 120 year).
#' @param HEIGHT Height. Height of the patient. Must be provided as numeric (min. 100, max. 250 cm).
#' @param WEIGHT Weight. Actual body weight of the patient. Must be provided as numeric (min. 20, max. 500 kg).
#' @param GENDER Sex. Patient's sex for clinical decision-making. Must be provided as string ('Male' or 'Female').
#' @param CREATININE Creatinine. Serum creatinine. Must be provided as numeric (min. 0.01, max. 15 mg/dL).
#' @param MODEL Model for population of interest. Pharmacokinetic model to be used for specific patient type during simulations. Must be provided as string ('Felton et al. (2014) - ICU' or 'Patel et. al. (2010) - General ward').
#' @param PCTABOVEMIC Percent of time target that the drug concentration is above MIC. The PK/PD target can be provided as the percent of time that the drug concentration is above the minimum inhibitory concentration (\% T > MIC). Must be provided as numeric (min. 5, max. 100 \%).
#' @param CRCLCAP Capping Creatinine Clearance. Whether to use capping for creatinine clearance. Must be provided as string ('No cap', '120 ml/min', '130 ml/min', '140 ml/min' or '150 ml/min').
#' @param REGIMENS Dosing Regimens. List of dosing regimens to be used in simulating target attainment, from which the dosing regimen with the smallest absolute difference from the desired target will be automatically selected. Must be provided as list of 1-8 'REGIMEN' values. Use the \code{regimen} helper function to define the REGIMEN values.
#' 
#' @examples \dontrun{
#' simulate_piperacillin_tazobactam_mc_pta_ftime_above_mic(PATID = "Anonymous", 
#'     AGE = 35, HEIGHT = 155, 
#'     WEIGHT = 55, GENDER = "Female", 
#'     CREATININE = 1.0638498, 
#'     MODEL = "Felton et al. (2014) - ICU", 
#'     PCTABOVEMIC = 50, 
#'     CRCLCAP = "No cap", 
#'     REGIMENS = list(list(
#'         DOSE = 3375, 
#'         INTERVAL = 6, 
#'         TINF = 0.5, set = "REGIMEN")))
#' }
#' 
#' @references \itemize{
#'   \item Felton, T. W. et al. (2014): Individualization of Piperacillin Dosing for Critically Ill Patients: Dosing Software To Optimize Antimicrobial Therapy. In. Antimicrobial Agents and Chemotherapy. https://aac.asm.org/content/58/7/4094
#'   \item Patel, N. et al. (2010): Identification of Optimal Renal Dosage Adjustments for Traditional and Extended-Infusion Piperacillin-Tazobactam Dosing Regimens in Hospitalized Patients. In. Antimicrobial Agents and Chemotherapy. https://aac.asm.org/content/54/1/460
#' }
#' @export 
#' @importFrom checkmate assert_number assert_string assert_choice
simulate_piperacillin_tazobactam_mc_pta_ftime_above_mic <- function(PATID, AGE, HEIGHT, WEIGHT, GENDER, CREATININE, MODEL, PCTABOVEMIC, CRCLCAP, REGIMENS) {
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
  assert_choice(MODEL, c("Felton et al. (2014) - ICU", "Patel et. al. (2010) - General ward"))
  assert_number(PCTABOVEMIC,
    lower = 5,
    upper = 100
  )
  assert_string(CRCLCAP)
  assert_choice(CRCLCAP, c("No cap", "120 ml/min", "130 ml/min", "140 ml/min", "150 ml/min"))
  ## API call
  simulate("piperacillin-tazobactam-mc-pta-ftime-above-mic", PATID = PATID, AGE = AGE, HEIGHT = HEIGHT, WEIGHT = WEIGHT, GENDER = GENDER, CREATININE = CREATININE, MODEL = MODEL, PCTABOVEMIC = PCTABOVEMIC, CRCLCAP = CRCLCAP, REGIMENS = REGIMENS)
}
