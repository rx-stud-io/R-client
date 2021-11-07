#' Meropenem » CFR » \% fT > MIC
#' 
#' Meropenem » Cumulative Fraction of Response » Percent time of free concentration above MIC
#' 
#' \strong{Drug}:
#' Meropenem
#' 
#' \strong{Method}:
#' Monte Carlo simulation on the proportion of the population achieving a certain pharmacodynamic index value, given the minimum inhibitory concentration (MIC) distribution of the target microorganism(s).
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
#' @param MODEL Model for population of interest. Pharmacokinetic model to be used for specific patient type during simulations. Must be provided as string ('Crandon et al. (2011) - ICU', 'Li, C. et. al. (2006) - General ward' or 'Doh, K. et al. (2010) - Burn patients').
#' @param EDEMA Edema. Presence of edema in case of Burn Patients. Must be provided as string ('No' or 'Yes').
#' @param PCTABOVEMIC Percent of time target that the drug concentration is above MIC. The PK/PD target can be provided as the percent of time that the drug concentration is above the minimum inhibitory concentration (\% T > MIC). Must be provided as numeric (min. 5, max. 100 \%).
#' @param CRCLCAP Capping Creatinine Clearance. Whether to use capping for creatinine clearance. Must be provided as string ('No cap', '120 ml/min', '130 ml/min', '140 ml/min' or '150 ml/min').
#' @param REGIMENS Dosing Regimens. List of dosing regimens to be used in simulating target attainment, from which the dosing regimen with the smallest absolute difference from the desired target will be automatically selected. Must be provided as list of 1-8 'REGIMEN' values. Use the \code{regimen} helper function to define the REGIMEN values.
#' @param MICDISTR MIC distribution. Non-cumulative MIC distribution of organism for the given antibiotic at the specified MICs provided as percentages with a sum of 100\%. MIC distribution of meropenem and comparator antimicrobial agents when tested against Pseudomonas isolates in the SENTRY program collected during 2019 (mg/L). Dataset provided by JMI Laboratories and the SENTRY Antimicrobial Surveillance Program, available at <a href="http://sentry-mvp.jmilabs.com" target="_new">sentry-mvp.jmilabs.com</a>. Must be provided as set.
#' 
#' @examples \dontrun{
#' simulate_meropenem_mc_cfr_ftime_above_mic(PATID = "Anonymous", 
#'     AGE = 65, HEIGHT = 175, 
#'     WEIGHT = 75, GENDER = "Male", 
#'     CREATININE = 1, MODEL = "Crandon et al. (2011) - ICU", 
#'     EDEMA = "No", PCTABOVEMIC = 40, 
#'     CRCLCAP = "No cap", 
#'     REGIMENS = list(list(
#'         DOSE = 500, INTERVAL = 6, 
#'         TINF = 0.5, set = "REGIMEN"), 
#'         list(DOSE = 1000, 
#'             INTERVAL = 8, 
#'             TINF = 0.5, 
#'             set = "REGIMEN"), 
#'         list(DOSE = 2000, 
#'             INTERVAL = 8, 
#'             TINF = 0.5, 
#'             set = "REGIMEN"), 
#'         list(DOSE = 2000, 
#'             INTERVAL = 8, 
#'             TINF = 3, 
#'             set = "REGIMEN")), 
#'     MICDISTR = list("MICDISTR-0.015625" = list(
#'         0.6), "MICDISTR-0.03125" = list(
#'         1.2), "MICDISTR-0.0625" = list(
#'         6), "MICDISTR-0.125" = list(
#'         10.5), "MICDISTR-0.25" = list(
#'         20.4), "MICDISTR-0.5" = list(
#'         20.1), "MICDISTR-1" = list(
#'         12.7), "MICDISTR-2" = list(
#'         7.1), "MICDISTR-4" = list(
#'         5.6), "MICDISTR-8" = list(
#'         4.6), "MICDISTR-16" = list(
#'         5.4), "MICDISTR-32" = list(
#'         2.7), "MICDISTR-64" = list(
#'         3.2)))
#' }
#' 
#' @references \itemize{
#'   \item Jared L Crandon et al. (2011): Optimization of meropenem dosage in the critically ill population based on renal function. In. Intensive Care Medicine. https://pubmed.ncbi.nlm.nih.gov/21136037
#'   \item Li, C. et. al (2006): Population Pharmacokinetic Analysis and Dosing Regimen Optimization of Meropenem in Adult Patients. In. The Journal of Clinical Pharmacology. https://accp1.onlinelibrary.wiley.com/doi/10.1177/0091270006291035
#'   \item Doh, K. et al. (2010): Population pharmacokinetics of meropenem in burn patients. In. Journal of Antimicrobial Chemotherapy. https://academic.oup.com/jac/article/65/11/2428/762112
#' }
#' @export 
#' @importFrom checkmate assert_number assert_string assert_choice
simulate_meropenem_mc_cfr_ftime_above_mic <- function(PATID, AGE, HEIGHT, WEIGHT, GENDER, CREATININE, MODEL, EDEMA, PCTABOVEMIC, CRCLCAP, REGIMENS, MICDISTR) {
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
  assert_choice(MODEL, c("Crandon et al. (2011) - ICU", "Li, C. et. al. (2006) - General ward", "Doh, K. et al. (2010) - Burn patients"))
  assert_string(EDEMA)
  assert_choice(EDEMA, c("No", "Yes"))
  assert_number(PCTABOVEMIC,
    lower = 5,
    upper = 100
  )
  assert_string(CRCLCAP)
  assert_choice(CRCLCAP, c("No cap", "120 ml/min", "130 ml/min", "140 ml/min", "150 ml/min"))
  ## API call
  simulate("meropenem-mc-cfr-ftime-above-mic", PATID = PATID, AGE = AGE, HEIGHT = HEIGHT, WEIGHT = WEIGHT, GENDER = GENDER, CREATININE = CREATININE, MODEL = MODEL, EDEMA = EDEMA, PCTABOVEMIC = PCTABOVEMIC, CRCLCAP = CRCLCAP, REGIMENS = REGIMENS, MICDISTR = MICDISTR)
}
