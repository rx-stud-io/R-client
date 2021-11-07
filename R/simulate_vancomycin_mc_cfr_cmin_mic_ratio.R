#' Vancomycin » CFR » Cmin/MIC
#' 
#' Vancomycin » Cumulative Fraction of Response » Total minimum concentration to MIC ratio
#' 
#' \strong{Drug}:
#' Vancomycin
#' 
#' \strong{Method}:
#' Monte Carlo simulation on the proportion of the population achieving a certain pharmacodynamic index value, given the minimum inhibitory concentration (MIC) distribution of the target microorganism(s).
#' 
#' \strong{PK/PD target}:
#' Minimum concentration (mg/L) to minimum inhibitory concentration ratio (mg/L).
#' 
#' @param PATID Patient Identifier. User-provided free text (such as patient id, name or alias) to identify related simulations. Must be provided as string.
#' @param AGE Age. Age of the patient in years. Must be provided as numeric (min. 18, max. 120 year).
#' @param HEIGHT Height. Height of the patient. Must be provided as numeric (min. 100, max. 250 cm).
#' @param WEIGHT Weight. Actual body weight of the patient. Must be provided as numeric (min. 20, max. 500 kg).
#' @param GENDER Sex. Patient's sex for clinical decision-making. Must be provided as string ('Male' or 'Female').
#' @param CREATININE Creatinine. Serum creatinine. Must be provided as numeric (min. 0.01, max. 15 mg/dL).
#' @param MODEL Model for population of interest. Pharmacokinetic model to be used for specific patient type during simulations. Must be provided as string ('Goti et al. (2018) - Patients NOT undergoing hemodialysis', 'Goti et al. (2018) - Patients undergoing hemodialysis', 'Buelga et al. (2005) - Patients with hematological malignancies', 'Buelga et al. (2005) - AML patients, Model 1' or 'Buelga et al. (2005) - AML patients, Model 2').
#' @param CMINPERMIC Minimum concentration to MIC ratio target. The PK/PD target can be provided as minimum concentration to minimum inhibitory concentration ratio (Cmin/MIC). Must be provided as numeric (min. 0.1, max. 50 ).
#' @param CRCLCAP Capping Creatinine Clearance. Whether to use capping for creatinine clearance. Must be provided as string ('No cap', '120 ml/min', '130 ml/min', '140 ml/min' or '150 ml/min').
#' @param REGIMENS Dosing Regimens. List of dosing regimens to be used in simulating target attainment, from which the dosing regimen with the smallest absolute difference from the desired target will be automatically selected. Must be provided as list of 1-8 'REGIMEN' values. Use the \code{regimen} helper function to define the REGIMEN values.
#' @param MICDISTR MIC distribution. Non-cumulative MIC distribution of organism for the given antibiotic at the specified MICs provided as percentages with a sum of 100\%. MIC distribution of vancomycin and comparator antimicrobial agents when tested against Staphylococcus aureus isolates in the SENTRY program collected during 2019 (mg/L). Dataset provided by JMI Laboratories and the SENTRY Antimicrobial Surveillance Program, available at <a href="http://sentry-mvp.jmilabs.com" target="_new">sentry-mvp.jmilabs.com</a>. Must be provided as set.
#' 
#' @examples \dontrun{
#' simulate_vancomycin_mc_cfr_cmin_mic_ratio(PATID = "Anonymous", 
#'     AGE = 75, HEIGHT = 160, 
#'     WEIGHT = 105, GENDER = "Male", 
#'     CREATININE = 1, MODEL = "Goti et al. (2018) - Patients undergoing hemodialysis", 
#'     CMINPERMIC = 20, 
#'     CRCLCAP = "No cap", 
#'     REGIMENS = list(list(
#'         DOSE = 1000, 
#'         INTERVAL = 16, 
#'         TINF = 1, set = "REGIMEN")), 
#'     MICDISTR = list("MICDISTR-0.0625" = list(
#'         0), "MICDISTR-0.125" = list(
#'         0), "MICDISTR-0.25" = list(
#'         0.3), "MICDISTR-0.5" = list(
#'         53.4), "MICDISTR-1" = list(
#'         45.8), "MICDISTR-2" = list(
#'         0.5), "MICDISTR-4" = list(
#'         0), "MICDISTR-8" = list(
#'         0)))
#' }
#' 
#' @references \itemize{
#'   \item Goti et al. (2018): Hospitalized Patients With and Without Hemodialysis Have Markedly Different Vancomycin Pharmacokinetics: A Population Pharmacokinetic Model-Based Analysis. In. Therapeutic Drug Monitoring. https://pubmed.ncbi.nlm.nih.gov/29470227/
#'   \item Buelga, Dolores Santos et al. (2005): Population Pharmacokinetic Analysis of Vancomycin in Patients with Hematological Malignancies. In. Antimicrobial Agents and Chemotherapy. https://aac.asm.org/content/49/12/4934
#' }
#' @export 
#' @importFrom checkmate assert_number assert_string assert_choice
simulate_vancomycin_mc_cfr_cmin_mic_ratio <- function(PATID, AGE, HEIGHT, WEIGHT, GENDER, CREATININE, MODEL, CMINPERMIC, CRCLCAP, REGIMENS, MICDISTR) {
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
  assert_choice(MODEL, c("Goti et al. (2018) - Patients NOT undergoing hemodialysis", "Goti et al. (2018) - Patients undergoing hemodialysis", "Buelga et al. (2005) - Patients with hematological malignancies", "Buelga et al. (2005) - AML patients, Model 1", "Buelga et al. (2005) - AML patients, Model 2"))
  assert_number(CMINPERMIC,
    lower = 0.1,
    upper = 50
  )
  assert_string(CRCLCAP)
  assert_choice(CRCLCAP, c("No cap", "120 ml/min", "130 ml/min", "140 ml/min", "150 ml/min"))
  ## API call
  simulate("vancomycin-mc-cfr-cmin-mic-ratio", PATID = PATID, AGE = AGE, HEIGHT = HEIGHT, WEIGHT = WEIGHT, GENDER = GENDER, CREATININE = CREATININE, MODEL = MODEL, CMINPERMIC = CMINPERMIC, CRCLCAP = CRCLCAP, REGIMENS = REGIMENS, MICDISTR = MICDISTR)
}
