#' Amikacin » CFR » AUC/MIC
#' 
#' Amikacin » Cumulative Fraction of Response » AUC to MIC ratio
#' 
#' \strong{Drug}:
#' Amikacin
#' 
#' \strong{Method}:
#' Monte Carlo simulation on the proportion of the population achieving a certain pharmacodynamic index value, given the minimum inhibitory concentration (MIC) distribution of the target microorganism(s).
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
#' @param AUCPERMIC AUC to MIC ratio target. The PK/PD target can be provided as 24 hour area under the concentration-time curve to minimum inhibitory concentration ratio (AUC/MIC). Must be provided as numeric (min. 10, max. 2000 ).
#' @param REGIMENS Dosing Regimens. List of dosing regimens to be used in simulating target attainment, from which the dosing regimen with the smallest absolute difference from the desired target will be automatically selected. Must be provided as list of 1-8 'REGIMEN' values. Use the \code{regimen} helper function to define the REGIMEN values.
#' @param MICDISTR MIC distribution. Non-cumulative MIC distribution of organism for the given antibiotic at the specified MICs provided as percentages with a sum of 100%. MIC distribution of amikacin and comparator antimicrobial agents when tested against Pseudomonas isolates in the SENTRY program collected during 2013-2019 (mg/L). Dataset provided by JMI Laboratories and the SENTRY Antimicrobial Surveillance Program, available at <a href="http://sentry-mvp.jmilabs.com" target="_new">sentry-mvp.jmilabs.com</a>. Must be provided as set.
#' 
#' @examples \dontrun{
#' simulate_amikacin_mc_cfr_auc_mic_ratio(PATID = "Anonymous", 
#'     AGE = 75, HEIGHT = 160, 
#'     WEIGHT = 105, GENDER = "Male", 
#'     MODEL = "Saez Fernandez et al. (2019) - General ward", 
#'     CREATININE = 1, AUCPERMIC = 50, 
#'     REGIMENS = list(list(
#'         DOSE = 300, INTERVAL = 6, 
#'         TINF = 0.5, set = "REGIMEN")), 
#'     MICDISTR = list("MICDISTR-0.25" = list(
#'         0.8), "MICDISTR-0.5" = list(
#'         2.7), "MICDISTR-1" = list(
#'         6.4), "MICDISTR-2" = list(
#'         29.8), "MICDISTR-4" = list(
#'         34.3), "MICDISTR-8" = list(
#'         13.6), "MICDISTR-16" = list(
#'         4.8), "MICDISTR-32" = list(
#'         2.3), "MICDISTR-64" = list(
#'         5.3)))
#' }
#' 
#' @references \itemize{
#'   \item Saez Fernandez et al. (2019): Evaluation of renal function equations to predict amikacin clearance. In. Expert Review of Clinical Pharmacology. https://www.tandfonline.com/doi/full/10.1080/17512433.2019.1637253
#' }
simulate_amikacin_mc_cfr_auc_mic_ratio <- function(PATID, AGE, HEIGHT, WEIGHT, GENDER, MODEL, CREATININE, AUCPERMIC, REGIMENS, MICDISTR) {
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
  assert_number(AUCPERMIC,
    lower = 10,
    upper = 2000
  )
  ## API call
  simulate("amikacin-mc-cfr-auc-mic-ratio", PATID = PATID, AGE = AGE, HEIGHT = HEIGHT, WEIGHT = WEIGHT, GENDER = GENDER, MODEL = MODEL, CREATININE = CREATININE, AUCPERMIC = AUCPERMIC, REGIMENS = REGIMENS, MICDISTR = MICDISTR)
}
