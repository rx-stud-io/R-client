#' Amikacin » CFR » Cmin/MIC
#' 
#' Amikacin » Cumulative Fraction of Response » Total minimum concentration to MIC ratio
#' 
#' @section Drug:
#' Amikacin
#' 
#' @section Method:
#' Monte Carlo simulation on the proportion of the population achieving a certain pharmacodynamic index value, given the minimum inhibitory concentration (MIC) distribution of the target microorganism(s).
#' 
#' @section PK/PD target:
#' Minimum concentration (mg/L) to minimum inhibitory concentration ratio (mg/L).
#' 
#' @param PATID Patient Identifier. User-provided free text (such as patient id, name or alias) to identify related simulations. Must be provided as string.
#' @param AGE Age. Age of the patient in years. Must be provided as numeric (min. 18, max. 120 year).
#' @param HEIGHT Height. Height of the patient. Must be provided as numeric (min. 100, max. 250 cm).
#' @param WEIGHT Weight. Actual body weight of the patient. Must be provided as numeric (min. 20, max. 500 kg).
#' @param GENDER Sex. Sex of the patient. Must be provided as string ('Male' or 'Female').
#' @param MODEL Model for population of interest. Pharmacokinetic model to be used for specific patient type during simulations. Must be provided as string ('Saez Fernandez et al. (2019) - General ward').
#' @param CREATININE Creatinine. Serum creatinine. Must be provided as numeric (min. 0.01, max. 15 mg/dL).
#' @param CMINPERMIC Minimum concentration to MIC ratio target. The PK/PD target can be provided as minimum concentration to minimum inhibitory concentration ratio (Cmin/MIC). Must be provided as numeric (min. 0.1, max. 50 ).
#' @param REGIMENS Dosing Regimens. List of dosing regimens to be used in simulating target attainment, from which the dosing regimen with the smallest absolute difference from the desired target will be automatically selected. Must be provided as list of 1-8 'REGIMEN' values. Use the \code{regimen} helper function to define the REGIMEN values.
#' @param MICDISTR MIC distribution. Non-cumulative MIC distribution of organism for the given antibiotic at the specified MICs provided as percentages with a sum of 100%. MIC distribution of amikacin and comparator antimicrobial agents when tested against Pseudomonas isolates in the SENTRY program collected during 2013-2019 (mg/L). Dataset provided by JMI Laboratories and the SENTRY Antimicrobial Surveillance Program, available at <a href="http://sentry-mvp.jmilabs.com" target="_new">sentry-mvp.jmilabs.com</a>. Must be provided as set.
#' 
#' @examples \dontrun{
#' simulate_amikacin_mc_cfr_cmin_mic_ratio(PATID = "Anonymous", 
#'     AGE = 75, HEIGHT = 160, 
#'     WEIGHT = 105, GENDER = "Male", 
#'     MODEL = "Saez Fernandez et al. (2019) - General ward", 
#'     CREATININE = 1, CMINPERMIC = 6, 
#'     REGIMENS = list(list(
#'         DOSE = 400, INTERVAL = 4, 
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
simulate_amikacin_mc_cfr_cmin_mic_ratio <- function(PATID, AGE, HEIGHT, WEIGHT, GENDER, MODEL, CREATININE, CMINPERMIC, REGIMENS, MICDISTR) {
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
  assert_number(CMINPERMIC,
    lower = 0.1,
    upper = 50
  )
  ## API call
  simulate("amikacin-mc-cfr-cmin-mic-ratio", PATID = PATID, AGE = AGE, HEIGHT = HEIGHT, WEIGHT = WEIGHT, GENDER = GENDER, MODEL = MODEL, CREATININE = CREATININE, CMINPERMIC = CMINPERMIC, REGIMENS = REGIMENS, MICDISTR = MICDISTR)
}
