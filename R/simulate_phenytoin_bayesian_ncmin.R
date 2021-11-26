#' Phenytoin » Bayesian » Cmin
#' 
#' Phenytoin » Bayesian adaptive dosing » Normalized minimum concentration
#' 
#' \strong{Drug}:
#' Phenytoin
#' 
#' \strong{Method}:
#' Estimate the pharmacokinetic parameters of the patient from past concentrations with Bayesian inverse modeling, then use that information to predict the steady state concentrations for multiple dosing regimens and select the optimal one, with regard to the target pharmacodynamic index.
#' 
#' \strong{PK/PD target}:
#' Normalized minimum blood plasma concentration (mg/L).
#' 
#' @param PATID Patient Identifier. User-provided free text (such as patient id, name or alias) to identify related simulations. Must be provided as string.
#' @param AGE Age. Age of the patient in years. Must be provided as numeric (min. 18, max. 120 year).
#' @param HEIGHT Height. Height of the patient. Must be provided as numeric (min. 100, max. 250 cm).
#' @param WEIGHT Weight. Actual body weight of the patient. Must be provided as numeric (min. 20, max. 500 kg).
#' @param GENDER Sex. Patient's sex for clinical decision-making. Must be provided as string ('Male' or 'Female').
#' @param PHENYTOINSODIUM Phenytoin form. Form of phenytoin to be used during simulations. Must be provided as string ('Phenytoin sodium').
#' @param MODEL Model for population of interest. Pharmacokinetic model to be used for specific patient type during simulations. Must be provided as string ('Muñoz et al. (2019) - ICU').
#' @param RENALFUNCTIONSTATUS Renal function status. Renal function status of the patient. Must be provided as string ('Patients without renal impairment' or 'Dialysis or End-Stage Renal Disease').
#' @param VALPROICACID Valproic acid blood level (leave empty if none). Total valproic acid concentration, leave empty or set to zero if no concurrent administration. Must be provided as numeric (min. 0, max. 500 mg/L).
#' @param CMIN Goal corrected phenytoin trough level. Corrected phenytoin level is calculated using the May Equation given a measured valproic acid level, or with the help of Winter-Tozer Equation which involves the serum albumin level and renal function status. Must be provided as numeric (min. 5, max. 30 mg/L).
#' @param ORALORIVHISTORYWITHALBUMIN Historical Records.  Must be provided as list of 1-48 'HISTALBUMIN', 'HISTDOSE', 'HISTORALDOSE' or 'HISTCONCENTRATION' values.
#' @param ORALORIVREGIMENS Dosing Regimens. List of dosing regimens to be used in simulating target attainment, from which the dosing regimen with the smallest absolute difference from the desired target will be automatically selected. Must be provided as list of 1-20 'REGIMEN' or 'ORALREGIMEN' values. Use the \code{regimen} helper function to define the REGIMEN values.
#' 
#' @examples \dontrun{
#' structure(list("Anonymous", 
#'     45, 175, 75, "Male", 
#'     "Phenytoin sodium", 
#'     "Muñoz et al. (2019) - ICU", 
#'     "Dialysis or End-Stage Renal Disease", 
#'     NULL, 10, list(list(
#'         DATETIME = structure(1601866800, class = c("POSIXct", 
#'         "POSIXt"), tzone = ""), 
#'         ALBUMIN = 4.4, 
#'         set = "HISTALBUMIN"), 
#'         list(DATETIME = structure(1601870400, class = c("POSIXct", 
#'         "POSIXt"), tzone = ""), 
#'             DOSE = 1500, 
#'             TINF = 0.5, 
#'             set = "HISTDOSE"), 
#'         list(DATETIME = structure(1601913600, class = c("POSIXct", 
#'         "POSIXt"), tzone = ""), 
#'             CONCENTRATION = 13, 
#'             set = "HISTCONCENTRATION")), 
#'     list(list(set = "REGIMEN", 
#'         DOSE = 200, INTERVAL = 12, 
#'         TINF = 0.5), 
#'         list(set = "REGIMEN", 
#'             DOSE = 300, 
#'             INTERVAL = 12, 
#'             TINF = 0.5), 
#'         list(set = "REGIMEN", 
#'             DOSE = 400, 
#'             INTERVAL = 12, 
#'             TINF = 0.5), 
#'         list(set = "REGIMEN", 
#'             DOSE = 500, 
#'             INTERVAL = 12, 
#'             TINF = 0.5), 
#'         list(set = "REGIMEN", 
#'             DOSE = 600, 
#'             INTERVAL = 12, 
#'             TINF = 0.5), 
#'         list(set = "REGIMEN", 
#'             DOSE = 200, 
#'             INTERVAL = 24, 
#'             TINF = 0.5), 
#'         list(set = "REGIMEN", 
#'             DOSE = 300, 
#'             INTERVAL = 24, 
#'             TINF = 0.5), 
#'         list(set = "REGIMEN", 
#'             DOSE = 400, 
#'             INTERVAL = 24, 
#'             TINF = 0.5), 
#'         list(set = "REGIMEN", 
#'             DOSE = 500, 
#'             INTERVAL = 24, 
#'             TINF = 0.5), 
#'         list(set = "REGIMEN", 
#'             DOSE = 600, 
#'             INTERVAL = 24, 
#'             TINF = 0.5), 
#'         list(set = "ORALREGIMEN", 
#'             DOSE = 200, 
#'             INTERVAL = 12), 
#'         list(set = "ORALREGIMEN", 
#'             DOSE = 300, 
#'             INTERVAL = 12), 
#'         list(set = "ORALREGIMEN", 
#'             DOSE = 400, 
#'             INTERVAL = 12), 
#'         list(set = "ORALREGIMEN", 
#'             DOSE = 500, 
#'             INTERVAL = 12), 
#'         list(set = "ORALREGIMEN", 
#'             DOSE = 600, 
#'             INTERVAL = 12), 
#'         list(set = "ORALREGIMEN", 
#'             DOSE = 200, 
#'             INTERVAL = 24), 
#'         list(set = "ORALREGIMEN", 
#'             DOSE = 300, 
#'             INTERVAL = 24), 
#'         list(set = "ORALREGIMEN", 
#'             DOSE = 400, 
#'             INTERVAL = 24), 
#'         list(set = "ORALREGIMEN", 
#'             DOSE = 500, 
#'             INTERVAL = 24), 
#'         list(set = "ORALREGIMEN", 
#'             DOSE = 600, 
#'             INTERVAL = 24))), .Names = c("PATID", 
#' "AGE", "HEIGHT", "WEIGHT", 
#' "GENDER", "PHENYTOINSODIUM", 
#' "MODEL", "RENALFUNCTIONSTATUS", 
#' NA, "CMIN", "ORALORIVHISTORYWITHALBUMIN", 
#' "ORALORIVREGIMENS"))
#' }
#' 
#' @references \itemize{
#'   \item Muñoz-Pichuante, Daniel et al. (2019): Dosage of phenytoin in neurocritical patients using Bayesian algorithms: a pilot study. In. Drug Metabolism and Personalized Therapy. https://pubmed.ncbi.nlm.nih.gov/31981450
#'   \item K. Soetaert, T. Petzoldt (2010): Inverse Modelling, Sensitivity and Monte Carlo Analysis in R Using Package FME. In. Journal of Statistical Software. https://www.jstatsoft.org/article/view/v033i03
#' }
#' @export 
#' @importFrom checkmate assert_number assert_string assert_choice
simulate_phenytoin_bayesian_ncmin <- function(PATID, AGE, HEIGHT, WEIGHT, GENDER, PHENYTOINSODIUM, MODEL, RENALFUNCTIONSTATUS, VALPROICACID, CMIN, ORALORIVHISTORYWITHALBUMIN, ORALORIVREGIMENS) {
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
  assert_string(PHENYTOINSODIUM)
  assert_choice(PHENYTOINSODIUM, c("Phenytoin sodium"))
  assert_string(MODEL)
  assert_choice(MODEL, c("Muñoz et al. (2019) - ICU"))
  assert_string(RENALFUNCTIONSTATUS)
  assert_choice(RENALFUNCTIONSTATUS, c("Patients without renal impairment", "Dialysis or End-Stage Renal Disease"))
  assert_number(VALPROICACID,
    lower = 0,
    upper = 500
  )
  assert_number(CMIN,
    lower = 5,
    upper = 30
  )
  ## API call
  simulate("phenytoin-bayesian-ncmin", PATID = PATID, AGE = AGE, HEIGHT = HEIGHT, WEIGHT = WEIGHT, GENDER = GENDER, PHENYTOINSODIUM = PHENYTOINSODIUM, MODEL = MODEL, RENALFUNCTIONSTATUS = RENALFUNCTIONSTATUS, VALPROICACID = VALPROICACID, CMIN = CMIN, ORALORIVHISTORYWITHALBUMIN = ORALORIVHISTORYWITHALBUMIN, ORALORIVREGIMENS = ORALORIVREGIMENS)
}
