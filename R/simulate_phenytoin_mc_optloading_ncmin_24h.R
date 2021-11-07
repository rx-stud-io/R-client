#' Phenytoin » Optimal loading dose » Cmin
#' 
#' Phenytoin » Optimal loading dose simulation » Post-dose concentration after 24 hrs
#' 
#' \strong{Drug}:
#' Phenytoin
#' 
#' \strong{Method}:
#' Calculate optimal loading dose and simulate concentrations for the first 24 hours.
#' 
#' \strong{PK/PD target}:
#' Post-dose blood plasma concentration after 24 hrs (mg/L).
#' 
#' @param PATID Patient Identifier. User-provided free text (such as patient id, name or alias) to identify related simulations. Must be provided as string.
#' @param AGE Age. Age of the patient in years. Must be provided as numeric (min. 18, max. 120 year).
#' @param HEIGHT Height. Height of the patient. Must be provided as numeric (min. 100, max. 250 cm).
#' @param WEIGHT Weight. Actual body weight of the patient. Must be provided as numeric (min. 20, max. 500 kg).
#' @param GENDER Sex. Patient's sex for clinical decision-making. Must be provided as string ('Male' or 'Female').
#' @param PHENYTOINFORM Phenytoin form. Form of phenytoin to be used during simulations. Must be provided as string ('Phenytoin sodium IV' or 'Fosphenytoin sodium IV').
#' @param MODEL Model for population of interest. Pharmacokinetic model to be used for specific patient type during simulations. Must be provided as string ('Muñoz et al. (2019) - ICU').
#' @param ALBUMIN Albumin. Serum albumin. Must be provided as numeric (min. 0.01, max. 15 g/dL).
#' @param RENALFUNCTIONSTATUS Renal function status. Renal function status of the patient. Must be provided as string ('Patients without renal impairment' or 'Dialysis or End-Stage Renal Disease').
#' @param VALPROICACID Valproic acid blood level (leave empty if none). Total valproic acid concentration, leave empty or set to zero if no concurrent administration. Must be provided as numeric (min. 0, max. 500 mg/L).
#' @param CPOSTDOSE24H Post-dose plasma concentration after 24 hrs target. The PK/PD target can be provided as post-dose blood plasma concentration after 24 hrs (Cmin). Must be provided as numeric (min. 0.1, max. 200 mg/L).
#' 
#' @examples \dontrun{
#' structure(list("Anonymous", 
#'     45, 175, 75, "Male", 
#'     "Fosphenytoin sodium IV", 
#'     "Muñoz et al. (2019) - ICU", 
#'     3, "Dialysis or End-Stage Renal Disease", 
#'     NULL, 10), .Names = c("PATID", 
#' "AGE", "HEIGHT", "WEIGHT", 
#' "GENDER", "PHENYTOINFORM", 
#' "MODEL", "ALBUMIN", "RENALFUNCTIONSTATUS", 
#' NA, "CPOSTDOSE24H"))
#' }
#' 
#' @references \itemize{
#'   \item Muñoz-Pichuante, Daniel et al. (2019): Dosage of phenytoin in neurocritical patients using Bayesian algorithms: a pilot study. In. Drug Metabolism and Personalized Therapy. https://pubmed.ncbi.nlm.nih.gov/31981450
#' }
#' @export 
#' @importFrom checkmate assert_number assert_string assert_choice
simulate_phenytoin_mc_optloading_ncmin_24h <- function(PATID, AGE, HEIGHT, WEIGHT, GENDER, PHENYTOINFORM, MODEL, ALBUMIN, RENALFUNCTIONSTATUS, VALPROICACID, CPOSTDOSE24H) {
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
  assert_string(PHENYTOINFORM)
  assert_choice(PHENYTOINFORM, c("Phenytoin sodium IV", "Fosphenytoin sodium IV"))
  assert_string(MODEL)
  assert_choice(MODEL, c("Muñoz et al. (2019) - ICU"))
  assert_number(ALBUMIN,
    lower = 0.01,
    upper = 15
  )
  assert_string(RENALFUNCTIONSTATUS)
  assert_choice(RENALFUNCTIONSTATUS, c("Patients without renal impairment", "Dialysis or End-Stage Renal Disease"))
  assert_number(VALPROICACID,
    lower = 0,
    upper = 500
  )
  assert_number(CPOSTDOSE24H,
    lower = 0.1,
    upper = 200
  )
  ## API call
  simulate("phenytoin-mc-optloading-ncmin-24h", PATID = PATID, AGE = AGE, HEIGHT = HEIGHT, WEIGHT = WEIGHT, GENDER = GENDER, PHENYTOINFORM = PHENYTOINFORM, MODEL = MODEL, ALBUMIN = ALBUMIN, RENALFUNCTIONSTATUS = RENALFUNCTIONSTATUS, VALPROICACID = VALPROICACID, CPOSTDOSE24H = CPOSTDOSE24H)
}
