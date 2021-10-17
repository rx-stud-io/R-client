#' Generate a dosing regimen to be passed to the Rx Studio API
#' @param dose Dose Amount (number in mg)
#' @param interval optional Dosing Interval (number in hours)
#' @param tinf optional Infusion Time (number in hours)
#' @param type Regimen type, e.g. \code{REGIMEN} or \code{ORALREGIMEN}
#' @return list
#' @export
regimen <- function(dose, interval = NULL, tinf = NULL, type = 'REGIMEN') {
    Filter(
        Negate(is.null),
        list(DOSE = dose, INTERVAL = interval, TINF = tinf, set = 'REGIMEN'))
}
