#' Generate a dosing regimen to be passed to the Rx Studio API
#' @param dose Dose Amount (number in mg)
#' @param interval optional Dosing Interval (number in hours)
#' @param tinf optional Infusion Time (number in hours)
#' @param type Regimen type, e.g. \code{REGIMEN} or \code{ORALREGIMEN}
#' @return list
#' @importFrom checkmate assert_number
#' @export
#' @examples
#' regimen(2000)
#' regimen(2000, 8, 2)
#' regimen(2000, 12, type = 'ORALREGIMEN')
regimen <- function(dose, interval = NULL, tinf = NULL, type = c('REGIMEN', 'ORALREGIMEN')) {
    ## check args
    assert_number(dose)
    assert_number(interval, null.ok = TRUE)
    assert_number(tinf, null.ok = TRUE)
    type <- match.arg(type)
    ## return
    Filter(
        Negate(is.null),
        list(DOSE = dose, INTERVAL = interval, TINF = tinf, set = type))
}
