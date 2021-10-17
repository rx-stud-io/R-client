#' Define a dosing regimen object to be passed to the Rx Studio API
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


#' Define a dose object to be passed to the Rx Studio API
#' @param datetime date and time of the administered dose
#' @param dose Dose Amount (number in mg)
#' @param tinf optional Infusion Time (number in hours)
#' @param route Route of Administration, e.g. \code{IV} or \code{oral}
#' @return list
#' @importFrom checkmate assert_number assert_posixct
#' @export
#' @examples
#' dose(Sys.time(), 2000, 2)
#' dose(Sys.time(), 2000, route = 'oral')
dose <- function(datetime, dose, tinf = NULL, route = c('IV', 'oral')) {
    ## check args
    route <- match.arg(route)
    assert_posixct(datetime)
    assert_number(dose)
    assert_number(tinf, null.ok = route == 'oral')
    ## return
    if (route == 'IV') {
        list(DATETIME = datetime, DOSE = dose, TINF = tinf, set = 'HISTDOSE')
    } else {
        list(DATETIME = datetime, DOSE = dose, set = 'HISTORALDOSE')
    }
}


#' Define a concentration object to be passed to the Rx Studio API
#' @param datetime date and time of the administered dose
#' @param concentration Concentration (number)
#' @return list
#' @importFrom checkmate assert_number
#' @export
#' @examples
#' concentration(Sys.time(), 10)
concentration <- function(datetime, concentration) {
    ## check args
    assert_posixct(datetime)
    assert_number(concentration)
    ## return
    list(DATETIME = datetime, CONCENTRATION = concentration, set = 'HISTCONCENTRATION')
}
