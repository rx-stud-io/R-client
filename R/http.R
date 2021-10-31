client_id <- '074ba4733a420cc95b50f5036d5beb7a'


#' Hit Rx Studio simulation API
#' @param endpoint path under \url{https://api.rx.studio/v1/simulation/}
#' @param ... named parameters passed
#' @keywords internal
#' @importFrom httr POST stop_for_status
#' @importFrom jsonlite toJSON
#' @examples \dontrun{
#' simulate('cefepime_mc_pta_ftime_above_mic',
#'   PATID = 'Anonymous', AGE = 37, HEIGHT = 175, WEIGHT = 90, GENDER = 'MALE',
#'   CREATININE = 1, MODEL = 'Tam et al. (2003) - General ward',
#'   PCTABOVEMIC = 60, CRCLCAP = 'No cap', REGIMENS = list(regimen(1000, 12, 2)))
#' }
#' @importFrom logger log_info
simulate <- function(endpoint, ...) {
    url <- file.path('https://api.rx.studio/v1/simulation', endpoint)
    timer <- Sys.time()
    log_info('Sending request to {url}')
    body <- toJSON(list(...), auto_unbox = TRUE)
    res <- POST(url = url, body = body, add_headers('X-Api-Key' = client_id))
    log_info('Response received in {as.numeric(difftime(Sys.time(), timer, units = "secs"))} seconds.')
    stop_for_status(res)
    content(res)
}
