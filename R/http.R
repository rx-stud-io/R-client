api_base <- 'https://api.rx.studio/v1/simulation'
api_client_id <- '074ba4733a420cc95b50f5036d5beb7a'


#' Hit Rx Studio simulation API
#' @param endpoint path under \url{https://api.rx.studio/v1/simulation/}
#' @param ... named parameters passed
#' @keywords internal
#' @importFrom httr POST stop_for_status
#' @importFrom jsonlite toJSON
#' @examples \dontrun{
#' simulate('cefepime-mc-pta-ftime-above-mic',
#'   PATID = 'Anonymous', AGE = 37, HEIGHT = 175, WEIGHT = 90, GENDER = 'Male',
#'   CREATININE = 1, MODEL = 'Tam et al. (2003) - General ward',
#'   PCTABOVEMIC = 60, CRCLCAP = 'No cap', REGIMENS = list(regimen(1000, 12, 2)))
#' }
#' @importFrom logger log_info
simulate <- function(endpoint, ...) {
    url <- file.path(api_base, gsub('_', '-', endpoint))
    timer <- Sys.time()
    log_info('Sending request to {url}')
    body <- toJSON(list(...), auto_unbox = TRUE)
    res <- POST(url = url, body = body, add_headers('X-Api-Key' = api_client_id))
    log_info('Response received in {as.numeric(difftime(Sys.time(), timer, units = "secs"))} seconds.')
    stop_for_status(res)
    res <- content(res)
    structure(res, class = c('rx_studio_report', class(res)))
}


#' Open HTML in browser
#' @param x HTML document returned by \code{simulate}
#' @param ... further parameters
#' @export
#' @importFrom utils browseURL
print.rx_studio_report <- function(x, ...) {
    t <- tempfile(fileext = '.html')
    cat(as.character(x), file = t)
    browseURL(file.path('file:/', t))
}
