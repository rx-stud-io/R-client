api_base <- 'https://api.rx.studio/v1'
api_client_id <- '074ba4733a420cc95b50f5036d5beb7a'


#' Hit Rx Studio simulation API
#' @param endpoint path under \url{https://api.rx.studio/v1/simulation/}
#' @param ... named parameters passed
#' @keywords internal
#' @importFrom httr POST stop_for_status add_headers content headers
#' @importFrom jsonlite toJSON
#' @examples \dontrun{
#' simulate('cefepime-mc-pta-ftime-above-mic',
#'   PATID = 'Anonymous', AGE = 37, HEIGHT = 175, WEIGHT = 90, GENDER = 'Male',
#'   CREATININE = 1, MODEL = 'Tam et al. (2003) - General ward',
#'   PCTABOVEMIC = 60, CRCLCAP = 'No cap', REGIMENS = list(regimen(1000, 12, 2)))
#' }
#' @importFrom logger log_info
simulate <- function(endpoint, ...) {

    url <- file.path(api_base, 'simulation', endpoint)
    timer <- Sys.time()
    log_info('Sending request to {url}')
    body <- toJSON(list(...), auto_unbox = TRUE)
    res <- POST(
        url = url,
        body = body,
        add_headers(
            'X-Api-Key' = api_client_id,
            'Authorization' = paste('Bearer', key = get_id_token())
        ))

    request_id <- headers(res)[['rx-studio-request-id']]
    log_info(paste(
        'Response received in {as.numeric(difftime(Sys.time(), timer, units = "secs"))} seconds',
        '[request id: {request_id}].'))

    ## user-facing error due to bad request
    if (res$status == 400) {
        stop(content(res)$message)
    }
    ## other error?
    stop_for_status(res)

    ## return
    structure(list(
        json = read_cached_simulation(request_id, 'json'),
        html = content(res)
    ), class = c('rx_studio_report', class(res)))

}


#' Load previously run Rx Studio simulation
#' @param request_id UUID
#' @return \code{list} or HTML
#' @export
#' @importFrom httr GET
read_cached_simulation <- function(request_id, format = c('json', 'html')) {

    format <- match.arg(format)

    url <- file.path(api_base, 'cached', request_id, format)
    timer <- Sys.time()
    log_info('Sending request to {url}')
    res <- GET(
        url = url,
        add_headers(
            'X-Api-Key' = api_client_id,
            'Authorization' = paste('Bearer', key = get_id_token())
        ))
    log_info('Response received in {as.numeric(difftime(Sys.time(), timer, units = "secs"))} seconds.')
    stop_for_status(res)

    ## return
    content(res)

}


#' Open HTML in browser
#' @param x HTML document returned by \code{simulate}
#' @param ... further parameters
#' @export
#' @importFrom utils browseURL
print.rx_studio_report <- function(x, ...) {

    t <- tempfile(fileext = '.html')
    cat(as.character(x$html), file = t)

    viewer <- getOption('viewer')
    if (!is.null(viewer)) {
        viewer(t)
    } else {
        browseURL(file.path('file:/', t))
    }

}
