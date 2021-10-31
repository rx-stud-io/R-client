#' Hit Rx Studio simulation API
#' @param endpoint path under \url{https://api.rx.studio/v1/simulation/}
#' @param ... named parameters passed
#' @keywords internal
#' @importFrom httr POST
#' @importFrom jsonlite toJSON
simulate <- function(endpoint, ...) {
    POST(url = file.path('https://api.rx.studio/v1/simulation', endpoint),
         body = toJSON(list(...), auto_unbox = TRUE))
}
