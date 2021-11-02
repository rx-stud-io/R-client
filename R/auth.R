access_token <- NULL
id_token <- structure('', expires = as.POSIXct(0, origin = '1970-01-01'))


#' Sets access token for Rx Studio API usage
#' @param access_token token generated at \url{https://app.rx.studio/profile/preferences}
#' @export
#' @importFrom utils assignInMyNamespace
set_access_token <- function(access_token) {
    assignInMyNamespace('access_token', access_token)
}


#' Get an id token using the access token
#' @param access_token string
#' @keywords internal
#' @importFrom httr POST stop_for_status
get_id_token <- function() {

    ## return from cache
    if (attr(id_token, 'expires') > Sys.time()) {
        return(id_token)
    }

    if (is.null(access_token)) {
        stop('Rx Studio access token has not been set yet! Please use `set_access_token` first.')
    }

    resp <- POST(
        'https://us-central1-rx-studio-app.cloudfunctions.net/validate_access_token',
        body = list(access_token = access_token),
        encode = 'json')
    stop_for_status(resp)

    ## cache
    assignInMyNamespace(
        'id_token',
        structure(
            content(resp)$id_token,
            expires = Sys.time() + 3600 - 60))
    id_token

}
