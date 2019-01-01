#' Get altitude from coordinates
#'
#' This function returns the altitude (elevation) in meters for coordinates.
#'
#' @param .Data data.table or data.frame with columns `latitude` and `longitude`.
#'              Either specify the `.Data` argument or specify `.longitude` and `.latitude`.
#' @param .latitude Numeric vector.
#' @param .longitude Numeric vector.
#' @param .src Character value. Which API service to use?
#'             One of `c("geonames", "mapzen", "google", "openelevation")`.
#' @param .geonames.username Character value.
#' @param .google.api.key Character value.
#'
#' @details
#' Currently different services can be used:
#' * `geonames`: Uses the geonames webservice. The number of free requests is limited,
#'               for a higher rate, register and pass your username to this function.
#'               Check usage terms at <http://www.geonames.org/export/web-services.html>.
#' * `google`: Uses the Google Elevation API.
#'             You need to pass your API key. Costs can apply.
#'             Check usage terms at
#'             <https://developers.google.com/maps/documentation/elevation/usage-and-billing>.
#' * `openelevation`: Uses free Open-Elevation API.
#'                    See <https://github.com/Jorl17/open-elevation>.
#' * `racemap`: Uses free Racemap API. Based on mapzen terrain data.
#'              See <https://github.com/racemap/elevation-service>.
#'
#' It is recommended to use environment variables to pass secrets like your geonames
#' username or API keys to the function.
#'
#' Note that some APIs do not return values for ocean areas, e.g. at
#' geonames those values are encoded as -32768.
#'
#' @return Numeric vector of altitudes.
#'
#' @export
#' @importFrom magrittr "%>%"
#'
#' @rdname get_altitude
#'
#' @examples
#' \dontrun{
#' Data <- data.frame(latitude = 48:50, longitude = 11:13)
#'
#' # Geonames
#' get_altitude(Data, .src = "geonames")
#'
#' # Set your geonames username as environment variable in .Renviron file before.
#' # get_altitude(Data, .src = "geonames", .geonames.username = Sys.getenv("GEONAMES_USERNAME"))
#'
#' # Google
#' # get_altitude(Data, Data, .src = "google", .google.api.key = Sys.getenv("GOOGLE_API_KEY"))
#'
#' # Racemap
#' get_altitude(Data, .src = "racemap")
#'
#' # Open-Elevation
#' get_altitude(Data, .src = "openelevation")
#' }
get_altitude <- function(.Data,
                         .longitude = NULL,
                         .latitude = NULL,
                         .src = c("racemap", "geonames", "google", "openelevation"),
                         .geonames.username = "demo",
                         .google.api.key = NULL) {

  .src <- match.arg(.src)

  if (!missing(.Data)) {
    checkmate::assert_data_frame(.Data)
    stopifnot(c("latitude", "longitude") %in% names(.Data))
  }

  if (missing(.Data)) {
    checkmate::assert_numeric(.longitude)
    checkmate::assert_numeric(.latitude)
    stopifnot(length(.latitude) == length(.longitude))

    .Data <- data.frame(longitude = .longitude, latitude = .latitude)
  }

  checkmate::assert_choice(.src, c("geonames", "racemap", "google", "openelevation"))


  altitude <- switch(.src,
                     "openelevation" = get_altitude_openelevation(.Data),
                     "geonames" = get_altitude_geonames(.Data, .geonames.username),
                     "google" = get_altitude_google(.Data, .google.api.key),
                     "racemap" = get_altitude_racemap(.Data))

  altitude
}


get_altitude_geonames <- function(.Data, .geonames.username) {

  query <- stringr::str_interp("http://api.geonames.org/srtm3?lats=${LATITUDE}&lngs=${LONGITUDE}&username=${USERNAME}",
                               list(LATITUDE = stringr::str_c(.Data$latitude, collapse = ","),
                                    LONGITUDE = stringr::str_c(.Data$longitude, collapse = ","),
                                    USERNAME = .geonames.username))

  res <- httr::GET(query)
  stopifnot(res$status_code != 200L)
  httr::content(res) %>%
    XML::xmlParse() %>%
    XML::xmlToList() %>%
    .$body %>%
    .$p %>%
    stringr::str_split("\\\n", simplify = TRUE) %>%
    as.integer %>%
    .[-length(.)]
}


get_altitude_racemap <- function(.Data) {
  if (NROW(.Data) != 1L)
    stop("Currently the implementation only supports one coordinate pair per query.")
  query <- stringr::str_interp("https://elevation.racemap.com/api?lat=${LATITUDE}&lng=${LONGITUDE}",
                               list(LATITUDE = .Data$latitude, LONGITUDE = .Data$longitude))

  res <- httr::GET(query)
  httr::content(res) %>% as.integer()
}


get_altitude_google <- function(.Data, .google.api.key)  {

  locstring <- build_location_string(.Data)
  query <-
    create_api_query("https://maps.googleapis.com/maps/api/elevation/json", locstring) %>%
    stringr::str_c(., "&key=", .google.api.key)
  res <- send_api_query(query)

  extract_altitude(res)
}


get_altitude_openelevation <- function(.Data)  {

  locstring <- build_location_string(.Data)
  query <- create_api_query("https://api.open-elevation.com/api/v1/lookup", locstring)
  res <- send_api_query(query)

  extract_altitude(res)
}


build_location_string <- function(.Data) {
  stringr::str_c(do.call(stringr::str_c,
                         list(.Data$latitude, .Data$longitude, sep = ",")),
                 collapse = "|")
}

create_api_query <- function(.link, .locstring) {
  stringr::str_c(.link,
                 stringr::str_interp("?locations=${loc}",
                                     list(loc = .locstring)))
}

send_api_query <- function(.query) {
  RJSONIO::fromJSON(.query)
}

extract_altitude <- function(.res) {
  .res[[1]] %>%
    purrr::transpose(.) %>%
    .$elevation %>%
    unlist %>%
    round %>%
    as.integer
}
