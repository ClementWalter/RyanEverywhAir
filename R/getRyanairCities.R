#' Get all the cities served by Ryanair
#'
#' @param url the url of the Ryanair api
#'
#' @return A data.frame with all the cities
#' @export
#'
#' @import dplyr
#' @import magrittr
#' @import tibble
getRyanairAirports = function(url = "https://desktopapps.ryanair.com/en-gb/res/stations"){
  ryanairAirports = do.call(rbind, lapply(jsonlite::fromJSON(url), as.data.frame)) %>%
    rownames_to_column() %>% rename(IATA = rowname)
}

#' Infer cities from list of all served airports.
#'
#' Some cities may have severa airports. One considers that the name of
#' the city is part of the name of the airport.
#'
#' @param ryanairAirports data.frame of all the airports
#'
#' @return
#' @export
#'
#' @examples
getRyanairCities = function(ryanairAirports){
  # Get all airports names
  if(missing(ryanairAirports)) ryanairAirports = getRyanairAirports()
  allAirports = ryanairAirports$name

  # First filter on obvious patterns
  allCities = gsub(pattern = " [a-z]+[[:digit:]]+| airport| international",
                   replacement = "",
                   x = ryanairAirports$name, ignore.case = TRUE)

  # Manuall handle special cases
  specialCities = c('Barcelona', 'Bilbao', 'Stockholm', 'Milan')
  for(c in specialCities){
    isCity = grep(pattern = c, x = allCities, ignore.case = TRUE)
    allCities[isCity] = c
  }

  # Finally remove all parenthesis and their content
  allCities = gsub(pattern = " \\([a-z -ù]*\\)",
                   replacement = "",
                   x = allCities, ignore.case = TRUE)
}
