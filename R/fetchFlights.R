#' Return all the departing flights from a city during a interval of time
#'
#' @param city the deprating city
#' @param date the date of departure
#' @param interval the number of days to be considered from \code{date}. Default to 1
#' means that only one day is considered
#' @param ryanairCities the data.frame of all the served cities
#'
#' @return
#' @export
#'
#' @import magrittr
fetchFlights = function(city, date, interval = 1, ryanairCities){
  # Get IATA code of the city
  normalisedCity = unique(unlist(sapply(strsplit(city, split = " ")[[1]], function(c)
    grep(pattern = c, x = ryanairCities$name, ignore.case = TRUE, value = TRUE)
    )))
  iataList = ryanairCities %>% filter(name %in% normalisedCity) %>% use_series(IATA)

  outboundFlights = do.call(rbind, lapply(iataList, function(iata){
  # Get corresponding url
  url = paste0("https://api.ryanair.com/farefinder/3/oneWayFares?&",
               "departureAirportIataCode=",iata,
               "&language=en",
               "&market=en-gb",
               "&offset=0",
               "&outboundDepartureDateFrom=",date,
               "&outboundDepartureDateTo=",date + interval - 1,
               "&currency=EUR")
  # Set a price upper bound
               # ,"&priceValueTo=150")

  # fetch date
  flights = jsonlite::fromJSON(url)$fares
  flights = flights$outbound$arrivalAirport %>%
    cbind(data.frame(
      From = iata,
      Price = flights$outbound$price$value,
      DepartureDate = lubridate::ymd_hms(flights$outbound$departureDate),
      ArrivalDate = lubridate::ymd_hms(flights$outbound$arrivalDate))) %>%
    rename(IATA = iataCode) %>%
    mutate(seoName = NULL,
           Duration = ArrivalDate - DepartureDate)
  }))
  return(outboundFlights)
}
