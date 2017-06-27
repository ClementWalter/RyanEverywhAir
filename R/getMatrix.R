getFlightMatrix = function(){
  # Get Ryanair database
  ryanairAirports = getRyanairAirports();

  # Fill up cities
  ryanairAirports$City = getRyanairCities(ryanairAirports)

  # Get all routes from the 2017 flights over a year
  correspondance <- foreach(iata = ryanairAirports$IATA) %do% {
  url = paste0("https://api.ryanair.com/farefinder/3/oneWayFares?&",
               "departureAirportIataCode=",iata,
               "&language=en",
               "&market=en-gb",
               "&offset=0",
               "&outboundDepartureDateFrom=","2017-01-01",
               "&outboundDepartureDateTo=","2017-12-31",
               "&currency=EUR")

    jsonlite::fromJSON(url)$fares$outbound$arrivalAirport$iataCode
  }
  names(correspondance) = ryanairAirports$IATA

  flightMatrix = matrix(0,
                                      nrow = length(ryanairAirports$IATA),
                                      ncol = length(ryanairAirports$IATA))
  row.names(flightMatrix) = ryanairAirports$IATA
  colnames(flightMatrix) = ryanairAirports$IATA

  for(iata in ryanairAirports$IATA){
    flightMatrix[iata, correspondance[[iata]]] = 1
  }

  flightMatrix
}

getRouteMatrix = function(IATA){
  t(sapply(IATA, function(x) paste(x, IATA, sep = "->")))
}
