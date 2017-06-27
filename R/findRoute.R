#' Solve the problem of finding a route between two cities of the Ryanair dataset
#'
#' @param from the departing city
#' @param to the arrival city
#' @param date the departing day
#' @param interval how many days to consider from date. Default 1 means that departure
#' is exactly at \code{date}
#' @import foreach
#' @import iterators
#' @export
findRoute = function(from, to, date, interval = 1, max.stops = 5){
  # Get the Ryanair dataset
  ryanairCities = getRyanairCities()

  # check if from and to are in the database
  isFrom = length(grep(from, ryanairCities$name, ignore.case = TRUE))>0
  isTo = length(grep(to, ryanairCities$name, ignore.case = TRUE))>0

  # Recursive loop to find a route
  outboundFlights = fetchFlights(city = from, date = date, interval = interval, ryanairCities = ryanairCities)
  foundRoutes = grep(pattern = to, x = outboundFlights$name, ignore.case = TRUE, value = TRUE)
  routes = list(outboundFlights[foundRoutes,])
  for(i in 1:max.stops){
    corres<- foreach(f = iter(outboundFlights[-foundRoutes,], by = "row"), .combine = 'rbind') %do% {

      of = fetchFlights(city = f$name,
                                     date = getDay(f$ArrivalDate),
                                     interval = interval,
                                     ryanairCities = ryanairCities)
      fr = grep(pattern = to, x = of$name, ignore.case = TRUE)
      of[fr,]
    }
  }
}

getDay = function(date){
  lubridate::ymd(strsplit(format(lubridate::floor_date(date, unit = "day"), tz = ""), split = " ")[[1]][1])
}
