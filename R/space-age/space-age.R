# calculate the number of revolutions around the sun (i.e years) that a given
# planet `planet` will traverse in seconds `seconds`
space_age <- function(seconds, planet) {

  stopifnot(
    is.numeric(seconds),
    ! is.na(seconds),
    seconds >= 0,
    is.character(planet),
    ! is.na(planet),
    length(planet) == 1L
  )

  # associative array of the number of earth years that it takes a given planet
  # to revolve around the sun
  planet_map <- c(
    mercury = 0.2408467,
    venus   = 0.61519726,
    earth   = 1,
    mars    = 1.8808158,
    jupiter = 11.862615,
    saturn  = 29.447498,
    uranus  = 84.016846,
    neptune = 164.79132
  )
  seconds_per_earth_year <- 365.25 * 24 * 60 * 60

  # find the number of number of earth years for planet `planet`
  stopifnot(planet %in% names(planet_map))
  planet_coef <- planet_map[planet]

  # calculate the number of revolutions that traversed in seconds `seconds`
  years <- seconds / seconds_per_earth_year / planet_coef
  unname(round(years, 2))
}
