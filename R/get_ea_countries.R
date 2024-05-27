#' Get Euro Area countries
#' @param type char; fullName or iso2
#' @returns Returns a vector of all Euro Area countries.
#'
#' @export

get_ea_countries <- function(type = "fullName"){

  out <- c("Austria", "Belgium", "Cyprus", "Estonia", "Finland", "France",
           "Germany", "Greece", "Ireland", "Italy", "Latvia", "Lithuania",
           "Luxembourg", "Malta", "Netherlands",  "Portugal", "Slovakia",
           "Slovenia", "Spain")

  if(type == "iso2") out <- c("AT", "BE", "CY", "DE", "EE", "FI", "FR", "GR",
                              "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PT",
                              "SK", "SI", "ES")

  return(out)
}
