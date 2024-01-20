#' Crude Oil Import Register (COIR)
#'
#' Crude oil imports by European Union countries from 2013 to 2019
#'
#' @format ## `crude_oil_import_register`
#'    A data frame with 17,277 rows and 8 columns:
#' \describe{
#'   \item{Reporting Country}{Importing Country}
#'   \item{Country of Origin}{Where the crude oil comes from.}
#'   \item{Type of crude oil}{Name of the crude oil}
#'   \item{Volume (1000 bbl)}{Volume in 1000 bbl}
#'   \item{Total Value (\$ 1000)}{Total value in \$ 1000}
#'   \item{CIF price (2) (\$/bbl)}{Cost, Insurance and Freight price}
#'   \item{\% of Total Imports}{\% of monthly total crude oil imports entering the European Union}
#'   \item{date}{End of the month (formatted as date)}
#' }
#' @source <https://wayback.archive-it.org/12090/20220915190726/https://energy.ec.europa.eu/data-and-analysis/eu-crude-oil-imports-and-supply-cost_en>
"crude_oil_import_register"
