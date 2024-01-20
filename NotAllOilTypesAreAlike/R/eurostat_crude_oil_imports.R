#' Eurostat Crude Oil Imports
#'
#' A subset of crude oil imports provided by Eurostat
#' The dataset was filtered to Total EU imports.
#'
#' @format ## `eurostat_crude_oil_imports`
#'    A data frame with 17,277 rows and 8 columns:
#' \describe{
#'   \item{DATAFLOW}{Eurostat specific database name}
#'   \item{LAST.UPDATE}{When the data was last updated}
#'   \item{freq}{Frequency: M = monthly}
#'   \item{crudeoil}{Key for country of origin and crude oil type.}
#'   \item{indic_nrg}{Containing variable keys for volume, trade value, api
#'   gravity, sulfur content, average price per barrel}
#'   \item{geo}{Key of importing country/region}
#'   \item{TIME_PERIOD}{Year-Month}
#'   \item{OBS_VALUE}{Value of the data entry}
#'   \item{OBS_FLAG}{Flags: see Eurostat documentation}
#' }
#' @source <https://data.europa.eu/data/datasets/shxq1h97ouyruy8bafkuw?locale=en>
"eurostat_crude_oil_imports"
