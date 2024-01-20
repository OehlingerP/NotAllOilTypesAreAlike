#' CEPII Gravity database
#'
#' Subsample of the CEPII Gravity database. Only distance used in hedonic
#' pricing model.
#'
#' @format ## `oil_gravity`
#'    A data frame with 36,500 rows and 87 columns:
#' \describe{
#'   \item{year}{Year}
#'   \item{iso3_o}{3-digit ISO of origin country}
#'   \item{iso3_d}{3-digit ISO of destination country}
#'   \item{dist}{Distance}
#' }
#' @source <http://www.cepii.fr/cepii/en/bdd_modele/bdd_modele_item.asp?id=8>
"oil_gravity"
