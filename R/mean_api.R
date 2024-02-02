#' Calculates the mean of API gravity for different types of crude oil.
#' @param x num; numeric vector
#' @param weight num; weighting vector
#' @param na.rm	logical evaluating to TRUE or FALSE indicating whether NA values
#'    should be stripped before the computation proceeds.
#' @details As API gravity cannot be added linearly, it is necessary to convert
#'    to specific gravity, then calculate means and only then convert back to
#'    API gravity.
#'    In practice, the difference between this method and taking means directly
#'    will not result in large differences due to similarity of crude oils.
#' @export

mean_api <- function( x,
                      weight = NULL,
                      na.rm = FALSE ){

  df <- as.data.frame( cbind( x, weight ) )

  if( na.rm ) df <- na.omit( df )

  # convert to specific grafity
  sg <- 141.5 / (df$x + 131.5)

  mean_sg <- sum( sg * df$weight / sum( df$weight ) )

  mean_api <- 141.5 / mean_sg - 131.5

  return( mean_api )

}
