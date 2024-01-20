#' My own color codes. https://www.sessions.edu/color-calculator/ can be used to find new colors.
#'
#' @param theme char; "dark" or "light" theme colors
#' @return Return my color palette. (HEX codes)
#' @export

gx_colors <- function( theme = "light" ){

  if( !theme %in% c( "light", "dark" ) ) stop( "Set theme equal to \"light\", or \"dark\"")

  gx_colors <- c( "#011936", # first six are my selection (appended RBIcolours for the time being)
                  "#942911",
                  "#F49937",
                  "#93B7BE",
                  "#CEAE2A",
                  "#56666B",
                  "#363001",
                  "#4f0113",
                  "#3a4f01",
                  "#700151",
                  "#9d9efd",
                  "#a87654",
                  "#4ca54c",
                  "#8f708f",
                  "#1F1F5C",
                  "#75816B",
                  "#7f0000",
                  "#323232",
                  "#ff8800",
                  "#43A6C6",
                  "#90EE90",
                  "#F6BE00",
                  "#FFA07A",
                  "#008080",
                  "#CCCCFF",
                  "#8A9A5B",
                  "#5C5CFF",
                  "#80400B",
                  "#0000D1",
                  "#A3A300",
                  "#FA8072",
                  "#B4FEE7"

  )

  if( theme == "dark" ){

    # colors created with colorRampPalette(c("#EE5420", "#E1AA6E", "#c0d267"))( 6 )
    gx_colors <- c( "#EE5420", "#E8763F", "#E3985E", "#DAB26C", "#CDC269", "#C0D267" )

  }

  return( gx_colors )

}
