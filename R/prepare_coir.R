#' Import and combine raw data files from the Crude Oil Import Register (data
#'    from 2013 to 2019)
#' @details
#' No data transformations are applied. This function solely imports all
#'    Excel files and consolidates them into one data.frame. The ultimate
#'    dataset is included with the package installation and can be accessed
#'    directly. This function serves exclusively for replication purposes.
#' @export

prepare_coir <- function(){

  file_names <- dir(system.file("extdata", package = "NotAllOilTypesAreAlike"))

  file_names <- grep("Crude_Oil_Imports_Reporting_Country", file_names, value = T)

  df <- NULL

  for(FILE in seq_along(file_names)){

    print(file_names[FILE])

    sheet_names <- openxlsx::getSheetNames(file_names[FILE])[-1]

    # extract year
    NCHAR <- nchar(file_names[FILE])
    year <- substr(file_names[FILE], NCHAR-8, NCHAR-5)

    for(SHEET in sheet_names){

      print(SHEET)

      temp <- openxlsx::read.xlsx(file_names[FILE],
                                   sheet = SHEET,
                                   startRow = ifelse(FILE > 5, 7, 1),
                                   sep.names = " ")

      # replace missing country entries (country is only entered in the first row
      # of a country's imports. E.g. UK is entered for the first import of a
      # certain oil type but not for the others)
      temp <- temp %>%
        fill(`Reporting Country`) %>%
        filter(`Reporting Country` != "Reporting Country", !is.na(`Country of Origin`)) %>%
        mutate_at(c("Volume (1000 bbl)", "Total Value ($ 1000)",
                      "CIF price (2) ($/bbl)", "% of Total Imports"), as.numeric)

      # create date column
      month <- substr(SHEET, 10, 12)

      temp$date <- as.Date(paste(year, month, "01"), format = "%Y %b %d") + months(1) - days(1)

      df <- rbind(df, temp)

    } # end sheet loop

  } # end file loop

  return(df)

}



