#' Merge Quality and Quantity data and adjust Russian missspecification
#'
#' @param share_urals num; how much of Russian crude oil reported as Other
#'    should be treated as Urals crude oil
#' @details
#' Most imports from Russia are classified as "Other Russian Crude Oil". Thus,
#' no quality can be assigned. However, various EU Institutions stated that the
#' largest share (more than 80%) of the crude oil imported from Russia is of
#' type Urlas. Also the more recent data on Eurostat supports this fact.
#' Therefore, we make a conservative assumption and treat 70% of all Russian
#' imports as Urlas crude oil. This parameter can be varied for robustness
#' checks. Lowering it to 50% does not impair our results.
#' @export

merge_quality_quantity <- function(share_urals = 0.7){

  data(crude_oil_import_register)

  df_quality <- openxlsx::read.xlsx(
    system.file("extdata", "types of crude in coir data.xlsx",
                package = "NotAllOilTypesAreAlike"),
    sep.names = " " ) %>%
    select( -`Note 1`, -`Note 2` ) %>%
    na.omit()

  df <- merge(crude_oil_import_register,
              df_quality,
              by = c( "Country of Origin", "Type of crude oil" ),
              all.x = T)

  df_ru_urals <- filter(df, `Type of crude oil` == "Urals")
  df_ru_other <- filter(df, `Type of crude oil` == "Other Russian Fed. Crude")

  df_ru_other_to_urals <- df_ru_other %>%
    mutate(`Volume (1000 bbl)` = `Volume (1000 bbl)` * share_urals,
           `Total Value ($ 1000)` = `Total Value ($ 1000)` * share_urals,
           `% of Total Imports` = `% of Total Imports` * share_urals,
           `Typical API gravity` = df_ru_urals$`Typical API gravity`[1],
           `Typical sulphur content` = df_ru_urals$`Typical sulphur content`[1],
           Weight = df_ru_urals$Weight[1],
           Sulfur =df_ru_urals$Sulfur[1],
           `Type of crude oil` = "Urals")

  df_ru_urals <- df_ru_urals %>%
    rbind(df_ru_other_to_urals) %>%
    group_by(`Country of Origin`, `Type of crude oil`, `Reporting Country`,
             `date`, `Typical API gravity`, `Typical sulphur content`,
             `Weight`, `Sulfur`) %>%
    summarize(`Volume (1000 bbl)` = sum(`Volume (1000 bbl)`, na.rm = T),
              `Total Value ($ 1000)` = sum(`Total Value ($ 1000)`, na.rm = T),
              `% of Total Imports` = sum(`% of Total Imports`, na.rm = T)) %>%
    mutate(`CIF price (2) ($/bbl)` = `Total Value ($ 1000)`/`Volume (1000 bbl)`) %>%
    ungroup()

  df_ru_other <- df_ru_other %>%
    mutate(`Volume (1000 bbl)` = `Volume (1000 bbl)` * (1-share_urals),
           `Total Value ($ 1000)` = `Total Value ($ 1000)` * (1-share_urals),
           `% of Total Imports` = `% of Total Imports` * (1-share_urals))

  df_ru_urals <- df_ru_urals[ , colnames(df)]

  df_ru_adjusted <- rbind(df_ru_urals, df_ru_other)

  df <- filter(df, `Country of Origin` != "Russian Federation") %>%
    rbind(df_ru_adjusted)

  return(df)

}


