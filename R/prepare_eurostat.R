#' Import and prepare Eurostat crude oil import data.
#' @param only_eu log; if FALSE column COMP_NR will be removed. Necessary if the
#'    original dataset is used. Here we just use a subsample and consider only
#'    imports into the European Union as a whole.
#' @details
#' This function just renames variables, matches country names to country
#'    codes and creates some new variables from existing ones. No data
#'    transformations are applied.
#' @export

prepare_eurostat <- function(only_eu = T){

  df_raw <- eurostat_crude_oil_imports

  # import country metadata to match full country names
  df_meta_country <- openxlsx::read.xlsx(
    system.file("extdata", "metadata_countries.xlsx",
                package = "NotAllOilTypesAreAlike")) %>%
    select(eurostat, iso.name.en) %>%
    rename("sourceCountryIso" = eurostat,
           "sourceCountry"    = iso.name.en)

  df <- df_raw %>%
    select(-OBS_FLAG, -DATAFLOW, -LAST.UPDATE, -freq) %>%
    pivot_wider(names_from = indic_nrg, values_from = OBS_VALUE) %>%
    mutate(sourceCountryIso = stringr::str_extract(crudeoil, "^[^_]*")) %>%
    left_join(df_meta_country) %>%
    left_join(df_meta_country %>%
                rename("geo"        = sourceCountryIso,
                       "repCountry" = sourceCountry)) %>%
    mutate(repCountry = ifelse(geo == "EU_V", "European Union", repCountry),
           geo = ifelse(geo == "EU_V", "EU", geo))

  if(!only_eu) df <- df %>% select(-COMP_NR)

  df <- df %>%
    rename("repCountryIso" = geo,
           "month"         = TIME_PERIOD,
           "api"           = API_DEG,
           "avgPriceBbl"   = AVGPRC_USD_BBL,
           "sul"           = SULPH_PC,
           "value"         = VAL_THS_USD,
           "volume"        = VOL_THS_BBL) %>%
    filter(volume != 0) %>%
    mutate(month = as.Date(paste0(month, "-01")) + months(1)-1)

  return(df)

}
