# price analysis
  df <- merge_quality_quantity()

  df <- df %>% filter(!is.na(`Typical API gravity`),
                       `Total Value ($ 1000)` != 0,
                       `CIF price (2) ($/bbl)` != 0)

# prepare oil pipeline data
  df_pipe <- oil_ngl_pipelines %>%
    select(StartCountry, EndCountry) %>%
    unique() %>%
    mutate(pipeline = TRUE)

  df <- merge(df, df_pipe,
               by.x = c("Country of Origin", "Reporting Country"),
               by.y = c("StartCountry", "EndCountry"),
               all.x = T) %>%
    mutate(pipeline = ifelse(is.na(pipeline), FALSE, pipeline))

# merge iso to merge gravity
  meta_countries <- openxlsx::read.xlsx(system.file("extdata", "metadata_countries.xlsx",
                                                    package = "NotAllOilTypesAreAlike"))

  df <- merge(df,
               meta_countries %>% select(iso.name.en, iso3c),
               by.x = "Country of Origin",
               by.y = "iso.name.en",
               all.x = T) %>%
    rename("isoOrigin" = iso3c) %>%
    merge(meta_countries %>% select(iso.name.en, iso3c),
           by.x = "Reporting Country",
           by.y = "iso.name.en",
           all.x = T) %>%
    rename("isoReporter" = iso3c)

  df <- df %>%
    mutate(isoReporter = ifelse(`Reporting Country` == "Czech Republic", "CZE",
                                  ifelse(`Reporting Country` == "Netherlands", "NLD", isoReporter)),
            isoOrigin = ifelse(`Country of Origin` == "Russian Federation", "RUS",
                                ifelse(`Country of Origin` == "United States", "USA",
                                        ifelse(`Country of Origin` == "Iran", "IRN",
                                                ifelse(`Country of Origin` == "Venezuela", "VEN",
                                                        ifelse(`Country of Origin` == "Abu Dhabi", "UAE",
                                                                ifelse(`Country of Origin` == "Syria", "SYR", isoOrigin)))))))

  df <- df %>%
    mutate(year = as.numeric(substr(date, 1, 4))) %>%
    merge(oil_gravity %>% filter(country_exists_o == 1, country_exists_d == 1),
           by.x = c("year", "isoOrigin", "isoReporter"),
           by.y = c("year", "iso3_o", "iso3_d"),
           all.x = T)

  # calculate time fixed effects (characters needed)
  df$time_fe <- as.character(df$date)

  df$`Typical API gravity^2` <- df$`Typical API gravity`^2

  model <- lm(`CIF price (2) ($/bbl)`~ `Volume (1000 bbl)` + `Typical API gravity`+`Typical API gravity^2`+`Typical sulphur content` + pipeline +# oil type effects
                 dist+ # gravity vars (distance, neighbor, colonial relationship)
                 `Country of Origin`+`Reporting Country`+time_fe,  # fixed effects
               data = df)

  summary(model)$r.squared

  openxlsx::write.xlsx(as.data.frame(
    cbind("var" = rownames(summary(model)$coefficients),
          summary(model)$coefficients)),
    file = file.path(FILE_OUTPUT, "table_1.xlsx"))





