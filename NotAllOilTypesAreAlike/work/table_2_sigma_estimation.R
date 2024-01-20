df <- merge_quality_quantity() %>%
  # restrict country set based on data restrictions (some countries just don't import enough varieties to estimate the elasticity on oil type)
  filter(`Reporting Country` %in% c("France", "Netherlands", "Belgium", "Germany", "Italy", "Spain",
                                      "United Kingdom", "Greece", "Sweden", "Austria", "Romania", "Portugal")) %>%
  rename("Oil Field" = `Type of crude oil`)

df <- df %>%
  mutate(date = as.numeric(substr(date, 1, 4))) %>%
  group_by(`Country of Origin`, `Oil Field`, `Reporting Country`,
            `date`, `Weight`, `Sulfur`) %>%
  summarize(`Volume (1000 bbl)` = sum(`Volume (1000 bbl)`, na.rm = T),
             `Total Value ($ 1000)` = sum(`Total Value ($ 1000)`, na.rm = T)) %>%
  ungroup() %>%
  mutate(`CIF price (2) ($/bbl)` = `Total Value ($ 1000)` / `Volume (1000 bbl)`,
          date = as.Date(paste0(date, "-12-31")))

# Add Euro Area
  df_ea <- merge_quality_quantity() %>%
    # restrict country set based on data restrictions (some countries just don't import enough varieties to estimate the elasticity on oil type)
    filter(`Reporting Country` %in% get_ea_countries()) %>%
    rename("Oil Field" = `Type of crude oil`)

  df_ea <- df_ea %>%
    mutate(`Reporting Country` = "Euro Area",
           date = as.numeric(substr(date, 1, 4))) %>%
    group_by(`Country of Origin`, `Oil Field`, `Reporting Country`,
              `date`, `Weight`, `Sulfur`) %>%
    summarize(`Volume (1000 bbl)` = sum(`Volume (1000 bbl)`, na.rm = T),
               `Total Value ($ 1000)` = sum(`Total Value ($ 1000)`, na.rm = T)) %>%
    ungroup() %>%
    mutate(`CIF price (2) ($/bbl)` = `Total Value ($ 1000)` / `Volume (1000 bbl)`,
            date = as.Date(paste0(date, "-12-31"))) %>%
    filter(`Total Value ($ 1000)` > 0, `CIF price (2) ($/bbl)` > 0, `Volume (1000 bbl)` > 0)

df <- rbind(df, df_ea)


# country level aggregation
df_country <- df %>%
  #mutate(type = paste(Weight, Sulfur)) %>%
  group_by(date, `Reporting Country`, `Country of Origin`) %>% #type
  summarize(`Volume (1000 bbl)` = sum(`Volume (1000 bbl)`, na.rm = T),
             `Total Value ($ 1000)` = sum(`Total Value ($ 1000)`, na.rm = T)) %>%
  ungroup() %>%
  mutate(`CIF price (2) ($/bbl)` = `Total Value ($ 1000)` / `Volume (1000 bbl)`)


# oil type aggregation
df_api_sulphur <- df %>%
  mutate(`Oil Type` = paste(Weight, Sulfur)) %>%
  group_by(date, `Reporting Country`, `Oil Type`) %>%
  summarize(`Volume (1000 bbl)` = sum(`Volume (1000 bbl)`, na.rm = T),
             `Total Value ($ 1000)` = sum(`Total Value ($ 1000)`, na.rm = T)) %>%
  ungroup() %>%
  mutate(`CIF price (2) ($/bbl)` = `Total Value ($ 1000)` / `Volume (1000 bbl)`)

# estimate the elasticity of substition for each aggregation type

results <- NULL

for(COUNTRY in sort(unique(df$`Reporting Country`))){
  print(COUNTRY)
  for(VARIETY in c("Oil Field", "Country of Origin", "Oil Type")){
    print(VARIETY)
    temp <- df

    if(VARIETY == "Country of Origin") temp <- df_country

    if(VARIETY == "Oil Type") temp <- df_api_sulphur

    temp <- temp %>% filter(`Reporting Country` == COUNTRY)

    out <- tryCatch({
      elasticity_feenstra1994(df     = temp,
                               t_name = "date",
                               v_name = VARIETY,
                               p_name = "CIF price (2) ($/bbl)",
                               q_name = "Volume (1000 bbl)",
                               freq   = "y")
    }, error=function(e){ out <- as.data.frame(t(rep(NA, 12))); colnames(out) <- c("nObs", "nVarieties", "theta1", "theta2", "rho", "sigma", "theta1_se", "theta2_se", "rho_se", "sigma_se", "sigma_lb", "sigma_ub"); out })

    out$Variety <- VARIETY; out$Country <- COUNTRY

    results <- rbind(results, out)

  }

}

tab <- results %>%
  select(Variety, Country, sigma) %>%
  pivot_wider(names_from = Variety, values_from = sigma)

openxlsx::write.xlsx(tab, file = file.path(FILE_OUTPUT, "table_2.xlsx"))

# calculate within group elasticities

df$weight_sulfur = paste(df$Weight, df$Sulfur)
results_within <- NULL

for(COUNTRY in sort(unique(df$`Reporting Country`))){
  print(COUNTRY)

  temp1 <- df %>% filter(`Reporting Country` == COUNTRY)

  for(VARIETY in unique(temp1$weight_sulfur)){
    print(VARIETY)
    temp <- df %>% filter(weight_sulfur == VARIETY, `Reporting Country` == COUNTRY)

    out <- tryCatch({
      elasticity_feenstra1994(df     = temp,
                               t_name = "date",
                               v_name = "Oil Field",
                               p_name = "CIF price (2) ($/bbl)",
                               q_name = "Volume (1000 bbl)",
                               freq   = "y")
    }, error=function(e){ out <- as.data.frame(t(rep(NA, 12))); colnames(out) <- c("nObs", "nVarieties", "theta1", "theta2", "rho", "sigma", "theta1_se", "theta2_se", "rho_se", "sigma_se", "sigma_lb", "sigma_ub"); out })

    out$Variety <- VARIETY; out$Country <- COUNTRY

    results_within <- rbind(results_within, out)

  }

}

tab <- results_within %>%
  filter(Variety != "NA NA") %>%
  select(sigma, Variety, Country) %>%
  pivot_wider(names_from = Variety, values_from = sigma)

openxlsx::write.xlsx(tab, file = file.path(FILE_OUTPUT, "table_a2.xlsx"))
