# create Euro Area data
df_ea <- merge_quality_quantity() %>%
  # restrict country set based on data restrictions (some countries just don't import enough varieties to estimate the elasticity on oil type)
  filter(`Reporting Country` %in% c("Austria", "Belgium", "Finland", "France",
                                      "Germany", "Greece", "Ireland", "Italy",
                                      "Lithuania", "Netherlands",  "Portugal",
                                      "Slovakia", "Spain")) %>%
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
          date = as.Date(paste0(date, "-12-31"))) %>%
  rbind(df_ea)

# correct Russian Federation Oil Types. According to reports from Bruegel almost
# all crude oil imported from Russia is Urals therefore this seems to be a
# reasonable assumptions

df <- df %>%
  mutate(type = paste(Weight, Sulfur),
         `Oil Field` = ifelse(`Country of Origin` == "Russian Federation", "Urals", `Oil Field`),
         type = ifelse(`Country of Origin` == "Russian Federation", "medium sour", type))


# IDEA: Estimate the welfare changes for dropping a whole nest (namely medium-sour crude oil)
# and compare it with the case of dropping Russia (where we treat all Russian imports as Urals)

# create data frame of varieties
df_final <- data.frame("country" = c("Austria", "Belgium", "France", "Germany", "Italy", "Euro Area"),
                       "sigma_feenstra" = c(5.58, 16.4, 30.9, 18.8, 30, 36.2),
                       "sigma" = c(5.1, 53.2, 19.2, 25.5, 79.3, 45.8),
                       "gamma" = c(2.8, 9.7, 3.9, 5.8, 6.1, 4.2),
                       "share of medium sour in % of total imports" = numeric(6),
                       "share of Russian crude oil in % of total medium sour imports" = numeric(6),
                       "remove all medium sour crude oil" = numeric(6),
                       "remove Russia treating all as Urals" = numeric(6),
                       check.names = F)

# calculate shares of medium sour and share of Russian crude oil
df_share_ms <- df %>%
  filter(`Reporting Country` %in% df_final$country) %>%
  group_by(`Reporting Country`, type) %>%
  summarize(q_ms = sum(`Volume (1000 bbl)`)) %>%
  group_by(`Reporting Country`) %>%
  mutate(totQ = sum(q_ms)) %>%
  mutate(q_ms = round(q_ms/totQ*100,1)) %>%
  filter(type == "medium sour") %>%
  ungroup()

df_final$`share of medium sour in % of total imports` <- unlist(df_share_ms[match(df_final$country,df_share_ms$`Reporting Country`), "q_ms"])

df_share_ru <- df %>%
  filter(`Reporting Country` %in% df_final$country,
         type == "medium sour") %>%
  group_by(`Reporting Country`) %>%
  mutate(totQ = sum(`Volume (1000 bbl)`)) %>%
  filter(`Country of Origin` == "Russian Federation") %>%
  mutate(q_ru = sum(`Volume (1000 bbl)`)) %>%
  mutate(q_ru = round(q_ru/totQ * 100, 1)) %>%
  select(`Reporting Country`, q_ru) %>%
  unique()

df_final$`share of Russian crude oil in % of total medium sour imports` <- unlist(df_share_ru[match(df_final$country,df_share_ru$`Reporting Country`), "q_ru"])

df_final_perc <- df_final

for(i in 1:nrow(df_final)){

  df_sim <- df %>%
    filter(`Reporting Country` == df_final$country[i])

  # drop medium sour
  temp <- est_welfare(x = df_sim,
                      x_diff = df_sim %>% filter(type != "medium sour"),
                      quantity = "Volume (1000 bbl)",
                      type = "type",
                      sigma_ces = df_final$sigma_feenstra[i],
                      sigma_nces = df_final$sigma[i],
                      gamma_nces = df_final$gamma[i])

  df_final$`remove all medium sour crude oil`[i] <- round(temp[1] - temp[2], 2)

  df_final_perc$`remove all medium sour crude oil`[i] <- round(((temp[1] - temp[2]) / temp[2]) * 100, 2) * -1


  # drop all Russian crude oil treating all as Urals
  temp <- est_welfare(x = df_sim,
                      x_diff = df_sim %>%
                        filter(`Country of Origin` != "Russian Federation"),
                      quantity = "Volume (1000 bbl)",
                      type = "type",
                      sigma_ces = df_final$sigma_feenstra[i],
                      sigma_nces = df_final$sigma[i],
                      gamma_nces = df_final$gamma[i])

  df_final$`remove Russia treating all as Urals`[i] <- round(temp[1] - temp[2], 2)

  df_final_perc$`remove Russia treating all as Urals`[i] <- round(((temp[1] - temp[2]) / temp[2]) * 100, 2) * -1

}

openxlsx::write.xlsx(df_final, file = file.path(FILE_OUTPUT, "table_3.xlsx"))




