library(NotAllOilTypesAreAlike)

df_raw <-
  readRDS(file = "data/eurostat_eu_crude_oil_imports (update).Rdata")

df <- df_raw %>%
  select(-OBS_FLAG, -DATAFLOW, -LAST.UPDATE, -freq) %>%
  pivot_wider(names_from = indic_nrg, values_from = OBS_VALUE) %>%
  mutate(sourceCountryIso = str_extract(crudeoil, "^[^_]*")) %>%
  select(-COMP_NR) %>%
  rename("repCountryIso" = geo,
         "month"         = TIME_PERIOD,
         "api"           = API_DEG,
         "avgPriceBbl"   = AVGPRC_USD_BBL,
         "sul"           = SULPH_PC,
         "value"         = VAL_THS_USD,
         "volume"        = VOL_THS_BBL) %>%
  filter(volume != 0, value != 0) %>%
  mutate(month = as.Date(paste0(month, "-01")) + months(1)-1) %>%
  mutate(year = as.numeric(substr(month, 1, 4)),
         weight = ifelse(api > 31.1, "light",
                         ifelse(api >=  22.3, "medium", "heavy")),
         sulfur = ifelse(sul < 1, "sweet", "sour"),
         oilType = paste(weight, sulfur, sep = "_")) %>%
  filter(crudeoil != "TOTAL")

# add Euro Area
df_ea <- df %>%
  filter(repCountryIso %in% get_ea_countries(type = "iso2")) %>%
  mutate(repCountryIso = "EA")

df <- rbind(df, df_ea)

# load elasticity estimates
df_ela <- data.frame("repCountryIso" = c("AT", "BE", "FR", "DE", "IT", "EA"),
                     "sigma_ces" = c(5.58, 16.4, 30.9, 18.8, 30, 36.2),
                     "gamma_nces" = c(2.8, 9.7, 3.9, 5.8, 6.1, 4.2),
                     "sigma_nces" = c(5.1, 53.2, 19.2, 25.5, 79.3, 45.8))

df_year <- df %>%
  group_by(year, repCountryIso, sourceCountryIso,
           crudeoil, oilType) %>%
  summarize(p = sum(avgPriceBbl*volume/sum(volume, na.rm = T), na.rm = T),
            q = sum(volume, na.rm = T)) %>%
  left_join(df_ela) %>%
  ungroup()

out <- NULL

# We only show Euro Area in the paper (table 4 column 3-4)
COUNTRY <- "Euro Area"
TASTE_YEAR <- 20132019

# load taste parameters for the reference period ---------------------
df_taste <- read.csv(file = "data/tastes_nces.csv") %>%
  filter(year == 20132019) %>%
  select(country, good, taste) %>%
  mutate(country = ifelse(country == "Austria", "AT",
                          ifelse(country == "France", "FR",
                                 ifelse(country == "Italy", "IT",
                                        ifelse(country == "Germany", "DE",
                                               ifelse(country == "Euro Area", "EA",
                                                      ifelse(country == "Belgium", "BE", country))))))) %>%
  rename("oilType" = good,
         "repCountryIso" = country)

df_calc <- df_year %>%
  left_join(df_taste) %>%
  # ignore taste if it could not be calculated
  mutate(taste = ifelse(is.na(taste), 1, taste)) %>%
  na.omit()

tmp <- df_calc %>%
  filter(repCountryIso == "EA")

tmp <- price_index_nces_ces_comparison(x = tmp,
                                       time = "year",
                                       quantity = "q",
                                       price    = "p",
                                       variety  = "crudeoil",
                                       variety_ces = "sourceCountryIso",
                                       good     = "oilType",
                                       taste    = "taste",
                                       sigma_ces = "sigma_ces",
                                       sigma_nces    = "sigma_nces",
                                       gamma_nces    = "gamma_nces")

tmp

