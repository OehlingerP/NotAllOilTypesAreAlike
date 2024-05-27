library(OilTradeQuality)
library(stringr)
df_raw <- read.csv(file = "data/resubmission/nrg_ti_coifpm_linear.csv")

# import country metadata to match full country names
df_meta_country <- read.xlsx("data/metadata_countries.xlsx") %>%
  select(eurostat, iso.name.en) %>%
  rename("sourceCountryIso" = eurostat,
         "sourceCountry" = iso.name.en)

df <- df_raw %>%
  select(-OBS_FLAG, -DATAFLOW, -LAST.UPDATE, -freq) %>%
  pivot_wider(names_from = indic_nrg, values_from = OBS_VALUE) %>%
  mutate(sourceCountryIso = str_extract(crudeoil, "^[^_]*")) %>%
  left_join(df_meta_country) %>%
  left_join(df_meta_country %>%
              rename("geo" = sourceCountryIso,
                     "repCountry" = sourceCountry)) %>%
  mutate(repCountry = ifelse(geo == "EU_V", "European Union", repCountry),
         geo = ifelse(geo == "EU_V", "EU", geo)) %>%
  select(-COMP_NR) %>%
  rename("repCountryIso" = geo,
         "month"         = TIME_PERIOD,
         "api"           = API_DEG,
         "avgPriceBbl"   = AVGPRC_USD_BBL,
         "sul"           = SULPH_PC,
         "value"         = VAL_THS_USD,
         "volume"        = VOL_THS_BBL) %>%
  filter(volume != 0) %>%
  mutate(month = as.Date(paste0(month, "-01")) + months(1)-1) %>%
  # correct wrong (source) country names
  mutate(sourceCountry = ifelse(sourceCountryIso == "AG", "Argentina",
                                ifelse(sourceCountryIso == "AFR", "Africa",
                                       ifelse(sourceCountryIso == "ASI", "Asia",
                                              ifelse(sourceCountryIso == "AUH", "Abu Dhabi",
                                                     ifelse(sourceCountryIso == "EUR", "Europe",
                                                            ifelse(sourceCountryIso == "GR", "Greece",
                                                                   ifelse(sourceCountryIso == "LA", "Latin America",
                                                                          ifelse(sourceCountryIso == "ME", "Middle East",
                                                                                 ifelse(sourceCountryIso == "NZ", "Neutral Zone",
                                                                                        sourceCountry))))))))),
         repCountry = ifelse(repCountry == "Netherlands (the)", "Netherlands", repCountry),
         year = as.numeric(substr(month, 1, 4)),
         weight = ifelse(api > 31.1, "light",
                         ifelse(api >=  22.3, "medium", "heavy")),
         sulfur = ifelse(sul < 1, "sweet", "sour"),
         oilType = paste(weight, sulfur, sep = " ")) %>%
  filter(crudeoil != "TOTAL")

# add Euro Area
df_ea <- df %>%
  filter(repCountry %in% get_ea_countries()) %>%
  mutate(repCountry = "Euro Area", repCountryIso = "EA")

df <- rbind(df, df_ea)

# prepare for welfare estimation (we compare 2021 with 2023)
df_year <- df %>%
  group_by(year, repCountryIso, repCountry, sourceCountryIso, sourceCountry,
           crudeoil, oilType) %>%
  summarize(vol = sum(volume, na.rm = T),
            price = sum(value, na.rm = T)/sum(volume, na.rm = T)) %>%
  # normalize
  group_by(repCountryIso, year) %>%
  mutate(volNorm = vol / sum(vol) * 100) %>%
  ungroup()

# load elasticity estimates
df_within <- read.xlsx("tables/results_within.xlsx") %>%
  group_by(Country) %>%
  summarize(sigma = mean(sigma, na.rm = T)) %>%
  rename("sigma_nces" = sigma)

df_ela <- read.xlsx("tables/results_across.xlsx") %>%
  select(Country, Variety, sigma) %>%
  pivot_wider(names_from = Variety, values_from = sigma) %>%
  rename("sigma_ces" = `Country of Origin`,
         "gamma_nces" = `Oil Type`) %>%
  select(-`Oil Field`) %>%
  left_join(df_within) %>%
  na.omit()

df_within <- read.xlsx("tables/results_within.xlsx") %>%
  select(Country, Variety, sigma) %>%
  rename("repCountry" = Country, "oilType" = Variety)

df_year <- df_year %>%
  left_join(df_within) %>%
  left_join(df_ela %>% rename("repCountry" = Country))

# older dataset
df_ea_2013_2019 <- merge_quality_quantity() %>%
  # restrict country set based on data restrictions (some countries just don't import enough varieties to estimate the elasticity on oil type)
  filter( `Reporting Country` %in% get_ea_countries() )

df_ea_2013_2019 <- df_ea_2013_2019 %>%
  mutate(repCountry = "Euro Area",
         crudeoil = paste(`Country of Origin`, `Type of crude oil`),
         date = as.numeric( substr( date, 1, 4 ) ) ) %>%
  group_by(crudeoil, repCountry,
           `date`, `Weight`, `Sulfur` ) %>%
  summarize( vol = sum( `Volume (1000 bbl)`, na.rm = T ),
             `Total Value ($ 1000)` = sum( `Total Value ($ 1000)`, na.rm = T ) ) %>%
  ungroup() %>%
  mutate(price = `Total Value ($ 1000)` / vol,
         oilType = paste(Weight, Sulfur)) %>%
  filter( `Total Value ($ 1000)` > 0, price > 0, vol > 0) %>%
  left_join(df_within) %>%
  left_join(df_ela %>% rename("repCountry" = Country))


# calculate price index change
# most recent data
df_price_idx_nces <- calc_price_index(x = df_year %>%
                                         filter(repCountry == "Euro Area",
                                                !oilType %in% c("medium NA", "heavy sweet")),
                                       time     = "year",
                                       quantity = "vol",
                                       price    = "price",
                                       variety  = "crudeoil",
                                       good     = "oilType",
                                       sigma    = "sigma",
                                       gamma    = "gamma_nces")

df_de <- df_year %>%
  filter(repCountry == "Germany") %>%
  group_by(year, oilType) %>%
  summarize(exp = sum(vol*price, na.rm = T)) %>%
  group_by(year) %>%
  mutate(share = exp/sum(exp))

df_price_idx_nces <- calc_price_index(x = df_year %>%
                                        filter(repCountry == "Germany",
                                               oilType %in% c("medium sour", "light sweet")),
                                      time     = "year",
                                      quantity = "vol",
                                      price    = "price",
                                      variety  = "crudeoil",
                                      good     = "oilType",
                                      sigma    = "sigma",
                                      gamma    = "gamma_nces") %>%
  select(time, good, priceIndex, exactPriceIndex, lambdaRatio, lambdaRatioCorr,
         expShare, chgPercAggPriceIdx, contPercChgCumExactPriceIdx,
         contPercChgExpShare)


# analyse shares
test <- df_price_idx_nces %>%
  select(time, good, chgLnExpShare) %>%
  na.omit() %>%
  mutate(perChgExpShare = (exp(chgLnExpShare)-1)*100) %>%
  filter(time != 2024) %>%
  select(-chgLnExpShare) %>%
  pivot_wider(names_from = time, values_from = perChgExpShare)



# # old data to compare
# df_price_idx_13_19 <- calc_price_index(x = df_ea_2013_2019,
#                                        time     = "date",
#                                        quantity = "vol",
#                                        price    = "price",
#                                        variety  = "crudeoil",
#                                        good     = "oilType",
#                                        sigma    = "sigma",
#                                        gamma    = "gamma_nces")
#
# df_price_idx_13_19 %>%
#   select(time, chgLnAggPriceIdx, contLnChgExactPriceIdx, contLnChgExpShare) %>%
#   unique()

# prepare for welfare estimation (we compare 2021 with 2023)
df_country_ces <- df %>%
  filter(repCountry == "Euro Area",
         !oilType %in% c("medium NA", "heavy sweet")) %>%
  group_by(year, sourceCountryIso, sourceCountry) %>%
  summarize(vol = sum(volume, na.rm = T),
            price = sum(value, na.rm = T)/sum(volume, na.rm = T)) %>%
  ungroup() %>%
  mutate(oilType = "oil",
         sigma_ces = unlist(df_ela[df_ela$Country == "Euro Area", "sigma_ces"]),
         # gamma just a placeholder
         gamma = 1000) %>%
  filter(price > 0, vol > 0)

df_price_idx_ces <- calc_price_index(x        = df_country_ces,
                                     time     = "year",
                                     quantity = "vol",
                                     price    = "price",
                                     variety  = "sourceCountry",
                                     good     = "oilType",
                                     sigma    = "sigma_ces",
                                     gamma    = "gamma")

# compare ces and nces price index

df_price_idx_nces %>%
  select(time, chgLnAggPriceIdx, contLnChgExactPriceIdx, contLnChgExpShare,
         chgPercAggPriceIdx, contPercChgExactPriceIdx, contPercChgExpShare) %>%
  unique()

df_price_idx_ces %>%
  select(time, chgLnAggPriceIdx, contLnChgExactPriceIdx, contLnChgExpShare, chgPercAggPriceIdx) %>%
  unique()

# calculate an average import price change observed in the data
df_price_chg_data <- df %>%
  filter(repCountry == "Euro Area",
         !oilType %in% c("medium NA", "heavy sweet")) %>%
  group_by(year) %>%
  summarize(vol = sum(volume, na.rm = T),
            price = sum(value, na.rm = T)/sum(volume, na.rm = T)) %>%
  ungroup() %>%
  arrange(year) %>%
  mutate(priceChgPct = (price/lag(price)-1)*100)

df_nces <- df_price_idx_nces %>%
  select(time, chgPercAggPriceIdx, contPercChgExactPriceIdx,
         contPercChgExpShare) %>%
  unique() %>%
  rename("chgAggPidxNces" = chgPercAggPriceIdx,
         "contExactPidxNces" = contPercChgExactPriceIdx,
         "contExpShareNces" = contPercChgExpShare)

df_ces <- df_price_idx_ces %>%
  select(time, chgPercAggPriceIdx) %>%
  unique() %>%
  rename("chgAggPidxCes" = chgPercAggPriceIdx)

# compare average price change in data, CES and nested CES

df_price_chg <- df_price_chg_data %>%
  select(-vol, -price) %>%
  rename("observedAvgChg" = priceChgPct,
         "time" = year) %>%
  left_join(df_ces) %>%
  left_join(df_nces)

# same analysis as above but from 2021 to 2023 (drop 2022)
# calculate price index change
# most recent data
df_price_idx_nces_wo22 <- calc_price_index(x = df_year %>%
                                        filter(repCountry == "Euro Area",
                                               !oilType %in% c("medium NA", "heavy sweet"),
                                               year != 2022) %>%
                                          mutate(year = ifelse(year == 2023, 2022, year)),
                                      time     = "year",
                                      quantity = "vol",
                                      price    = "price",
                                      variety  = "crudeoil",
                                      good     = "oilType",
                                      sigma    = "sigma",
                                      gamma    = "gamma_nces")


# prepare for welfare estimation (we compare 2021 with 2023)
df_country_ces_wo22 <- df %>%
  filter(repCountry == "Euro Area",
         !oilType %in% c("medium NA", "heavy sweet")) %>%
  group_by(year, sourceCountryIso, sourceCountry) %>%
  summarize(vol = sum(volume, na.rm = T),
            price = sum(value, na.rm = T)/sum(volume, na.rm = T)) %>%
  ungroup() %>%
  mutate(oilType = "oil",
         sigma_ces = unlist(df_ela[df_ela$Country == "Euro Area", "sigma_ces"]),
         # gamma just a placeholder
         gamma = 1000) %>%
  filter(price > 0, vol > 0)

df_price_idx_ces_wo22 <- calc_price_index(x        = df_country_ces_wo22 %>%
                                            mutate(year = ifelse(year == 2023, 2022, year)),
                                     time     = "year",
                                     quantity = "vol",
                                     price    = "price",
                                     variety  = "sourceCountry",
                                     good     = "oilType",
                                     sigma    = "sigma_ces",
                                     gamma    = "gamma")

# compare ces and nces price index

df_price_idx_nces_wo22 %>%
  select(time, chgLnAggPriceIdx, contLnChgExactPriceIdx, contLnChgExpShare,
         chgPercAggPriceIdx, contPercChgExactPriceIdx, contPercChgExpShare) %>%
  unique()

df_price_idx_ces_wo22 %>%
  select(time, chgLnAggPriceIdx, contLnChgExactPriceIdx, contLnChgExpShare, chgPercAggPriceIdx) %>%
  unique()

# calculate an average import price change observed in the data
df_price_chg_data_wo22 <- df %>%
  filter(repCountry == "Euro Area",
         !oilType %in% c("medium NA", "heavy sweet"),
         year != 2022) %>%
  mutate(year = ifelse(year == 2023, 2022, year)) %>%
  group_by(year) %>%
  summarize(vol = sum(volume, na.rm = T),
            price = sum(value, na.rm = T)/sum(volume, na.rm = T)) %>%
  ungroup() %>%
  arrange(year) %>%
  mutate(priceChgPct = (price/lag(price)-1)*100)

df_nces_wo22 <- df_price_idx_nces_wo22 %>%
  select(time, chgPercAggPriceIdx, contPercChgExactPriceIdx,
         contPercChgExpShare) %>%
  unique() %>%
  rename("chgAggPidxNces" = chgPercAggPriceIdx,
         "contExactPidxNces" = contPercChgExactPriceIdx,
         "contExpShareNces" = contPercChgExpShare)

df_ces_wo22 <- df_price_idx_ces_wo22 %>%
  select(time, chgPercAggPriceIdx) %>%
  unique() %>%
  rename("chgAggPidxCes" = chgPercAggPriceIdx)

# compare average price change in data, CES and nested CES

df_price_chg_wo22 <- df_price_chg_data_wo22 %>%
  select(-vol, -price) %>%
  rename("observedAvgChg" = priceChgPct,
         "time" = year) %>%
  left_join(df_ces_wo22) %>%
  left_join(df_nces_wo22)
