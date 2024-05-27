library(NotAllOilTypesAreAlike)
library(stringr)
df_raw <- readRDS(file = "data/eurostat_eu_crude_oil_imports (update).RData")

# import country metadata to match full country names
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
  filter(volume != 0) %>%
  mutate(month = as.Date(paste0(month, "-01")) + months(1)-1) %>%
  # correct wrong (source) country names
  mutate(year = as.numeric(substr(month, 1, 4)),
         weight = ifelse(api > 31.1, "light",
                         ifelse(api >=  22.3, "medium", "heavy")),
         sulfur = ifelse(sul < 1, "sweet", "sour"),
         oilType = paste(weight, sulfur, sep = " ")) %>%
  filter(crudeoil != "TOTAL")

# add Euro Area
df_ea <- df %>%
  filter(repCountryIso %in% get_ea_countries(type = "iso2")) %>%
  mutate(repCountryIso = "EA")

df <- rbind(df, df_ea)

# prepare for welfare estimation (we compare 2021 with 2023)
df_year <- df %>%
  group_by(year, repCountryIso, sourceCountryIso,
           crudeoil, oilType) %>%
  summarize(vol = sum(volume, na.rm = T),
            price = sum(value, na.rm = T)/sum(volume, na.rm = T)) %>%
  # normalize
  group_by(repCountryIso, year) %>%
  mutate(volNorm = vol / sum(vol) * 100) %>%
  ungroup()

# load elasticity estimates
df_within <- readRDS("data/elasticities_within.RDS") %>%
  group_by(iso2) %>%
  summarize(sigma = mean(sigma, na.rm = T)) %>%
  rename("sigma_nces" = sigma)

df_ela <- readRDS("data/elasticities_across.RDS") %>%
  select(iso2, Variety, sigma) %>%
  pivot_wider(names_from = Variety, values_from = sigma) %>%
  rename("sigma_ces" = `Country of Origin`,
         "gamma_nces" = `Oil Type`) %>%
  select(-`Oil Field`) %>%
  left_join(df_within) %>%
  na.omit()

df_within <- readRDS("data/elasticities_within.RDS") %>%
  select(iso2, Variety, sigma) %>%
  rename("repCountryIso" = iso2, "oilType" = Variety)

df_year <- df_year %>%
  left_join(df_within) %>%
  left_join(df_ela %>% rename("repCountryIso" = iso2))

# calculate price index change
# most recent data
df_price_idx_nces <- calc_price_index(x = df_year %>%
                                         filter(repCountryIso == "EA",
                                                !oilType %in% c("medium NA", "heavy sweet")),
                                       time     = "year",
                                       quantity = "vol",
                                       price    = "price",
                                       variety  = "crudeoil",
                                       good     = "oilType",
                                       sigma    = "sigma",
                                       gamma    = "gamma_nces")

unique(df_price_idx_nces %>%
         select("time", "chgPercAggPriceIdx",
                "contPercChgCumExactPriceIdx", "contPercChgExpShare"))

df_country_ces <- df %>%
  filter(repCountryIso == "EA",
         !oilType %in% c("medium NA", "heavy sweet")) %>%
  group_by(year, sourceCountryIso) %>%
  summarize(vol = sum(volume, na.rm = T),
            price = sum(value, na.rm = T)/sum(volume, na.rm = T)) %>%
  ungroup() %>%
  mutate(oilType = "oil",
         sigma_ces = unlist(df_ela[df_ela$iso2 == "EA", "sigma_ces"]),
         # gamma just a placeholder
         gamma = 1000) %>%
  filter(price > 0, vol > 0)

df_price_idx_ces <- calc_price_index(x        = df_country_ces,
                                     time     = "year",
                                     quantity = "vol",
                                     price    = "price",
                                     variety  = "sourceCountryIso",
                                     good     = "oilType",
                                     sigma    = "sigma_ces",
                                     gamma    = "gamma")

unique(df_price_idx_ces %>%
         select("time", "chgPercAggPriceIdx",
                "contPercChgCumExactPriceIdx", "contPercChgExpShare"))
