# import country metadata to match full country names
df <- prepare_eurostat()

# Figure 4 Panel a
df_plot <- df %>%
  filter(repCountryIso == "EU", sourceCountryIso %in% c("RU", "US")) %>%
  mutate(sourceCountryIso = ifelse(sourceCountryIso == "RU",
                                   "Crude oil imports from Russia",
                                   "Crude oil imports from the U.S.")) %>%
  group_by(month, sourceCountryIso) %>%
  summarize(volume = sum(volume),
            value = sum(value)) %>%
  mutate(avgPriceBBl = value / volume)

# continue missing RU imports with zeros (as they are true zeros)
df_plot_vol <- df_plot %>%
  select(month, sourceCountryIso, volume) %>%
  pivot_wider(names_from = month, values_from = volume)

df_plot_vol[is.na(df_plot_vol)] <- 0

df_plot_vol <- df_plot_vol %>%
  pivot_longer(-sourceCountryIso, names_to = "month", values_to = "volume") %>%
  mutate(month = as.Date(month),
         volume = volume/1000)

plot_vol <- ggplot(df_plot_vol, aes(x = month, y = volume, color = sourceCountryIso)) +
  geom_point(size = 1.2) +
  geom_line(size = 1.1) +
  geom_vline(xintercept = as.Date("2022-12-05"), color = "black", size = 1.2) +
  annotate("text", x=as.Date("2022-12-05"),y=63.5, label = "EU embargo\ntakes effect", color = "black",
           hjust = -0.025, size = 6, fontface = "bold")

plot_vol <- gx_theme(plot_vol,
                            y1_title = "Crude oil imports in million barrels",
                            text_size = 18,
                            legend_rows = 2,
                            text_colour = "black") +
  scale_x_date(limit = c(as.Date("2020-01-01"), as.Date("2023-12-30")))

pdf(file.path(FILE_OUTPUT, "figure_4a.pdf")); print(plot_vol); dev.off()

df_oil_quality_ru <- df %>%
  filter(repCountryIso == "EU", sourceCountryIso %in% c("RU"),
         month <= as.Date("2022-10-31"),
         month > as.Date("2021-10-31")) %>%
  mutate(volShare = volume/sum(volume),
         meanApi = mean_api(api, volShare),
         meanSul = sum(sul*volShare)) %>%
  select(meanApi, meanSul) %>%
  unique()

df_oil_quality_us <- df %>%
  filter(repCountryIso == "EU", sourceCountryIso %in% c("US"),
         month > as.Date("2022-10-31")) %>%
  mutate(volShare = volume/sum(volume),
         meanApi = mean_api(api, volShare),
         meanSul = sum(sul*volShare)) %>%
  select(meanApi, meanSul) %>%
  unique()

df_plot_qual <- df %>%
  filter(repCountryIso == "EU", sourceCountryIso %in% c("RU", "US"),
         ((sourceCountryIso == "RU" & month <= as.Date("2022-10-31")) |
            (sourceCountryIso == "US" & month > as.Date("2022-10-31")))) %>%
  mutate(sulType = ifelse(sul < 1, "sweet", "sour"),
         apiType = ifelse(api < 22.3, "heavy", ifelse(api > 31.1, "light", "medium")),
         oilType = paste(apiType, sulType))

plot_quality_us_ru <- df_plot_qual %>%
  ggplot(aes(x = api, y = sul,
             size  = volume,
             color = sourceCountryIso,
             shape = sourceCountryIso)) +
  geom_point(alpha = 0.75 ) +
  scale_size(range = c(2, 14), guide = "none" ) +
  scale_shape_manual(values = c(15, 19))

plot_quality_us_ru <- gx_theme(plot_quality_us_ru,
          x_title     = "API gravity [°]",
          y1_title    = "Sulfur content [%]",
          legend_rows = 2,
          text_size   = 18,
          text_colour = "black") +
  theme(axis.text.x   = element_text(vjust = .1)) +
  scale_color_manual(values = c(gx_colors()[1:2]),
    labels = c("RU" = "Pre-embargo crude oil imports from Russia",
               "US" = "Post-embargo crude oil imports from the U.S.")) +
  scale_shape_manual(
    values = c(15,19),
    labels = c("RU" = "Pre-embargo crude oil imports from Russia",
               "US" = "Post-embargo crude oil imports from the U.S.")) +
  guides(color = guide_legend(nrow = 2),
         shape = guide_legend(nrow = 2, override.aes = list(size = 4)))

pdf(file.path(FILE_OUTPUT, "figure_4b.pdf")); print(plot_quality_us_ru); dev.off()
