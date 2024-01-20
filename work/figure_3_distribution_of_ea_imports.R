df_ea <- merge_quality_quantity() %>%
  filter(`Reporting Country` %in% get_ea_countries())

df_ea <- df_ea %>%
  mutate(`Reporting Country` = "Euro Area",
         date = as.numeric(substr(date, 1, 4))) %>%
  group_by(`Country of Origin`, `Type of crude oil`, `Reporting Country`,
            `date`, `Weight`, `Sulfur`) %>%
  summarize(`Volume (1000 bbl)` = sum(`Volume (1000 bbl)`, na.rm = T),
             `Total Value ($ 1000)` = sum(`Total Value ($ 1000)`, na.rm = T)) %>%
  ungroup() %>%
  mutate(`CIF price (2) ($/bbl)` = `Total Value ($ 1000)` / `Volume (1000 bbl)`,
          date = as.Date(paste0(date, "-12-31"))) %>%
  filter(`Total Value ($ 1000)` > 0, `CIF price (2) ($/bbl)` > 0, `Volume (1000 bbl)` > 0)

df <- df_ea %>%
  mutate(oil_type = paste(Weight, Sulfur)) %>%
  rename("country" = `Country of Origin`) %>%
  group_by(country, oil_type) %>%
  summarize(volume = sum(`Volume (1000 bbl)`)) %>%
  filter(volume > 5000) %>%
  filter(oil_type %in% c("light sweet", "medium sour")) %>%
  pivot_wider(names_from = oil_type, values_from = volume) %>%
  pivot_longer(-country) %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  mutate(value = value / 1000,
         country = ifelse(country == "Russian Federation", "Russia",
                          ifelse(country == "United Kingdom", "UK",
                                 ifelse(country == "United States", "USA", country))))


plot <- ggplot(df, aes(x = country, y = value)) +
  geom_col_pattern(aes(pattern = name),
                   position        = "dodge",
                   fill            = 'white',
                   colour          = 'black',
                   pattern_spacing = 0.015) +
  geom_col_pattern(data            = filter(df, country == "Russia"),
                   mapping         = aes(fill = name, pattern = name),
                   position        = "dodge",
                   pattern_spacing = 0.015)

plot <- gx_theme(plot,
                 text_size = 12,
                 text_colour = "black",
                 y1_title = "Crude oil imports in million barrels") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1.05, vjust = 0.25))

pdf(file.path(FILE_OUTPUT, "figure_3.pdf")); print(plot); dev.off()
