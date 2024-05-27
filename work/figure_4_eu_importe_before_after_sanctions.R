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
  filter(volume != 0) %>%
  mutate(month = as.Date(paste0(month, "-01")) + months(1)-1) %>%
  mutate(year = as.numeric(substr(month, 1, 4)),
         weight = ifelse(api > 31.1, "light",
                         ifelse(api >=  22.3, "medium", "heavy")),
         sulfur = ifelse(sul < 1, "sweet", "sour"),
         oilType = paste(weight, sulfur, sep = "_")) %>%
  filter(crudeoil != "TOTAL")

# EU Import Quality 2021 vs 2023 ------------------------------------------
size_min <- min(df$volume, na.rm = T)
size_max <- max(df$volume, na.rm = T)

# Same graphs but aggregated by oil field ---------------------------------
size_min <- min(df$volume, na.rm = T)
size_max <- max(df$volume, na.rm = T)

for(i in 2020:2023){

  temp <- df %>%
    filter(year == i,
           repCountryIso == "EU_V") %>%
    group_by(crudeoil, oilType) %>%
    summarize(volume = sum(volume, na.rm = T),
              api = mean_api(api, weight = volume),
              sul = mean(sul))

  # add heavy light crude oil to all datasets but not visible in graphics
  # to keep legend ordering for all graphs
  add_light_heavy <- temp[1, ]
  add_light_heavy$crudeoil <- "legend"
  add_light_heavy$oilType <- "heavy_sweet"
  add_light_heavy[ , c(3, 5)] <- 0

  temp <- rbind(temp, add_light_heavy)

  temp_ru <- df %>%
    filter(year == i,
           repCountryIso == "EU_V",
           sourceCountryIso == "RU") %>%
    group_by(crudeoil, oilType) %>%
    summarize(volume = sum(volume, na.rm = T),
              api = mean_api(api, weight = volume),
              sul = mean(sul))

  shape <- c(17, 18, 19, 15, 8)
  color_order <- gx_colors()[c(1, 2, 3, 4, 5)]


  if(length(unique(temp$oilType)) == 6){

    shape <- c(17, 9, 18, 19, 15, 8)
    color_order <- gx_colors()[c(2, 6, 1, 3, 4, 5)]

  }

  plot <- ggplot(temp,
                 aes(x = api, y = sul, size = volume, color = oilType, shape = oilType ) ) +
    geom_point( alpha = 0.75 ) +
    geom_point(data = temp_ru, color = "red", alpha = 0.8) +
    scale_size(range = c(6, 36), guide = "none" ) +
    theme( legend.position = "none" ) +
    scale_shape_manual(values = shape) +
    scale_color_manual(values = color_order)

  plot <- gx_theme(plot,
                   x_title = "API gravity [°]",
                   y1_title = "Sulfur content [%]",
                   legend_position = "none",
                   title = i,
                   colours = color_order,
                   text_size = 20,
                   text_colour = "black" ) +
    scale_y_continuous( limits = c( 0, 6 ) ) +
    scale_x_continuous( limits = c( 5, 51 ) ) +
    scale_size( limits = c(size_min, size_max ), range = c(6, 36) ) +
    theme(axis.text.x = element_text(vjust = .1))

  if(i == 2020){

    plot <- plot +
      annotate("segment", x = 39, y = 1.78, xend = 34, yend = 1.5, color = "red",
               arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
      annotate("segment", x = 39.2, y = 1.78, xend = 37.8, yend = 1.05, color = "red",
               arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
      annotate("text", x=37.5,y=1.9, label = "Russia", color = "red",
               angle = 0, hjust = 0.1, size = 5, fontface = "bold")

  }

  if(i == 2021){

    plot <- plot +
      annotate("segment", x = 39, y = 1.78, xend = 35.1, yend = 1.5, color = "red",
               arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
      annotate("segment", x = 39.2, y = 1.78, xend = 35.7, yend = 0.81, color = "red",
               arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
      annotate("text", x=37.5,y=1.9, label = "Russia", color = "red",
               angle = 0, hjust = 0.1, size = 5, fontface = "bold")

  }

  if(i == 2022){

    plot <- plot +
      annotate("segment", x = 39, y = 1.78, xend = 35.1, yend = 1.5, color = "red",
               arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
      annotate("segment", x = 39.2, y = 1.78, xend = 37, yend = 1.2, color = "red",
               arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
      annotate("text", x=37.5,y=1.9, label = "Russia", color = "red",
               angle = 0, hjust = 0.1, size = 5, fontface = "bold")

  }

  if(i == 2023){

    plot <- plot +
      annotate("text", x=43,y=0.9, label = "Russia", color = "red",
               angle = 0, hjust = 0.1, size = 5, fontface = "bold")

  }

  assign(paste0("plot_", i), plot)

  #pdf(file = paste0("figures/eu_imports_oil_quality_", i, ".pdf"))
  print(plot)
  #dev.off()
}


p <- ggplot( temp %>% mutate(oilType = gsub("_", " ", oilType)),
             aes(x = api, y = sul, size = volume, color = oilType, shape = oilType) ) +
  geom_point( alpha = 0.75 ) +
  scale_shape_manual( values = c(17, 9, 18, 19, 15, 8)) +
  scale_color_manual(values = color_order) +
  theme(legend.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 16) )+
  guides(size = "none",
         shape = guide_legend(override.aes = list(size = 5)))

# Extract the legend. Returns a gtable
leg <- ggpubr::get_legend(p)

#pdf("figures/eu_imports_oil_quality_20_23.pdf", width = 10, height = 0.8 )
print(grid::grid.draw(leg))
#dev.off()

# pdf(file = "figures/eu_imports_oil_quality_2020_2023_oilfield.pdf")
# gridExtra::grid.arrange(plot_2020, plot_2021, plot_2022, plot_2023)
# dev.off()
