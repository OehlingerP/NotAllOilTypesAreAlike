#' the idea of this script is to investigate if crude oil imports are less
#' diversified when considered on a country basis compared with a crude oil type
#' or quality basis. The method to show this will be a simple two dimensional
#' bubble chart where API gravity is on the horizontal axis and sulfur content is
#' on the vertical axis. As API gravity cannot be added linearly we will use the
#' mean_api() function for calculating means.

df_raw <- merge_quality_quantity() %>%
  mutate(oil_type = paste(Weight, Sulfur)) %>%
  filter(!is.na(`Typical API gravity`),
         `Reporting Country` %in% get_ea_countries()) %>%
  na.omit()

df_country <- df_raw %>%
  group_by(`Country of Origin`, oil_type) %>%
  summarize(vol = sum(`Volume (1000 bbl)`),
            api = mean_api(`Typical API gravity`, weight = `Volume (1000 bbl)`),
            sul = mean(`Typical sulphur content`))

df_field <- df_raw %>%
  group_by(`Type of crude oil`, `Country of Origin`, oil_type) %>%
  summarize(vol = sum(`Volume (1000 bbl)`),
             api = mean_api(`Typical API gravity`, weight = `Volume (1000 bbl)`),
             sul = mean(`Typical sulphur content`))

df_type <- df_raw %>%
  group_by(oil_type) %>%
  summarize(vol = sum(`Volume (1000 bbl)`),
             api = mean_api(`Typical API gravity`, weight = `Volume (1000 bbl)`),
             sul = mean(`Typical sulphur content`))

plot_field <- ggplot(df_field,
                      aes(x = api, y = sul, size = vol, color = oil_type, shape = oil_type)) +
  geom_point(alpha = 0.75) +
  scale_size(range = c(6, 30), guide = "none") +
  theme(legend.position = "none") +
  scale_shape_manual(values = c(17, 18, 19, 15, 8)) +
  annotate("segment", x = 35, y = 2.5, xend = 32.5, yend = 2.25,
           arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
  annotate("text", x=34,y=2.55, label = "Russian Urals", color = "#011936",
           angle = 0, hjust = 0.1, size = 5, fontface = "bold")

plot_country <- ggplot(df_country,
                        aes(x = api, y = sul, size = vol, color = oil_type, shape = oil_type)) +
  geom_point(alpha = 0.75) +
  scale_size(range = c(6, 30), guide = "none") +
  theme(legend.position = "none") +
  scale_shape_manual(values = c(17, 18, 19, 15, 8)) +
  annotate("segment", x = 31.5, y = 0.1, xend = 34.5, yend = 0.15,
           arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
  annotate("text", x=26,y=0.1, label = "Kazakhstan", color = "#011936",
           angle = 0, hjust = 0.1, size = 5, fontface = "bold")

plot_type <- ggplot(df_type,
                     aes(x = api, y = sul, size = vol, color = oil_type, shape = oil_type)) +
  geom_point(alpha = 0.75) +
  scale_size(range = c(6, 30), guide = "none") +
  theme(legend.position = "none") +
  scale_shape_manual(values = c(17, 18, 19, 15, 8)) +
  annotate("segment", x = 36, y = 2.7, xend = 33.4, yend = 2.5,
           arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
  annotate("text", x=34,y=2.85, label = "81% of which is\nRussian Urals", color = "#011936",
           angle = 0, hjust = 0.1, size = 5, fontface = "bold") +
  annotate("segment", x = 45, y = 0.715, xend = 42, yend = 0.5,
           arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
  annotate("text", x=41,y=0.84, label = "17% of which is\nKazakhstan crude", color = "#011936",
           angle = 0, hjust = 0.1, size = 5, fontface = "bold")

size_min <- min(df_field$vol)
size_max <- max(df_type$vol)

# output each graph separately
plot_field <- gx_theme(plot_field,
                        x_title = "API gravity [°]",
                        y1_title = "Sulfur content [%]",
                        legend_position = "none",
                        #title = "Crude Oil Imports by Oil Field",
                        text_size = 20,
                        text_colour = "black") +
  scale_y_continuous(limits = c(0, 3.5)) +
  scale_x_continuous(limits = c(13, 51)) +
  scale_size(limits = c(size_min, size_max), range = c(6, 36)) +
  theme(axis.text.x = element_text(vjust = .1))

pdf(file.path(FILE_OUTPUT, "figure_1a.pdf")); print(plot_field); dev.off()

plot_country <- gx_theme(plot_country,
                          x_title = "API gravity [°]",
                          y1_title = "Sulfur content [%]",
                          legend_position = "none",
                          #title = "Crude Oil Imports by Source Country",
                          text_size = 20,
                          text_colour = "black") +
  scale_y_continuous(limits = c(0, 3.5)) +
  scale_x_continuous(limits = c(13, 51)) +
  scale_size(limits = c(size_min, size_max), range = c(6, 36)) +
  theme(axis.text.x = element_text(vjust = .1))

pdf(file.path(FILE_OUTPUT, "figure_1b.pdf")); print(plot_country); dev.off()

df_field$within_ela <- ifelse(df_field$oil_type == "light sweet", 0.8, 0.2)

plot_within_ela <- gx_theme(ggplot(df_field,
                                     aes(x = api, y = sul, size = vol, color = oil_type, alpha = within_ela, shape = oil_type)) +
                               geom_point() +
                               scale_size(range = c(6, 30), guide = "none") +
                               theme(legend.position = "none") +
                               scale_shape_manual(values = c(17, 18, 19, 15, 8)),
                             x_title = "API gravity [°]",
                             y1_title = "Sulfur content [%]",
                             legend_position = "none",
                             #title = "Crude Oil Imports by Oil Field",
                             text_size = 20,
                             text_colour = "black") +
  scale_alpha(range = c(0.1, 0.75)) +
  scale_y_continuous(limits = c(0, 3.5)) +
  scale_x_continuous(limits = c(13, 51)) +
  scale_size(limits = c(size_min, size_max), range = c(6, 36)) +
  theme(axis.text.x = element_text(vjust = .1))

pdf(file.path(FILE_OUTPUT, "figure_1c.pdf")); print(plot_within_ela); dev.off()

plot_type <- gx_theme(plot_type,
                       x_title = "API gravity [°]",
                       y1_title = "Sulfur content [%]",
                       legend_position = "none",
                       #title = "Crude Oil Imports by Oil Type",
                       text_size = 20,
                       text_colour = "black") +
  scale_y_continuous(limits = c(0, 3.5)) +
  scale_x_continuous(limits = c(13, 51)) +
  scale_size(limits = c(size_min, size_max), range = c(6, 36)) +
  theme(axis.text.x = element_text(vjust = .1))

pdf(file.path(FILE_OUTPUT, "figure_1d.pdf")); print(plot_type); dev.off()

