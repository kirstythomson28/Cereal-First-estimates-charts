## Code for producing the plots used in the cereals first estimates publication.
## Adapted from code used for the final ests. publication.

##NB: The svg plots will likely have some overlapping text issues; these are easier to sort manually in Inkscape than by
## fickering with the x/y coordinates in the code.

library(tidyverse)
library(readxl)
library(ggrepel)
library(ggplot2)
library(scales)
library(styler)
library(extrafont)

#Define the harvest year here
CurrentYear = 2025
#Set up graph parameters based on harvest year
# TenYearsAgo = CurrentYear-10
xlimits = c(CurrentYear-9, CurrentYear)
xbreaks = c((CurrentYear-9):CurrentYear)
xaxislabels = c(CurrentYear-9, "",
                CurrentYear-7, "",
                CurrentYear-5, "",
                CurrentYear-3, "",
                CurrentYear-1, "")
if(CurrentYear %% 2 == 0){
  xaxislabels=c(" ", CurrentYear-8,
                " ", CurrentYear-6, 
                " ", CurrentYear-4,
                " ", CurrentYear-2,
                " ", CurrentYear)
}
SVGWidth <- 225
SVGHeight <- 141

#Use the resas theme (modified to remove some grid lines)
resas_theme <- source("resas_theme_modified.R")

## Load data from Tables and charts excel document 
ch_data <- read_csv("CH_data.csv")

#filter for production values from the past 10 years
production <- ch_data %>%
  select(c(Year, contains("production"))) %>%
  filter(Year > (CurrentYear-10))


########################### Total cereals ######################################
df_total_cereals <- data.frame(production$Year, production$Cereals_Production) %>%
  setNames(c("Year", "Cereals_Production"))

df_total_cereals_mean_5yr <- df_total_cereals %>%
  filter(Year >= 2020 & Year <= 2024) %>%
  summarise(mean_production = mean(Cereals_Production, na.rm = TRUE))

Crop <- ggplot(df_total_cereals, aes(Year)) +
  # Five-year average line
  geom_segment(
    data = df_total_cereals_mean_5yr,
    aes(
      x = 2020, xend = 2025,
      y = mean_production, yend = mean_production,
      color = "Five-year average (2020:2024)"
    ),
    size = 0.75
  ) +
  
  # Cereals production: past (solid)
  geom_line(
    data = subset(df_total_cereals, Year < CurrentYear),
    aes(y = Cereals_Production, color = "Total cereals production"),
    size = 2, linetype = "solid"
  ) +
  
  # Cereals production: recent/projection (dashed)
  geom_line(
    data = subset(df_total_cereals, Year > CurrentYear - 2),
    aes(y = Cereals_Production, color = "Total cereals production"),
    size = 2, linetype = "11"
  ) +
  
  # Current year point (excluded from legend)
  geom_point(
    data = subset(df_total_cereals, Year == CurrentYear),
    aes(y = Cereals_Production, color = "Total cereals production"),
    size = 5,
    show.legend = FALSE
  ) +
  
  # Current year label (excluded from legend)
  geom_text(
    data = subset(df_total_cereals, Year == CurrentYear),
    aes(
      y = Cereals_Production,
      label = format((round(Cereals_Production, -3) / 1000), big.mark = ","),
      color = "Total cereals production"
    ),
    size = 6, vjust = -1,
    show.legend = FALSE
  ) +
  
  # Make legend text same color as lines
  guides(
    color = guide_legend(
      override.aes = list(
        color = c("#00833E", "black")   # match legend key colors
      )
    )
  ) +
  
  # Manual legend colors
  scale_color_manual(
    name = NULL,
    values = c(
      "Total cereals production" = "#00833E",
      "Five-year average (2020:2024)" = "black"
    ),
    breaks = c(
      "Total cereals production",
      "Five-year average (2020:2024)"
    )
  ) +
  # Axis and theme
  scale_y_continuous(
    labels = scales::label_comma(scale = 1 / 1000, accuracy = 1),
    limits = c(0, 3500000),
    breaks = c(0, 500000, 1500000, 2500000, 3500000)
  ) +
  scale_x_continuous(limits = xlimits, breaks = xbreaks, labels = xaxislabels
                     ) +
  labs(y = "Thousand tonnes", x = "Year") +
  theme_set(theme_resas) +
  theme(
    plot.title = element_text(size = 17, hjust = 0.5),
    axis.title = element_text(size = 19),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    strip.text = element_text(size = 15),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 16),
    legend.box = "horizontal"
  )


Crop

ggsave(filename = paste0("CH_",CurrentYear,"_cereals_production.svg"), plot = Crop, width = SVGWidth, height = SVGHeight, units = "mm", dpi = "retina", bg = "white")


################################################################################
############################## Barley Crop #####################################

df_barley <- ch_data %>%
  select(c(Year, contains("Barley_Production"))) %>%
  filter(Year > (CurrentYear-10)) %>%
  as.data.frame()

# Compute 5-year averages (2020–2024) for both types
df_barley_mean_5yr <- df_barley %>%
  filter(Year >= 2020 & Year <= 2024) %>%
  summarise(
    S_Barley_Production = mean(S_Barley_Production, na.rm = TRUE),
    W_Barley_Production = mean(W_Barley_Production, na.rm = TRUE)
  )

Crop <- ggplot(df_barley, aes(Year)) +
  # --- 5-year average line: Spring barley ---
  geom_segment(
    data = df_barley_mean_5yr,
    aes(
      x = 2020, xend = 2025,
      y = S_Barley_Production, yend = S_Barley_Production,
      color = "Five-year average (2020:2024)"
    ),
    linetype = "solid", size = 0.75
  ) +
  
  # --- 5-year average line: Winter barley ---
  geom_segment(
    data = df_barley_mean_5yr,
    aes(
      x = 2020, xend = 2025,
      y = W_Barley_Production, yend = W_Barley_Production,
      color = "Five-year average (2020:2024)"
    ),
    linetype = "solid", size = 0.75
  ) +
  # Manual colors for legend
  scale_color_manual(
    name = NULL,
    values = c(
      "Five-year average (2020:2024)" = "black",
      "Spring barley production" = "#00833E",
      "Winter barley production" = "#00833E"
    ),
    breaks = c(
      "Spring barley production",
      "Winter barley production",
      "Five-year average (2020:2024)"
    )
  )+
  scale_y_continuous(
    labels = scales::label_comma(scale = 1 / 1000, prefix = "", suffix = "", accuracy = 1, big.mark = ","), limits = c(0, 2000000), breaks = c(0, 500000, 1000000, 1500000, 2000000)
  ) +
  scale_x_continuous(
    limits = xlimits, breaks = xbreaks, labels = xaxislabels
  ) +
  geom_line(data=subset(df_barley, Year<CurrentYear),
            aes(y = S_Barley_Production),
            color = "#00833E", size = 2, linetype = "solid"
  ) +
  geom_line(data=subset(df_barley, Year>CurrentYear-2),
            aes(y = S_Barley_Production),
            color = "#00833E", size = 2, linetype = '11'
  ) +
  geom_point(data=subset(df_barley, Year==CurrentYear),
             aes(y = S_Barley_Production),
             color = "#00833E", size = 5
  )+
  annotate(
    "text",
    x = CurrentYear, y = df_barley$S_Barley_Production[df_barley$Year==CurrentYear]+100000, 
    label = format((round(df_barley$S_Barley_Production[df_barley$Year==CurrentYear],-3)/1000), big.mark=","), 
    size = 6, color = "#00833E"
  )+
  annotate(
    "text",
    x = CurrentYear-7.5, y = 1650000, label = "Spring barley production", size = 6, color = "#00833E"
  ) +
  geom_line(data=subset(df_barley, Year<CurrentYear),
            aes(y = W_Barley_Production),
            color = "#00833E", size = 2, linetype = "solid"
  ) +  
  geom_line(data=subset(df_barley, Year>CurrentYear-2),
            aes(y = W_Barley_Production),
            color = "#00833E", size = 2, linetype = '11'
  ) +
  geom_point(data=subset(df_barley, Year==CurrentYear),
             aes(y = W_Barley_Production),
             color = "#00833E", size = 5
  )+
  annotate(
    "text",
    x = CurrentYear, y = df_barley$W_Barley_Production[df_barley$Year==CurrentYear]+100000, 
    label = paste0(round(df_barley$W_Barley_Production[df_barley$Year==CurrentYear],-3)/1000), 
    size = 6, color = "#00833E"
  )+
  annotate(
    "text",
    x = CurrentYear-7.5, y =  480000, label = "Winter barley production", size = 6, color = "#00833E"
  ) +
  labs(
    title = "",  y = "Thousand tonnes", x = "Year"
  ) +
  theme_set(
    theme_resas
  ) +
  theme(
    plot.title = element_text(size = 17, hjust = 0.5),
    axis.title = element_text(size = 19),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    strip.text = element_text(size = 15),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.text = element_text(size = 16),
  )

Crop

ggsave(filename = paste0("CH_",CurrentYear,"_barley_production.svg"), plot = Crop, width = SVGWidth, height = SVGHeight, units = "mm", dpi = 320, bg = "white")


################################################################################
#################################### Oats Crop #################################

df_oats <- ch_data %>%
  select(c(Year, contains("Oats_Production"))) %>%
  filter(Year > (CurrentYear-10)) %>%
  as.data.frame()

df_oats_mean_5yr <- df_oats %>%
  filter(Year >= 2020 & Year <= 2024) %>%
  summarise(Oats_Production = mean(Oats_Production, na.rm = TRUE))

Crop <- ggplot(df_oats, aes(Year)) +
  # 5-year average line (2020–2024)
  geom_segment(
    data = df_oats_mean_5yr,
    aes(
      x = 2020, xend = 2025,
      y = Oats_Production, yend = Oats_Production,
      color = "Five-year average (2020:2024)"
    ),
    size = 0.75
  ) +
  # Oats production: past (solid)
  geom_line(data=subset(df_oats, Year < CurrentYear),
            aes(y = Oats_Production, colour = "Oats production"),
            size = 2, linetype = "solid"
  ) +
  # Cereals production: recent/projection (dashed)
  geom_line(data=subset(df_oats, Year>CurrentYear-2),
            aes(y = Oats_Production, colour = "Oats production"),
            size = 2, linetype = "solid"
  ) +
  # Current year point (excluded from legend)
  geom_point(data=subset(df_oats, Year==CurrentYear),
             aes(y = Oats_Production, colour = "Oats production"),
             size = 5,
             show.legend = FALSE
  )+
  # Current year label (excluded from legend)
  geom_text(
    data = subset(df_oats, Year == CurrentYear),
    aes(
      y = Oats_Production,
      label = format((round(Oats_Production, -3) / 1000), big.mark = ","),
      color = "Oats production"
    ),
    size = 6, vjust = -1,
    show.legend = FALSE
  ) +
  
  # Make legend text same color as lines
  guides(
    color = guide_legend(
      override.aes = list(
        color = c("#00833E", "black")   # match legend key colors
      )
    )
  ) +
  # Manual legend colors
  scale_color_manual(
    name = NULL,
    values = c(
      "Oats production" = "#00833E",
      "Five-year average (2020:2024)" = "black"
    ),
    breaks = c(
      "Oats production",
      "Five-year average (2020:2024)"
    )
  ) +
  scale_y_continuous(
    labels = scales::label_comma(scale = 1 / 1000, prefix = "", suffix = "", accuracy = 1, big.mark = ","), limits = c(0, 250000), breaks = c(0, 50000, 100000, 150000, 200000, 250000, 300000)
  ) +
  scale_x_continuous(
    limits = xlimits, breaks = xbreaks, labels = xaxislabels
  ) +
  labs(y = "Thousand tonnes", x = "Year") +
  theme_set(theme_resas
  ) +
  theme(
    plot.title = element_text(size = 17, hjust = 0.5),
    axis.title = element_text(size = 19),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    strip.text = element_text(size = 15),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.text = element_text(size = 16),
  )

Crop

ggsave(filename = paste0("CH_",CurrentYear,"_oats_production.svg"), Crop, width = SVGWidth, height = SVGHeight, units = "mm", dpi = "retina", bg = "white")

####################################################################################################
# Alex - Wheat Crop

df_wheat <- ch_data %>%
  select(c(Year, contains("Wheat_Production"))) %>%
  filter(Year > CurrentYear-10) %>%
  as.data.frame()

df_wheat_mean_5yr <- df_wheat %>%
  filter(Year >= 2020 & Year <= 2024) %>%
  summarise(Wheat_Production = mean(Wheat_Production, na.rm = TRUE))

Crop <- ggplot(df_wheat, aes(Year)) +
  # 5-year average line (2020–2024)
  geom_segment(
    data = df_wheat_mean_5yr,
    aes(
      x = 2020, xend = 2025,
      y = Wheat_Production, yend = Wheat_Production,
      color = "Five-year average (2020:2024)"
    ),
    size = 0.75
  ) +
  
  # Wheat production: past (solid)
  geom_line(
    data = subset(df_wheat, Year < CurrentYear),
    aes(y = Wheat_Production, color = "Wheat production"),
    size = 2, linetype = "solid"
  ) +
  
  # Wheat production: recent/projection (dashed)
  geom_line(
    data = subset(df_wheat, Year > CurrentYear - 2),
    aes(y = Wheat_Production, color = "Wheat production"),
    size = 2, linetype = "11"
  ) +
  
  # Current year point (excluded from legend)
  geom_point(
    data = subset(df_wheat, Year == CurrentYear),
    aes(y = Wheat_Production, color = "Wheat production"),
    size = 5,
    show.legend = FALSE
  ) +
  
  # Current year label (excluded from legend)
  geom_text(
    data = subset(df_wheat, Year == CurrentYear),
    aes(
      y = Wheat_Production,
      label = format((round(Wheat_Production, -3) / 1000), big.mark = ","),
      color = "Wheat production"
    ),
    size = 6, vjust = -1,
    show.legend = FALSE
  ) +
  
  # Make legend text same color as lines
  guides(
    color = guide_legend(
      override.aes = list(
        color = c("#00833E", "black")   # match legend key colors
      )
    )
  ) +
  
  # Manual legend colors
  scale_color_manual(
    name = NULL,
    values = c(
      "Wheat production" = "#00833E",
      "Five-year average (2020:2024)" = "black"
    ),
    breaks = c(
      "Wheat production",
      "Five-year average (2020:2024)"
    )
  ) +
  # Axis and theme
  scale_y_continuous(
    labels = scales::label_comma(scale = 1 / 1000, prefix = "", suffix = "", accuracy = 1, big.mark = ","), limits = c(0, 1250000), breaks = c(0, 250000, 500000, 750000, 1000000, 1250000)
  ) +
  scale_x_continuous(
    limits = xlimits, breaks = xbreaks, labels = xaxislabels
  ) +
  labs(y = "Thousand tonnes", x = "Year") +
  theme_set(theme_resas) +
  theme(
    plot.title = element_text(size = 17, hjust = 0.5),
    axis.title = element_text(size = 19),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    strip.text = element_text(size = 15),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 16),
    legend.box = "horizontal"
  )


Crop

ggsave(filename = paste0("CH_",CurrentYear,"_wheat_production.svg"), plot = Crop, width = SVGWidth, height = SVGHeight, units = "mm", dpi = "retina", bg = "white")


####################################################################################################
# Alex - OSR Crop

df_osr <- ch_data %>%
  select(c(Year, contains("OSR_Production"))) %>%
  filter(Year > CurrentYear-10) %>%
  as.data.frame()

df_osr_mean_5yr <- df_osr %>%
  filter(Year >= 2020 & Year <= 2024) %>%
  summarise(OSR_Production = mean(OSR_Production, na.rm = TRUE))

Crop <- ggplot(df_osr, aes(Year)) +
  # 5-year average line (2020–2024)
  geom_segment(
    data = df_osr_mean_5yr,
    aes(
      x = 2020, xend = 2025,
      y = OSR_Production, yend = OSR_Production,
      color = "Five-year average (2020:2024)"
    ),
    size = 0.75
  ) +
  
  # Oilseed rape production: past (solid)
  geom_line(
    data = subset(df_osr, Year < CurrentYear),
    aes(y = OSR_Production, color = "Oilseed rape production"),
    size = 2, linetype = "solid"
  ) +
  
  # Oilseed rape production: recent/projection (dashed)
  geom_line(
    data = subset(df_osr, Year > CurrentYear - 2),
    aes(y = OSR_Production, color = "Oilseed rape production"),
    size = 2, linetype = "11"
  ) +
  
  # Current year point (excluded from legend)
  geom_point(
    data = subset(df_osr, Year == CurrentYear),
    aes(y = OSR_Production, color = "Oilseed rape production"),
    size = 5,
    show.legend = FALSE
  ) +
  
  # Current year label (excluded from legend)
  geom_text(
    data = subset(df_osr, Year == CurrentYear),
    aes(
      y = OSR_Production,
      label = format((round(OSR_Production, -3) / 1000), big.mark = ","),
      color = "Oilseed rape production"
    ),
    size = 6, vjust = -1,
    show.legend = FALSE
  ) +
  
  # Make legend text same color as lines
  guides(
    color = guide_legend(
      override.aes = list(
        color = c("#00833E", "black")   # match legend key colors
      )
    )
  ) +
  
  # Manual legend colors
  scale_color_manual(
    name = NULL,
    values = c(
      "Oilseed rape production" = "#00833E",
      "Five-year average (2020:2024)" = "black"
    ),
    breaks = c(
      "Oilseed rape production",
      "Five-year average (2020:2024)"
    )
  ) +
  # Axis and theme
  scale_y_continuous(
    labels = scales::label_comma(scale = 1 / 1000, prefix = "", suffix = "", accuracy = 1, big.mark = ","), limits = c(0, 200000)
  ) +
  scale_x_continuous(
    limits = xlimits , breaks = xbreaks, labels = xaxislabels
  ) +
  labs(y = "Thousand tonnes", x = "Year") +
  theme_set(theme_resas) +
  theme(
    plot.title = element_text(size = 17, hjust = 0.5),
    axis.title = element_text(size = 19),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    strip.text = element_text(size = 15),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.text = element_text(size = 16),
  )


Crop

ggsave(filename = paste0("CH_",CurrentYear,"_osr_production.svg"), plot = Crop, width = SVGWidth, height = SVGHeight, units = "mm", dpi = "retina", bg = "white")

####################################################################################################
# ALEX - PIE CHART, WHEAT OATs BARLEY
# 
# library(dplyr)
# library(plotly)
# library(reticulate)
# 
# ch_data_pie <- ch_data %>%
#   select(c(Year, contains("Production"))) %>%
#   filter(Year == CurrentYear) %>%
#   mutate(Barley_Production = S_Barley_Production + W_Barley_Production) %>%
#   select(Barley_Production, Wheat_Production, Oats_Production)
# 
# ch_data_pie <- as.data.frame(t(ch_data_pie))
# ch_data_pie <- setNames(ch_data_pie, c("Value"))
# ch_data_pie$per <- 100*ch_data_pie$Value/sum(ch_data_pie$Value)
# ch_data_pie$Crop <- c('Barley','Wheat','Oats')
# ch_data_pie$per <- round(ch_data_pie$per, digits = 0)
# 
# # create plot labels
# labels = paste0(ch_data_pie$Crop, "\n ",ch_data_pie$per, big.mark = "%")
# 
# # create plot
# pie_plot <- plot_ly(ch_data_pie,
#                     labels = ~labels,
#                     values = ~per, type = 'pie',
#                     textposition = 'outside',
#                     textinfo = 'label',
#                     hoverinfo = 'text',
#                     text = ~paste(signif(ch_data_pie$Value/1000,digits = 5), "Thousand tonnes"),
#                     marker = list(colors=c("#3ED581","#575756", "#00833E"), line = list(color = "White", width = 7))) %>%
#   layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#          font = list(family = "Arial",size = 30, color = "black"),
#          showlegend = FALSE)
# 
# pie_plot