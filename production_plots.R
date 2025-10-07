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
# setwd("")
ch_data <- read_csv("CH_data.csv")

production <- ch_data %>%
  select(c(Year, contains("production"))) %>%
  filter(Year > (CurrentYear-10))

# ALEX - Reformat the way plots are created ###################################################################################################

df <- data.frame(production$Year, production$Cereals_Production) %>%
  setNames(c("Year", "Cereals_Production"))

df_mean_5yr <- df %>%
  filter(Year >= 2020 & Year <= 2024) %>%
  summarise(mean_production = mean(Cereals_Production, na.rm = TRUE))

Crop <- ggplot(df, aes(Year)) +
  # Five-year average line
  geom_segment(
    data = df_mean_5yr,
    aes(
      x = 2020, xend = 2025,
      y = mean_production, yend = mean_production,
      color = "Five-year average (2020:2024)"
    ),
    size = 0.75
  ) +
  
  # Cereals production: past (solid)
  geom_line(
    data = subset(df, Year < CurrentYear),
    aes(y = Cereals_Production, color = "Total cereals production"),
    size = 1.5, linetype = "solid"
  ) +
  
  # Cereals production: recent/projection (dashed)
  geom_line(
    data = subset(df, Year > CurrentYear - 2),
    aes(y = Cereals_Production, color = "Total cereals production"),
    size = 1.5, linetype = "11"
  ) +
  
  # Current year point (excluded from legend)
  geom_point(
    data = subset(df, Year == CurrentYear),
    aes(y = Cereals_Production, color = "Total cereals production"),
    size = 5,
    show.legend = FALSE
  ) +
  
  # Current year label (excluded from legend)
  geom_text(
    data = subset(df, Year == CurrentYear),
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


####################################################################################################
# Alex - Barley Crop

df <- ch_data %>%
  select(c(Year, contains("Barley_Production"))) %>%
  filter(Year > (CurrentYear-10)) %>%
  as.data.frame()

# Compute 5-year averages (2020–2024) for both types
df_mean_5yr <- df %>%
  filter(Year >= 2020 & Year <= 2024) %>%
  summarise(
    S_Barley_Production = mean(S_Barley_Production, na.rm = TRUE),
    W_Barley_Production = mean(W_Barley_Production, na.rm = TRUE)
  )

Crop <- ggplot(df, aes(Year)) +
  # --- 5-year average line: Spring barley ---
  geom_segment(
    data = df_mean_5yr,
    aes(
      x = 2020, xend = 2025,
      y = S_Barley_Production, yend = S_Barley_Production
    ),
    color = "#575756", linetype = "solid", size = 0.75
  ) +
  annotate(
    "text",
    x = 2022, y = df_mean_5yr$S_Barley_Production + 85000,
    label = "Five-year average",
    size = 6, color = "#575756"
  ) +
  
  # --- 5-year average line: Winter barley ---
  geom_segment(
    data = df_mean_5yr,
    aes(
      x = 2020, xend = 2025,
      y = W_Barley_Production, yend = W_Barley_Production
    ),
    color = "#575756", linetype = "solid", size = 0.75
  ) +
  annotate(
    "text",
    x = 2022.5, y = df_mean_5yr$W_Barley_Production - 60000,
    label = "Five-year average",
    size = 6, color = "#575756"
  ) +
  scale_y_continuous(
    labels = scales::label_comma(scale = 1 / 1000, prefix = "", suffix = "", accuracy = 1, big.mark = ","), limits = c(0, 2000000), breaks = c(0, 500000, 1000000, 1500000, 2000000)
  ) +
  scale_x_continuous(
    limits = xlimits, breaks = xbreaks, labels = xaxislabels
  ) +
  geom_line(data=subset(df, Year<CurrentYear),
            aes(y = S_Barley_Production),
            color = "#00833E", size = 1.75, linetype = "solid"
  ) +
  geom_line(data=subset(df, Year>CurrentYear-2),
            aes(y = S_Barley_Production),
            color = "#00833E", size = 1.75, linetype = '11'
  ) +
  geom_point(data=subset(df, Year==CurrentYear),
             aes(y = S_Barley_Production),
             color = "#00833E", size = 5
  )+
  annotate(
    "text",
    x = CurrentYear, y = df$S_Barley_Production[df$Year==CurrentYear]+100000, 
    label = format((round(df$S_Barley_Production[df$Year==CurrentYear],-3)/1000), big.mark=","), 
    size = 6, color = "#00833E"
  )+
  annotate(
    "text",
    x = CurrentYear-7.5, y = 1650000, label = "Spring barley production", size = 6, color = "#00833E"
  ) +
  geom_line(data=subset(df, Year<CurrentYear),
            aes(y = W_Barley_Production),
            color = "#00833E", size = 1.75, linetype = "solid"
  ) +  
  geom_line(data=subset(df, Year>CurrentYear-2),
            aes(y = W_Barley_Production),
            color = "#00833E", size = 1.75, linetype = '11'
  ) +
  geom_point(data=subset(df, Year==CurrentYear),
             aes(y = W_Barley_Production),
             color = "#00833E", size = 5
  )+
  annotate(
    "text",
    x = CurrentYear, y = df$W_Barley_Production[df$Year==CurrentYear]+100000, 
    label = paste0(round(df$W_Barley_Production[df$Year==CurrentYear],-3)/1000), 
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
    legend.position = "none"
  )

Crop

ggsave(filename = paste0("CH_",CurrentYear,"_barley_production.svg"), plot = Crop, width = SVGWidth, height = SVGHeight, units = "mm", dpi = 320, bg = "white")


####################################################################################################
# Alex - Oats Crop

df <- ch_data %>%
  select(c(Year, contains("Oats_Production"))) %>%
  filter(Year > (CurrentYear-10)) %>%
  as.data.frame()

df_mean_5yr <- df %>%
  filter(Year >= 2020 & Year <= 2024) %>%
  summarise(Oats_Production = mean(Oats_Production, na.rm = TRUE))

Crop <- ggplot(df, aes(Year)) +
  # 5-year average line (2020–2024)
  geom_segment(
    data = df_mean_5yr,
    aes(
      x = 2020, xend = 2025,
      y = Oats_Production, yend = Oats_Production
    ),
    color = "#575756", linetype = "solid", size = 0.75
  ) +
  annotate(
    "text",
    x = 2022.5, y = df_mean_5yr$Oats_Production + 12000,
    label = "Five-year average",
    size = 6, color = "#575756"
  ) +
  scale_y_continuous(
    labels = scales::label_comma(scale = 1 / 1000, prefix = "", suffix = "", accuracy = 1, big.mark = ","), limits = c(0, 250000), breaks = c(0, 50000, 100000, 150000, 200000, 250000, 300000)
  ) +
  scale_x_continuous(
    limits = xlimits, breaks = xbreaks, labels = xaxislabels
  ) +
  geom_line(data=subset(df, Year < CurrentYear),
            aes(y = Oats_Production),
            color = "#00833E", size = 1.75, linetype = "solid"
  ) +
  geom_line(data=subset(df, Year>CurrentYear-2),
            aes(y = Oats_Production),
            color = "#00833E", size = 1.75, linetype = '11'
  ) +
  geom_point(data=subset(df, Year==CurrentYear),
             aes(y = Oats_Production),
             color = "#00833E", size = 5
  )+
  annotate(
    "text",
    x = CurrentYear, y = df$Oats_Production[df$Year==CurrentYear]+20000, 
    label = format((round(df$Oats_Production[df$Year==CurrentYear],-3)/1000), big.mark=","), 
    size = 6, color = "#00833E"
  )+
  annotate(
    "text",
    x = CurrentYear-7.5, y = 155000, label = "Oats production", size = 6, color = "#00833E"
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
    legend.position = "none"
  )

Crop

ggsave(filename = paste0("CH_",CurrentYear,"_oats_production.svg"), Crop, width = SVGWidth, height = SVGHeight, units = "mm", dpi = "retina", bg = "white")

####################################################################################################
# Alex - Wheat Crop

df <- ch_data %>%
  select(c(Year, contains("Wheat_Production"))) %>%
  filter(Year > CurrentYear-10) %>%
  as.data.frame()

df_mean_5yr <- df %>%
  filter(Year >= 2020 & Year <= 2024) %>%
  summarise(Wheat_Production = mean(Wheat_Production, na.rm = TRUE))

Crop <- ggplot(df, aes(Year)) +
  # 5-year average line (2020–2024)
  geom_segment(
    data = df_mean_5yr,
    aes(
      x = 2020, xend = 2025,
      y = Wheat_Production, yend = Wheat_Production
    ),
    color = "#575756", linetype = "solid", size = 0.75
  ) +
  annotate(
    "text",
    x = 2022.4, y = df_mean_5yr$Wheat_Production -60000,
    label = "Five-year average",
    size = 6, color = "#575756"
  ) +
  scale_y_continuous(
    labels = scales::label_comma(scale = 1 / 1000, prefix = "", suffix = "", accuracy = 1, big.mark = ","), limits = c(0, 1250000), breaks = c(0, 250000, 500000, 750000, 1000000, 1250000)
  ) +
  scale_x_continuous(
    limits = xlimits, breaks = xbreaks, labels = xaxislabels
  ) +
  geom_line(data=subset(df, Year < CurrentYear),
            aes(y = Wheat_Production), 
            color = "#00833E", size = 1.75, linetype = "solid",
  ) +
  geom_line(data=subset(df, Year>CurrentYear-2),
            aes(y = Wheat_Production),
            color = "#00833E", size = 1.75, linetype = '11'
  ) +
  geom_point(data=subset(df, Year==CurrentYear),
             aes(y = Wheat_Production),
             color = "#00833E", size = 5
  )+
  annotate(
    "text",
    x = CurrentYear, y = df$Wheat_Production[df$Year==CurrentYear]+70000, 
    label = format((round(df$Wheat_Production[df$Year==CurrentYear],-3)/1000), big.mark=","), 
    size = 6, color = "#00833E"
  )+
  annotate(
    "text",
    x = CurrentYear-7.5, y = 970000, label = "Wheat production", size = 6, color = "#00833E"
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
    legend.position = "none"
  )

Crop

ggsave(filename = paste0("CH_",CurrentYear,"_wheat_production.svg"), plot = Crop, width = SVGWidth, height = SVGHeight, units = "mm", dpi = "retina", bg = "white")


####################################################################################################
# Alex - OSR Crop

df <- ch_data %>%
  select(c(Year, contains("OSR_Production"))) %>%
  filter(Year > CurrentYear-10) %>%
  as.data.frame()

df_mean_5yr <- df %>%
  filter(Year >= 2020 & Year <= 2024) %>%
  summarise(OSR_Production = mean(OSR_Production, na.rm = TRUE))

Crop <- ggplot(df, aes(Year)) +
  # 5-year average line (2020–2024)
  geom_segment(
    data = df_mean_5yr,
    aes(
      x = 2020, xend = 2025,
      y = OSR_Production, yend = OSR_Production
    ),
    color = "#575756", linetype = "solid", size = 0.75
  ) +
  annotate(
    "text",
    x = 2022.75, y = df_mean_5yr$OSR_Production -10000,
    label = "Five-year average (2020:2024)",
    size = 6, color = "#575756"
  ) +
  scale_y_continuous(
    labels = scales::label_comma(scale = 1 / 1000, prefix = "", suffix = "", accuracy = 1, big.mark = ","), limits = c(0, 200000)
  ) +
  scale_x_continuous(
    limits = xlimits , breaks = xbreaks, labels = xaxislabels
  ) +
  geom_line(data=subset(df, Year < CurrentYear),
            aes(y = OSR_Production),
            color = "#00833E", size = 1.75, linetype = "solid"
  ) +
  geom_line(data=subset(df, Year>CurrentYear-2),
            aes(y = OSR_Production),
            color = "#00833E", size = 1.75, linetype = '11'
  ) +
  geom_point(data=subset(df, Year==CurrentYear),
             aes(y = OSR_Production),
             color = "#00833E", size = 5
  )+
  annotate(
    "text",
    x = CurrentYear, y = df$OSR_Production[df$Year==CurrentYear]+12000, 
    label = format((round(df$OSR_Production[df$Year==CurrentYear],-3)/1000), big.mark=","), 
    size = 6, color = "#00833E"
  )+
  annotate(
    "text",
    x = CurrentYear-7.5, y = 155000, label = "Oilseed rape production", size = 6, color = "#00833E"
  ) +
  labs(
    title = "", y = "Thousand tonnes", x = "Year"
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
    legend.position = "none"
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