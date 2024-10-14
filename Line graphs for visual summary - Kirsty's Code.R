library(ggplot2)
library(tidyverse)
library(readxl)
library(scales)
resas_theme <- source("resas_theme_modified.R")

CurrentYear = 2024
YearRange <- c((CurrentYear-9):CurrentYear)
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

ch_data <- read.csv("CH_data.csv")

ch_data <- ch_data %>%
  mutate(Barley_Production = W_Barley_Production + S_Barley_Production) %>%
  select(-W_Barley_Production, -S_Barley_Production) %>%
  mutate(Barley_Area = W_Barley_Area + S_Barley_Area) %>%
  select(-W_Barley_Area, -S_Barley_Area) %>%
  select(-W_Barley_Yield, -S_Barley_Yield) %>%
  select(-OSR_Production, -OSR_Area, -OSR_Yield)

### Production Graph ###

# Select relevant columns and rename them
all_P <- ch_data %>% 
  select(Year, contains("Production")) %>%
  rename_with(~ gsub("_Production", "", .x))

# Pivot data to long format and factorize 'Crop'
all_P <- all_P %>%
  pivot_longer(-Year, names_to = "Crop", values_to = "Production") %>%
  mutate(Crop = factor(Crop, levels = c("Cereals", "Barley", "Wheat", "Oats")))

# Filter data for the past 10 years and separate Cereals data
all_prod <- filter(all_P, Year > (CurrentYear - 10) & Crop != "Cereals")
prod_cereals <- filter(all_P, Year > (CurrentYear - 10) & Crop == "Cereals")

# Base plot
prod <- ggplot(all_prod, aes(x = Year, y = Production, color = Crop)) +
  geom_line(data = filter(all_P, Year < CurrentYear & Year > (CurrentYear - 10)), lwd = 1.5) +
  geom_line(data = filter(all_P, Year > CurrentYear - 2 & Crop != "Cereals"), lwd = 1.5, linetype = '11') +
  geom_line(data = filter(prod_cereals, Year < CurrentYear), aes(x = Year, y = Production), lwd = 2.5) +
  geom_line(data = filter(prod_cereals, Year > (CurrentYear - 2)), aes(x = Year, y = Production), linetype = '11', lwd = 2.5) +
  
  # Add points for each Crop at each Year
  geom_point(data = all_prod, aes(shape = Crop), size = 4) +  # Different shapes for crops
  geom_point(data = prod_cereals, aes(x = Year, y = Production), shape = 15, size = 4, color = "#28a197") +  # Custom symbol for total Cereals
  
  # Axis labels and title
  labs(title = "", y = "Thousand tonnes", x = "Year") +
  
  # Y-axis scaling and limits
  scale_y_continuous(labels = scales::label_comma(scale = 0.001, accuracy = 1), 
                     limits = c(0, 3500000), breaks = c(0, 500000, 1500000, 2500000, 3500000)) +
  
  # X-axis scaling and limits
  scale_x_continuous(limits = c(CurrentYear - 9, CurrentYear), 
                     breaks = (CurrentYear - 9):CurrentYear, labels = xaxislabels) +
  
  # Manual color scale
  scale_color_manual(values = c("#28a197", "#0065bd", "#5eb135", "#0e450b")) +
  
  # Manual shape scale for different crops
  scale_shape_manual(values = c(16, 17, 18, 15)) +  # Customize shapes as needed
  
  # Custom theme
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.text = element_text(size = 16),
    axis.title.x = element_text(size = 17),
    axis.title.y = element_text(angle = 90, size = 17),
    strip.text = element_text(size = 16),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

prod

# Save the plot
ggsave(prod, filename = "all_prod.svg", width = 159, height = 150, units = "mm", dpi = "retina", bg = "white")

################################################################################
### Area Graph ###
# Select relevant columns and rename them
all_A <- ch_data %>% 
  select(Year, contains("Area")) %>%
  rename_with(~ gsub("_Area", "", .x))

# Pivot data to long format and factorize 'Crop'
all_A <- all_A %>%
  pivot_longer(-Year, names_to = "Crop", values_to = "Area") %>%
  mutate(Crop = factor(Crop, levels = c("Cereals", "Barley", "Wheat", "Oats")))

# Filter data for the past 10 years and separate Cereals data
all_area <- filter(all_A, Year > (CurrentYear - 10) & Crop != "Cereals")
area_cereals <- filter(all_A, Year > (CurrentYear - 10) & Crop == "Cereals")

# Base plot
area <- ggplot(all_area, aes(x = Year, y = Area, color = Crop)) +
  geom_line(data = filter(all_A, Year < CurrentYear & Year > (CurrentYear - 10)), lwd = 1.5) +
  geom_line(data = filter(all_A, Year > CurrentYear - 2 & Crop != "Cereals"), lwd = 1.5, linetype = '11') +
  geom_line(data = filter(area_cereals, Year < CurrentYear), aes(x = Year, y = Area), lwd = 2.5) +
  geom_line(data = filter(area_cereals, Year > (CurrentYear - 2)), aes(x = Year, y = Area), linetype = '11', lwd = 2.5) +
  
  # Add points for each Crop at each Year
  geom_point(data = all_area, aes(shape = Crop), size = 4) +  # Different shapes for crops
  geom_point(data = area_cereals, aes(x = Year, y = Area), shape = 15, size = 4, color = "#28a197") +  # Custom symbol for total Cereals
  
  # Axis labels and title
  labs(title = "", y = "Hectares", x = "Year") +
  
  # Y-axis scaling and limits
  scale_y_continuous(labels = scales::label_comma(accuracy = 1), 
                     limits = c(0, 500000), breaks = c(0, 100000, 200000, 300000, 400000, 500000)) +
  
  # X-axis scaling and limits
  scale_x_continuous(limits = c(CurrentYear - 9, CurrentYear), 
                     breaks = (CurrentYear - 9):CurrentYear, labels = xaxislabels) +
  
  # Manual color scale
  scale_color_manual(values = c("#28a197", "#0065bd", "#5eb135", "#0e450b")) +
  
  # Manual shape scale for different crops
  scale_shape_manual(values = c(16, 17, 18, 15)) +  # Customize shapes as needed
  
  # Custom theme
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.text = element_text(size = 16),
    axis.title.x = element_text(size = 17),
    axis.title.y = element_text(angle = 90, size = 17),
    strip.text = element_text(size = 16),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

area

# Save the plot
ggsave(area, filename = "all_area.svg", width = 159, height = 150, units = "mm", dpi = "retina", bg = "white")


#################################################################################
### Yield Graph ###
# Select relevant columns and rename them
all_Y <- ch_data %>% 
  select(Year, contains("Yield")) %>%
  rename_with(~ gsub("_Yield", "", .x))

# Pivot data to long format and factorize 'Crop'
all_Y <- all_Y %>%
  pivot_longer(-Year, names_to = "Crop", values_to = "Yield") %>%
  mutate(Crop = factor(Crop, levels = c("Cereals", "Barley", "Wheat", "Oats")))

# Filter data for the past 10 years and separate Cereals data
all_yield <- filter(all_Y, Year > (CurrentYear - 10) & Crop != "Cereals")
yield_cereals <- filter(all_Y, Year > (CurrentYear - 10) & Crop == "Cereals")

# Base plot
yield <- ggplot(all_yield, aes(x = Year, y = Yield, color = Crop)) +
  geom_line(data = filter(all_Y, Year < CurrentYear & Year > (CurrentYear - 10)), lwd = 1.5) +
  geom_line(data = filter(all_Y, Year > CurrentYear - 2 & Crop != "Cereals"), lwd = 1.5, linetype = '11') +
  geom_line(data = filter(yield_cereals, Year < CurrentYear), aes(x = Year, y = Yield), lwd = 2.5) +
  geom_line(data = filter(yield_cereals, Year > (CurrentYear - 2)), aes(x = Year, y = Yield), linetype = '11', lwd = 2.5) +
  
  # Add points for each Crop at each Year
  geom_point(data = all_yield, aes(shape = Crop), size = 4) +  # Different shapes for crops
  geom_point(data = yield_cereals, aes(x = Year, y = Yield), shape = 15, size = 4, color = "#28a197") +  # Custom symbol for total Cereals
  
  # Axis labels and title
  labs(title = "", y = "Tonnes per Hectares", x = "Year") +
  
  # Y-axis scaling and limits
  scale_y_continuous(labels = scales::label_comma(accuracy = 1), 
                     limits = c(0, 10), breaks = c(0, 2.5, 5.0, 7.5, 10.0)) +
  
  # X-axis scaling and limits
  scale_x_continuous(limits = c(CurrentYear - 9, CurrentYear), 
                     breaks = (CurrentYear - 9):CurrentYear, labels = xaxislabels) +
  
  # Manual color scale
  scale_color_manual(values = c("#28a197", "#0065bd", "#5eb135", "#0e450b")) +
  
  # Manual shape scale for different crops
  scale_shape_manual(values = c(16, 17, 18, 15)) +  # Customize shapes as needed
  
  # Custom theme
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.text = element_text(size = 16),
    axis.title.x = element_text(size = 17),
    axis.title.y = element_text(angle = 90, size = 17),
    strip.text = element_text(size = 16),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

yield

# Save the plot
ggsave(yield, filename = "all_yield.svg", width = 159, height = 150, units = "mm", dpi = "retina", bg = "white")
