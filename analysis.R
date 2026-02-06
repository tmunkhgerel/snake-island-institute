# PROJECT: Russian Equipment Loss Analysis (Tanks Vs. Drones)
# AUTHOR: Temuulen Ivanov-Munkhgerel
# DATE: December, 2025
# PURPOSE: Cleans raw daily cumulative data into monthly aggregated differentials for trend visualisation.

library(tidyverse)
library(lubridate)

# 1. DATA IMPORT
df <- read_csv('russia_losses_equipment.csv')

# 2. DATA PROCESSING
df_clean <- df %>%
    mutate(date = as.Date(date)) %>%
    arrange(date) %>%
    mutate(
        daily_tank = tank - lag(tank, default = 0),
        daily_drone = drone - lag(drone, default = 0)
    )

# 3. AGGREGATING BY MONTH
monthly_losses <- df_clean %>%
    group_by(month = floor_date(date, "month")) %>%
    summarise(
        total_tanks = sum(daily_tank),
        total_drones = sum(daily_drone)
    )

# 4. VISUALISATION

scale_factor <- max(monthly_losses$total_tanks, na.rm = TRUE) / 
                max(monthly_losses$total_drones, na.rm = TRUE)

ggplot(monthly_losses, aes(x = month)) +
  # LINES
  geom_line(aes(y = total_tanks, color = "Tanks"), linewidth = 1.2) +
  geom_line(aes(y = total_drones * scale_factor, color = "Drones"), linewidth = 1.2) +
  
  # DUAL-AXIS
  scale_y_continuous(
    name = "Monthly Tank Losses",
    sec.axis = sec_axis(~./scale_factor, name = "Monthly Drone Losses")
  ) +
  
  # COLORS
  scale_color_manual(values = c("Tanks" = "#e31a1c", "Drones" = "#1f78b4")) +
  
  # THEME & FORMATTING
  theme_minimal() +
  labs(
    title = "Equipment Loss Trends: Tanks vs. Drones",
    subtitle = "calculated from monthly aggregated daily differentials",
    x = "Timeline",
    color = NULL
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "bottom",
    axis.title.y.left = element_text(color = "#e31a1c", face = "bold"),
    axis.title.y.right = element_text(color = "#1f78b4", face = "bold"),
    panel.grid.minor = element_blank()
  )

# 5. SAVING THE FINAL VERSION
ggsave("equipment_losses_centered.png", width = 12, height = 7, dpi = 300)