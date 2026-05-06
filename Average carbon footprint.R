# ─── Load Libraries ───────────────────────────────────────────────────────────
library(ggplot2)
library(dplyr)
library(readr)

# ─── Create the Dataset ───────────────────────────────────────────────────────
food_data <- data.frame(
  food = c("Beef", "Lamb", "Cheese", "Pork", "Poultry", "Eggs",
           "Rice", "Milk", "Tofu", "Tomatoes", "Oats", "Lentils",
           "Nuts", "Root Vegetables", "Wheat Bread", "Bananas"),
  co2_per_kg = c(60.0, 24.0, 13.5, 7.6, 6.9, 4.5,
                 4.0, 3.2, 3.0, 2.1, 1.6, 0.9,
                 0.3, 0.4, 1.4, 0.7),
  category = c("Meat", "Meat", "Dairy", "Meat", "Meat", "Dairy",
               "Grain", "Dairy", "Plant", "Plant", "Grain", "Plant",
               "Plant", "Plant", "Grain", "Plant")
)

# ─── Analysis ─────────────────────────────────────────────────────────────────
# Sort from highest to lowest carbon footprint
food_data <- food_data %>% arrange(desc(co2_per_kg))

# Summary by category
category_summary <- food_data %>%
  group_by(category) %>%
  summarise(avg_co2 = mean(co2_per_kg)) %>%
  arrange(desc(avg_co2))

# Print results to console
cat("=== TOP 5 HIGHEST CARBON FOODS ===\n")
print(head(food_data[, c("food", "co2_per_kg")], 5))

cat("\n=== AVERAGE CO2 BY FOOD CATEGORY ===\n")
print(category_summary)

# ─── Chart 1: Carbon Footprint by Food ────────────────────────────────────────
ggplot(food_data, aes(x = reorder(food, co2_per_kg), y = co2_per_kg, fill = category)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Carbon Footprint of Common Foods",
    subtitle = "kg of CO2 equivalent per kg of food produced",
    x = "Food Item",
    y = "CO2 Emissions (kg)",
    fill = "Category"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c(
    "Meat"  = "#e74c3c",
    "Dairy" = "#f39c12",
    "Grain" = "#f1c40f",
    "Plant" = "#2ecc71"
  ))

# ─── Chart 2: Average CO2 by Category ─────────────────────────────────────────
ggplot(category_summary, aes(x = reorder(category, avg_co2), y = avg_co2, fill = category)) +
  geom_bar(stat = "identity", width = 0.5) +
  coord_flip() +
  labs(
    title = "Average Carbon Footprint by Food Category",
    subtitle = "Which food groups are most environmentally costly?",
    x = "Category",
    y = "Average CO2 (kg per kg of food)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c(
    "Meat"  = "#e74c3c",
    "Dairy" = "#f39c12",
    "Grain" = "#f1c40f",
    "Plant" = "#2ecc71"
  )) +
  theme(legend.position = "none")
