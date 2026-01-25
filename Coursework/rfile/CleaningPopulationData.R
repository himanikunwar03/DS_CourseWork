library(tidyverse)

base_path <- ".\\Coursework"

# =============================================================================
# SECTION 1: LOAD AND CLEAN POPULATION DATA
# =============================================================================

# Load raw population data
population_raw <- read_csv(
  file.path(base_path, "ObtainedData/Population2011_1656567141570.csv"),
  col_types = cols(
    Postcode = col_character(),
    Population = col_character()
  )
)

cat("Raw data loaded:", nrow(population_raw), "records\n")

# Clean population data
population_clean <- population_raw %>%
  mutate(
    # Remove spaces from postcode
    postcode_clean = str_replace_all(Postcode, " ", "") %>% str_to_upper(),
    
    # Extract postcode components
    postcode_area = str_extract(postcode_clean, "^[A-Z]+"),
    postcode_district = str_extract(postcode_clean, "^[A-Z]+[0-9]+"),
    postcode_sector = str_extract(postcode_clean, "^[A-Z]+[0-9]+\\s*[0-9]"),
    
    # Clean population number (remove commas)
    population = as.numeric(str_replace_all(Population, ",", "")),
    
    # Assign region based on postcode area
    region = case_when(
      postcode_area == "CH" ~ "Cheshire",      # Chester area
      postcode_area == "CW" ~ "Cheshire",      # Crewe/Winsford area
      postcode_area == "WA" ~ "Cheshire",      # Warrington area (partial Cheshire)
      postcode_area == "SK" ~ "Cheshire",      # Stockport area (partial Cheshire)
      postcode_area == "CA" ~ "Cumberland",    # Carlisle/Cumbria area
      TRUE ~ "Other"
    )
  ) %>%
  filter(
    !is.na(population),
    population > 0,
    region != "Other"
  )

cat("Cleaned data:", nrow(population_clean), "records\n")

# =============================================================================
# SECTION 2: CREATE SUMMARY TABLES
# =============================================================================

# --- 2.1 Population by Region ---
population_by_region <- population_clean %>%
  group_by(region) %>%
  summarise(
    total_population = sum(population, na.rm = TRUE),
    num_postcode_sectors = n_distinct(postcode_district),
    avg_population_per_sector = round(mean(population, na.rm = TRUE), 0),
    median_population_per_sector = median(population, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n=== Population by Region ===\n")
print(population_by_region)

# --- 2.2 Population by Postcode District ---
population_by_district <- population_clean %>%
  group_by(postcode_district, region) %>%
  summarise(
    total_population = sum(population, na.rm = TRUE),
    num_sectors = n(),
    .groups = "drop"
  ) %>%
  arrange(region, desc(total_population))

cat("\n=== Top 10 Most Populous Districts ===\n")
print(head(population_by_district, 10))

# --- 2.3 Population by Postcode Area ---
population_by_area <- population_clean %>%
  group_by(postcode_area, region) %>%
  summarise(
    total_population = sum(population, na.rm = TRUE),
    num_districts = n_distinct(postcode_district),
    num_sectors = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(total_population))

cat("\n=== Population by Postcode Area ===\n")
print(population_by_area)

# =============================================================================
# SECTION 3: SAVE CLEANED DATA
# =============================================================================

# Save cleaned population data
write_csv(population_clean, file.path(base_path, "cleanedData/population_cleaned.csv"))
write_csv(population_by_region, file.path(base_path, "cleanedData/population_by_region.csv"))
write_csv(population_by_district, file.path(base_path, "cleanedData/population_by_district.csv"))
write_csv(population_by_area, file.path(base_path, "cleanedData/population_by_area.csv"))

# =============================================================================
# SECTION 4: VISUALIZATION
# =============================================================================

library(ggplot2)
library(scales)

# --- 4.1 Population Distribution by Region ---
png(file.path(base_path, "Graphs/population_by_region.png"), 
    width = 800, height = 600, res = 120)

ggplot(population_by_region, aes(x = region, y = total_population, fill = region)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = format(total_population, big.mark = ",")), vjust = -0.5, size = 5) +
  labs(
    title = "Total Population by Region",
    subtitle = "Based on 2011 Census Data by Postcode Sector",
    x = "Region",
    y = "Total Population",
    fill = "Region"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    legend.position = "none"
  ) +
  scale_y_continuous(labels = label_comma(), limits = c(0, max(population_by_region$total_population) * 1.15)) +
  scale_fill_manual(values = c("Cheshire" = "#3498db", "Cumberland" = "#e74c3c"))

dev.off()

# --- 4.2 Population Distribution by Postcode Area ---
png(file.path(base_path, "Graphs/population_by_postcode_area.png"), 
    width = 1000, height = 600, res = 120)

ggplot(population_by_area, aes(x = reorder(postcode_area, total_population), 
                               y = total_population, fill = region)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = format(total_population, big.mark = ",")), hjust = -0.1, size = 3.5) +
  coord_flip() +
  labs(
    title = "Population by Postcode Area",
    subtitle = "CH=Chester, CW=Crewe, WA=Warrington, SK=Stockport, CA=Carlisle",
    x = "Postcode Area",
    y = "Total Population",
    fill = "Region"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold")
  ) +
  scale_x_discrete() +
  scale_y_continuous(labels = label_comma(), limits = c(0, max(population_by_area$total_population) * 1.2)) +
  scale_fill_manual(values = c("Cheshire" = "#3498db", "Cumberland" = "#e74c3c"))

dev.off()

# --- 4.3 Top 15 Most Populous Districts ---
top_15_districts <- head(population_by_district, 15)

png(file.path(base_path, "Graphs/population_top15_districts.png"), 
    width = 1000, height = 700, res = 120)

ggplot(top_15_districts, aes(x = reorder(postcode_district, total_population), 
                             y = total_population, fill = region)) +
  geom_bar(stat = "identity", width = 0.7) +
  coord_flip() +
  labs(
    title = "Top 15 Most Populous Postcode Districts",
    subtitle = "Population by District (2011 Census)",
    x = "Postcode District",
    y = "Total Population",
    fill = "Region"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold")
  ) +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_manual(values = c("Cheshire" = "#3498db", "Cumberland" = "#e74c3c"))

dev.off()

cat("\n========================================\n")
cat("POPULATION DATA CLEANING COMPLETE\n")
cat("========================================\n")
cat("Files saved:\n")
cat("  - population_cleaned.csv\n")
cat("  - population_by_region.csv\n")
cat("  - population_by_district.csv\n")
cat("  - population_by_area.csv\n")
cat("\nVisualizations saved to Graphs folder\n")