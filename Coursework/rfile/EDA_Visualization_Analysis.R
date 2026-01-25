# Load required libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(gridExtra)

# Set working directory path
base_path <- ".\\Coursework"

# =============================================================================
# SECTION 1: DATA LOADING AND PREPROCESSING
# =============================================================================

# --- 1.1 Load House Price Data ---
house_prices_2022 <- read_csv(file.path(base_path, "cleanedData/cleaned_house_prices_2022.csv"))
house_prices_203  <- read_csv(file.path(base_path, "cleanedData/cleaned_house_prices_2023.csv"))
house_prices_2024 <- read_csv(file.path(base_path, "cleanedData/cleaned_house_prices_2024.csv"))

# Add year column
house_prices_2022 <- house_prices_2022 %>% mutate(year = 2022)
house_prices_2023 <- house_prices_2023 %>% mutate(year = 2023)
house_prices_2024 <- house_prices_2024 %>% mutate(year = 2024)

# Combine all house price data
all_house_prices <- bind_rows(house_prices_2022, house_prices_2023, house_prices_2024)

# Standardize county names to assign to Cheshire or Cumberland
all_house_prices <- all_house_prices %>%
  mutate(
    region = case_when(
      county %in% c("Cheshire", "Cheshire East", "Cheshire West And Chester") ~ "Cheshire",
      county == "Cumberland" ~ "Cumberland",
      TRUE ~ "Other"
    )
  ) %>%
  filter(region != "Other")

# --- 1.2 Load Broadband Performance Data ---
broadband_performance <- read_csv(file.path(base_path, "cleanedData/broadbandSpeed_performance.csv"))

# Add postcode area for classification (first letters of postcode)
broadband_performance <- broadband_performance %>%
  mutate(
    postcode_area = str_extract(postcode, "^[A-Z]+"),
    region = case_when(
      postcode_area %in% c("CH", "CW", "WA", "SK") ~ "Cheshire",
      postcode_area == "CA" ~ "Cumberland",
      TRUE ~ "Unknown"
    )
  )

# --- 1.3 Load Population Data ---
population_data <- read_csv(
  file.path(base_path, "ObtainedData/Population2011_1656567141570.csv"),
  col_types = cols(
    Postcode = col_character(),
    Population = col_character()
  )
)

# Clean population data
population_data <- population_data %>%
  mutate(
    Postcode = str_replace_all(Postcode, " ", ""),
    Population = as.numeric(str_replace_all(Population, ",", ""))
  ) %>%
  filter(!is.na(Population))

# Extract postcode area for region mapping
population_data <- population_data %>%
  mutate(
    postcode_area = str_extract(Postcode, "^[A-Z]+"),
    region = case_when(
      postcode_area %in% c("CH", "CW", "WA", "SK") ~ "Cheshire",
      postcode_area == "CA" ~ "Cumberland",
      TRUE ~ "Other"
    )
  ) %>%
  filter(region != "Other")

# --- 1.4 Load and Process Crime Data ---
load_crime_data <- function(base_path) {
  crime_folder <- file.path(base_path, "ObtainedData/crimerate")
  month_folders <- list.dirs(crime_folder, recursive = FALSE)
  all_crime_data <- list()
  
  for (folder in month_folders) {
    csv_files <- list.files(folder, pattern = "\\.csv$", full.names = TRUE)
    for (file in csv_files) {
      tryCatch({
        crime_df <- read_csv(file, show_col_types = FALSE)
        if (grepl("cheshire", file, ignore.case = TRUE)) {
          crime_df$region <- "Cheshire"
        } else if (grepl("cumbria", file, ignore.case = TRUE)) {
          crime_df$region <- "Cumberland"
        }
        all_crime_data[[length(all_crime_data) + 1]] <- crime_df
      }, error = function(e) {})
    }
  }
  bind_rows(all_crime_data)
}

crime_data <- load_crime_data(base_path)
crime_data <- crime_data %>%
  mutate(
    year = as.numeric(str_sub(Month, 1, 4)),
    month_num = as.numeric(str_sub(Month, 6, 7)),
    month_year = Month
  ) %>%
  filter(!is.na(`Crime type`))

cat("Data loaded successfully!\n")

# =============================================================================
# SECTION 2: HOUSE PRICES EDA AND VISUALIZATION
# =============================================================================

# --- 2.1 Calculate Average House Prices by Town ---
avg_house_prices_by_town <- all_house_prices %>%
  group_by(town, region, year) %>%
  summarise(
    avg_price = mean(price, na.rm = TRUE),
    median_price = median(price, na.rm = TRUE),
    num_sales = n(),
    .groups = "drop"
  ) %>%
  filter(num_sales >= 5)

# --- 2.2 BOXPLOT: Average House Prices for Year 2023 by Town ---
house_prices_2023_summary <- avg_house_prices_by_town %>%
  filter(year == 2023)

top_towns_2023 <- house_prices_2023_summary %>%
  group_by(region) %>%
  top_n(8, num_sales) %>%
  ungroup()

boxplot_data_2023 <- all_house_prices %>%
  filter(year == 2023, town %in% top_towns_2023$town)

p1 <- ggplot(boxplot_data_2023, aes(x = reorder(town, price, FUN = median), y = price, fill = region)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1) +
  coord_flip() +
  scale_y_continuous(labels = label_comma(prefix = "£"), limits = c(0, 1000000)) +
  labs(
    title = "House Prices Distribution by Town (2023)",
    subtitle = "Comparing Cheshire and Cumberland Counties",
    x = "Town",
    y = "House Price (£)",
    fill = "Region"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.y = element_text(size = 10)
  ) +
  scale_fill_manual(values = c("Cheshire" = "#3498db", "Cumberland" = "#e74c3c"))

ggsave(file.path(base_path, "Graphs/boxplot_house_prices_2023.png"), 
       plot = p1, width = 12, height = 8, dpi = 120)
cat("Saved: boxplot_house_prices_2023.png\n")

# --- 2.3 BAR CHART: Average House Price for 2022 (Both Counties) ---
avg_prices_2022 <- avg_house_prices_by_town %>%
  filter(year == 2022) %>%
  group_by(region) %>%
  top_n(10, num_sales) %>%
  ungroup() %>%
  arrange(region, desc(avg_price))

p2 <- ggplot(avg_prices_2022, aes(x = reorder(town, avg_price), y = avg_price, fill = region)) +
  geom_bar(stat = "identity", width = 0.7) +
  coord_flip() +
  facet_wrap(~region, scales = "free_y") +
  scale_y_continuous(labels = label_comma(prefix = "£")) +
  labs(
    title = "Average House Prices by Town (2022)",
    subtitle = "Top Towns in Cheshire and Cumberland",
    x = "Town",
    y = "Average House Price (£)",
    fill = "Region"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    strip.text = element_text(size = 12, face = "bold")
  ) +
  scale_fill_manual(values = c("Cheshire" = "#3498db", "Cumberland" = "#e74c3c"))

ggsave(file.path(base_path, "Graphs/barchart_avg_house_prices_2022.png"), 
       plot = p2, width = 14, height = 8, dpi = 120)
cat("Saved: barchart_avg_house_prices_2022.png\n")

# --- 2.4 LINE GRAPH: Average House Prices 2022-2024 (Both Counties) ---
yearly_avg_prices <- all_house_prices %>%
  group_by(region, year) %>%
  summarise(
    avg_price = mean(price, na.rm = TRUE),
    median_price = median(price, na.rm = TRUE),
    .groups = "drop"
  )

p3 <- ggplot(yearly_avg_prices, aes(x = year, y = avg_price, color = region, group = region)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 4) +
  geom_text(aes(label = paste0("£", round(avg_price/1000, 0), "k")), 
            vjust = -1, size = 4) +
  scale_y_continuous(labels = label_comma(prefix = "£"), 
                     limits = c(0, max(yearly_avg_prices$avg_price) * 1.2)) +
  scale_x_continuous(breaks = c(2022, 2023, 2024)) +
  labs(
    title = "Average House Prices Trend (2022-2024)",
    subtitle = "Comparing Cheshire and Cumberland Counties",
    x = "Year",
    y = "Average House Price (£)",
    color = "Region"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    legend.position = "bottom"
  ) +
  scale_color_manual(values = c("Cheshire" = "#3498db", "Cumberland" = "#e74c3c"))

ggsave(file.path(base_path, "Graphs/linegraph_house_prices_2022_2024.png"), 
       plot = p3, width = 10, height = 6, dpi = 120)
cat("Saved: linegraph_house_prices_2022_2024.png\n")

# =============================================================================
# SECTION 3: BROADBAND SPEED EDA AND VISUALIZATION
# =============================================================================

# --- 3.1 BOXPLOT: Average Download Speeds by Region ---
broadband_by_region <- broadband_performance %>%
  filter(region %in% c("Cheshire", "Cumberland"))

p4 <- ggplot(broadband_by_region, aes(x = region, y = mean_avg_download, fill = region)) +
  geom_boxplot(outlier.shape = 21, outlier.alpha = 0.5) +
  labs(
    title = "Distribution of Average Download Speeds",
    subtitle = "Comparing Cheshire and Cumberland Counties",
    x = "Region",
    y = "Average Download Speed (Mbit/s)",
    fill = "Region"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    legend.position = "none"
  ) +
  scale_fill_manual(values = c("Cheshire" = "#3498db", "Cumberland" = "#e74c3c"))

ggsave(file.path(base_path, "Graphs/boxplot_broadband_speeds.png"), 
       plot = p4, width = 10, height = 6, dpi = 120)
cat("Saved: boxplot_broadband_speeds.png\n")

# --- 3.2 Create postcode district summary ---
broadband_by_district <- broadband_performance %>%
  mutate(postcode_district = str_extract(postcode, "^[A-Z]+[0-9]+")) %>%
  group_by(postcode_district, region) %>%
  summarise(
    avg_download = mean(mean_avg_download, na.rm = TRUE),
    max_download = mean(mean_max_download, na.rm = TRUE),
    avg_upload = mean(mean_avg_upload, na.rm = TRUE),
    num_postcodes = n(),
    .groups = "drop"
  ) %>%
  filter(region %in% c("Cheshire", "Cumberland"))

# --- 3.3 STACKED BAR CHART: Cheshire Broadband Speeds ---
cheshire_broadband <- broadband_by_district %>%
  filter(region == "Cheshire") %>%
  arrange(desc(avg_download)) %>%
  head(15) %>%
  pivot_longer(cols = c(avg_download, max_download), names_to = "speed_type", values_to = "speed")

p5 <- ggplot(cheshire_broadband, aes(x = reorder(postcode_district, speed), y = speed, fill = speed_type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  coord_flip() +
  labs(
    title = "Broadband Speeds in Cheshire by Postcode District",
    subtitle = "Average and Maximum Download Speeds",
    x = "Postcode District",
    y = "Speed (Mbit/s)",
    fill = "Speed Type"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold")) +
  scale_fill_manual(values = c("avg_download" = "#3498db", "max_download" = "#1a5276"),
                    labels = c("Average Download", "Max Download"))

ggsave(file.path(base_path, "Graphs/stacked_barchart_cheshire_broadband.png"), 
       plot = p5, width = 12, height = 7, dpi = 120)
cat("Saved: stacked_barchart_cheshire_broadband.png\n")

# --- 3.4 STACKED BAR CHART: Cumberland Broadband Speeds ---
cumberland_broadband <- broadband_by_district %>%
  filter(region == "Cumberland") %>%
  arrange(desc(avg_download)) %>%
  head(15) %>%
  pivot_longer(cols = c(avg_download, max_download), names_to = "speed_type", values_to = "speed")

p6 <- ggplot(cumberland_broadband, aes(x = reorder(postcode_district, speed), y = speed, fill = speed_type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  coord_flip() +
  labs(
    title = "Broadband Speeds in Cumberland by Postcode District",
    subtitle = "Average and Maximum Download Speeds",
    x = "Postcode District",
    y = "Speed (Mbit/s)",
    fill = "Speed Type"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold")) +
  scale_fill_manual(values = c("avg_download" = "#e74c3c", "max_download" = "#922b21"),
                    labels = c("Average Download", "Max Download"))

ggsave(file.path(base_path, "Graphs/stacked_barchart_cumberland_broadband.png"), 
       plot = p6, width = 12, height = 7, dpi = 120)
cat("Saved: stacked_barchart_cumberland_broadband.png\n")

# =============================================================================
# SECTION 4: CRIME RATE EDA AND VISUALIZATION
# =============================================================================

# --- 4.1 BOXPLOT: Drug Offense Rate by Region ---
drug_offenses <- crime_data %>%
  filter(`Crime type` == "Drugs") %>%
  group_by(region, `LSOA name`) %>%
  summarise(drug_count = n(), .groups = "drop")

p7 <- ggplot(drug_offenses, aes(x = region, y = drug_count, fill = region)) +
  geom_boxplot(outlier.shape = 21, outlier.alpha = 0.5) +
  labs(
    title = "Distribution of Drug Offenses by LSOA",
    subtitle = "Comparing Cheshire and Cumberland Counties",
    x = "Region",
    y = "Number of Drug Offenses",
    fill = "Region"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"), legend.position = "none") +
  scale_fill_manual(values = c("Cheshire" = "#3498db", "Cumberland" = "#e74c3c"))

ggsave(file.path(base_path, "Graphs/boxplot_drug_offenses.png"), 
       plot = p7, width = 10, height = 6, dpi = 120)
cat("Saved: boxplot_drug_offenses.png\n")

# --- 4.2 Crime Summary for 2023 ---
crime_2023 <- crime_data %>% filter(year == 2023)
crime_summary_2023 <- crime_2023 %>%
  group_by(region, `Crime type`) %>%
  summarise(count = n(), .groups = "drop")

# --- 4.3 BAR CHART: Crime Type Comparison 2023 ---
radar_data <- crime_summary_2023 %>%
  filter(`Crime type` %in% c("Vehicle crime", "Burglary", "Drugs", 
                             "Robbery", "Violence and sexual offences",
                             "Criminal damage and arson", "Other theft")) %>%
  pivot_wider(names_from = region, values_from = count, values_fill = 0)

radar_long <- radar_data %>%
  pivot_longer(cols = c(Cheshire, Cumberland), names_to = "region", values_to = "count")

p8 <- ggplot(radar_long, aes(x = reorder(`Crime type`, count), y = count, fill = region)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  coord_flip() +
  labs(
    title = "Crime Type Comparison (2023)",
    subtitle = "Cheshire vs Cumberland",
    x = "Crime Type",
    y = "Number of Incidents",
    fill = "Region"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold")) +
  scale_fill_manual(values = c("Cheshire" = "#3498db", "Cumberland" = "#e74c3c"))

ggsave(file.path(base_path, "Graphs/radarchart_vehicle_crime_2023.png"), 
       plot = p8, width = 10, height = 8, dpi = 120)
cat("Saved: radarchart_vehicle_crime_2023.png\n")

# --- 4.4 PIE CHART: Robbery Rate 2023 ---
robbery_2023 <- crime_summary_2023 %>% filter(`Crime type` == "Robbery")

p9 <- ggplot(robbery_2023, aes(x = "", y = count, fill = region)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(
    title = "Robbery Incidents Distribution (2023)",
    subtitle = "Cheshire vs Cumberland",
    fill = "Region"
  ) +
  theme_void() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
  geom_text(aes(label = paste0(count, "\n(", round(count/sum(count)*100, 1), "%)")),
            position = position_stack(vjust = 0.5), color = "white", size = 5) +
  scale_fill_manual(values = c("Cheshire" = "#3498db", "Cumberland" = "#e74c3c"))

ggsave(file.path(base_path, "Graphs/piechart_robbery_2023.png"), 
       plot = p9, width = 8, height = 6, dpi = 120)
cat("Saved: piechart_robbery_2023.png\n")

# --- 4.5 LINE GRAPH: Drug Offense Rate per 10k People ---
population_by_region <- population_data %>%
  group_by(region) %>%
  summarise(total_population = sum(Population, na.rm = TRUE), .groups = "drop")

drug_by_month <- crime_data %>%
  filter(`Crime type` == "Drugs") %>%
  group_by(region, month_year) %>%
  summarise(drug_count = n(), .groups = "drop") %>%
  left_join(population_by_region, by = "region") %>%
  mutate(
    drug_per_10k = (drug_count / total_population) * 10000,
    date = ym(month_year)
  ) %>%
  arrange(date)

p10 <- ggplot(drug_by_month, aes(x = date, y = drug_per_10k, color = region, group = region)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Drug Offense Rate per 10,000 People Over Time",
    subtitle = "Comparing Cheshire and Cumberland Counties",
    x = "Date",
    y = "Drug Offenses per 10,000 People",
    color = "Region"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_color_manual(values = c("Cheshire" = "#3498db", "Cumberland" = "#e74c3c")) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y")

ggsave(file.path(base_path, "Graphs/linegraph_drug_offenses_per_10k.png"), 
       plot = p10, width = 12, height = 6, dpi = 120)
cat("Saved: linegraph_drug_offenses_per_10k.png\n")

# =============================================================================
# SECTION 5: LINEAR MODELING VISUALIZATIONS
# =============================================================================

# Prepare data for linear modeling
house_by_district <- all_house_prices %>%
  mutate(postcode_district = str_extract(postcode, "^[A-Z]+[0-9]+")) %>%
  filter(year == 2023) %>%
  group_by(postcode_district, region) %>%
  summarise(avg_house_price = mean(price, na.rm = TRUE), .groups = "drop")

modeling_data <- house_by_district %>%
  left_join(broadband_by_district, by = c("postcode_district", "region")) %>%
  filter(!is.na(avg_download))

lm_price_speed <- lm(avg_house_price ~ avg_download, data = modeling_data)

p11 <- ggplot(modeling_data, aes(x = avg_download, y = avg_house_price, color = region)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, aes(group = 1), color = "black", linetype = "dashed") +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(labels = label_comma(prefix = "£")) +
  labs(
    title = "House Price vs Download Speed",
    subtitle = paste("R² =", round(summary(lm_price_speed)$r.squared, 3)),
    x = "Average Download Speed (Mbit/s)",
    y = "Average House Price (£)",
    color = "Region"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold")) +
  scale_color_manual(values = c("Cheshire" = "#3498db", "Cumberland" = "#e74c3c"))

ggsave(file.path(base_path, "Graphs/linear_model_price_vs_speed.png"), 
       plot = p11, width = 10, height = 7, dpi = 120)
cat("Saved: linear_model_price_vs_speed.png\n")

# --- Regional Comparison ---
broadband_by_region_summary <- broadband_by_region %>%
  group_by(region) %>%
  summarise(avg_download_speed = mean(mean_avg_download, na.rm = TRUE), .groups = "drop")

drug_rate_by_region <- crime_data %>%
  filter(`Crime type` == "Drugs", year == 2023) %>%
  group_by(region) %>%
  summarise(total_drugs = n(), .groups = "drop") %>%
  left_join(population_by_region, by = "region") %>%
  mutate(drug_rate_per_10k = (total_drugs / total_population) * 10000)

speed_vs_drug <- broadband_by_region_summary %>%
  left_join(drug_rate_by_region, by = "region")

p12 <- ggplot(speed_vs_drug, aes(x = avg_download_speed, y = drug_rate_per_10k, color = region)) +
  geom_point(size = 8, alpha = 0.8) +
  geom_text(aes(label = region), vjust = -1.5, size = 4) +
  labs(
    title = "Average Download Speed vs Drug Offense Rate (2023)",
    subtitle = "Regional Comparison",
    x = "Average Download Speed (Mbit/s)",
    y = "Drug Offenses per 10,000 People",
    color = "Region"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"), legend.position = "none") +
  scale_color_manual(values = c("Cheshire" = "#3498db", "Cumberland" = "#e74c3c")) +
  expand_limits(y = c(0, max(speed_vs_drug$drug_rate_per_10k) * 1.5))

ggsave(file.path(base_path, "Graphs/linear_model_speed_vs_drug.png"), 
       plot = p12, width = 10, height = 7, dpi = 120)
cat("Saved: linear_model_speed_vs_drug.png\n")

# =============================================================================
# SECTION 6: RECOMMENDATION SYSTEM
# =============================================================================

# --- House Price Score (lower is better) ---
house_price_scores <- all_house_prices %>%
  filter(year == 2023) %>%
  group_by(town, region) %>%
  summarise(avg_price = mean(price, na.rm = TRUE), num_sales = n(), .groups = "drop") %>%
  filter(num_sales >= 5) %>%
  mutate(price_score = 10 * (1 - (avg_price - min(avg_price)) / (max(avg_price) - min(avg_price))))

# --- Broadband Score (higher is better) ---
town_postcodes <- all_house_prices %>% distinct(town, postcode)

broadband_by_town <- broadband_by_district %>%
  left_join(town_postcodes %>% mutate(postcode_district = str_extract(postcode, "^[A-Z]+[0-9]+")) %>% 
              distinct(postcode_district, town), by = "postcode_district") %>%
  filter(!is.na(town)) %>%
  group_by(town) %>%
  summarise(avg_broadband = mean(avg_download, na.rm = TRUE), .groups = "drop") %>%
  mutate(broadband_score = 10 * (avg_broadband - min(avg_broadband)) / (max(avg_broadband) - min(avg_broadband)))

# --- Crime Score (lower is better) ---
crime_by_town <- crime_data %>%
  filter(year == 2023) %>%
  mutate(town_extracted = str_extract(`LSOA name`, "^[^\\s]+")) %>%
  group_by(region, town_extracted) %>%
  summarise(total_crime = n(), .groups = "drop") %>%
  rename(town = town_extracted)

crime_scores <- crime_by_town %>%
  mutate(crime_score = 10 * (1 - (total_crime - min(total_crime)) / (max(total_crime) - min(total_crime))))

# --- Combine Scores ---
overall_scores <- house_price_scores %>%
  select(town, region, price_score, avg_price) %>%
  left_join(broadband_by_town %>% select(town, broadband_score, avg_broadband), by = "town") %>%
  left_join(crime_scores %>% select(town, crime_score, total_crime), by = "town") %>%
  group_by(region) %>%
  mutate(
    broadband_score = ifelse(is.na(broadband_score), mean(broadband_score, na.rm = TRUE), broadband_score),
    crime_score = ifelse(is.na(crime_score), mean(crime_score, na.rm = TRUE), crime_score)
  ) %>%
  ungroup() %>%
  mutate(
    broadband_score = ifelse(is.na(broadband_score), 5, broadband_score),
    crime_score = ifelse(is.na(crime_score), 5, crime_score),
    overall_score = 0.4 * price_score + 0.3 * broadband_score + 0.3 * crime_score
  ) %>%
  arrange(desc(overall_score))

top_10_towns <- overall_scores %>% head(10)

print("=== TOP 10 RECOMMENDED TOWNS ===")
print(top_10_towns %>% select(town, region, price_score, broadband_score, crime_score, overall_score))

# --- Visualization: Top 10 Towns ---
p13 <- ggplot(top_10_towns, aes(x = reorder(town, overall_score), y = overall_score, fill = region)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = round(overall_score, 2)), hjust = -0.2, size = 4) +
  coord_flip() +
  labs(
    title = "Top 10 Recommended Towns for Property Investment",
    subtitle = "Based on House Prices (40%), Broadband (30%), Crime Rate (30%)",
    x = "Town",
    y = "Overall Score (0-10)",
    fill = "Region"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold")) +
  scale_fill_manual(values = c("Cheshire" = "#3498db", "Cumberland" = "#e74c3c")) +
  ylim(0, 12)

ggsave(file.path(base_path, "Graphs/recommendation_top10_towns.png"), 
       plot = p13, width = 12, height = 6, dpi = 120)
cat("Saved: recommendation_top10_towns.png\n")

# --- Score Breakdown ---
top_10_long <- top_10_towns %>%
  pivot_longer(cols = c(price_score, broadband_score, crime_score), 
               names_to = "score_type", values_to = "score")

p14 <- ggplot(top_10_long, aes(x = reorder(town, overall_score), y = score, fill = score_type)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  labs(
    title = "Score Breakdown for Top 10 Towns",
    subtitle = "Individual Scores: House Price, Broadband, Crime",
    x = "Town",
    y = "Score",
    fill = "Score Type"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold")) +
  scale_fill_manual(
    values = c("price_score" = "#27ae60", "broadband_score" = "#3498db", "crime_score" = "#9b59b6"),
    labels = c("Broadband Speed", "Crime Rate", "House Price")
  )

ggsave(file.path(base_path, "Graphs/recommendation_score_breakdown.png"), 
       plot = p14, width = 14, height = 8, dpi = 120)
cat("Saved: recommendation_score_breakdown.png\n")

# =============================================================================
# SECTION 7: SAVE DATA
# =============================================================================

write_csv(overall_scores, file.path(base_path, "cleanedData/recommendation_scores.csv"))
write_csv(top_10_towns, file.path(base_path, "cleanedData/top_10_recommended_towns.csv"))

# Print summary
cat("\n=== SUMMARY STATISTICS ===\n")
cat("\nHouse Prices (2023):\n")
print(all_house_prices %>% filter(year == 2023) %>% group_by(region) %>% 
        summarise(avg = mean(price), median = median(price), n = n()))

cat("\nBroadband Speeds:\n")
print(broadband_by_region %>% group_by(region) %>% 
        summarise(avg_download = mean(mean_avg_download), max_download = max(mean_max_download)))

cat("\nCrime Count (2023):\n")
print(crime_2023 %>% group_by(region) %>% summarise(total = n()))

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("All visualizations saved to:", file.path(base_path, "Graphs"), "\n")