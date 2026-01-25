library(tidyverse)

base_path <- "./Coursework"

# Read coverage data in chunks due to file size
coverage_file <- file.path(base_path, "ObtainedData/internet Speed/201809_fixed_pc_coverage_r01.csv")

# Read the file
broadband_coverage <- read_csv(coverage_file, show_col_types = FALSE)

# Check column names
print(colnames(broadband_coverage))

# Filter for Cheshire and Cumberland postcodes
# Cheshire: CH, CW, WA, SK (partial)
# Cumberland: CA

broadband_coverage_filtered <- broadband_coverage %>%
  mutate(
    postcode_area = str_extract(postcode, "^[A-Z]+")
  ) %>%
  filter(postcode_area %in% c("CH", "CW", "CA", "WA", "SK"))

# Select relevant coverage columns
# Common coverage metrics include SFBB availability, UFBB availability
coverage_clean <- broadband_coverage_filtered %>%
  select(
    postcode,
    postcode_area,
    contains("SFBB"),
    contains("UFBB"),
    contains("superfast"),
    contains("ultrafast"),
    contains("availability")
  )

# Add region classification
coverage_clean <- coverage_clean %>%
  mutate(
    region = case_when(
      postcode_area %in% c("CH", "CW", "WA", "SK") ~ "Cheshire",
      postcode_area == "CA" ~ "Cumberland",
      TRUE ~ "Unknown"
    )
  )

# Save cleaned coverage data
write_csv(coverage_clean, file.path(base_path, "cleanedData/broadband_coverage_cleaned.csv"))

# Create summary by postcode district
coverage_summary <- coverage_clean %>%
  mutate(
    postcode_district = str_extract(postcode, "^[A-Z]+[0-9]+")
  ) %>%
  group_by(postcode_district, region) %>%
  summarise(
    num_postcodes = n(),
    across(where(is.numeric), ~mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )

write_csv(coverage_summary, file.path(base_path, "cleanedData/broadband_coverage_summary.csv"))

print("Broadband coverage data cleaned and saved!")
print(head(coverage_summary))