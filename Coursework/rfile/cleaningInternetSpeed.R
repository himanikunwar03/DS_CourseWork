library(tidyverse)



broadband_performance_raw = read_csv(
  ".\\Coursework\\ObtainedData\\internet speed\\201805_fixed_pc_performance_r03.csv"
)

broadband_clean <- broadband_performance_raw %>%
  rename(
    postcode = postcode,
    postcode_area = `postcode area`,
    avg_download = `Average download speed (Mbit/s)`,
    max_download = `Maximum download speed (Mbit/s)`,
    avg_upload = `Average upload speed (Mbit/s)`,
    max_upload = `Maximum upload speed (Mbit/s)`
  )

broadband_filtered <- broadband_clean %>%
  filter(
    postcode_area %in% c("CH", "CW", "CA")
  ) %>%
  filter(
    !is.na(avg_download),
    !is.na(avg_upload),
    avg_download > 0,
    avg_upload > 0
  )

broadband_postcode_summary <- broadband_filtered %>%
  group_by(postcode) %>%
  summarise(
    mean_avg_download = mean(avg_download),
    mean_max_download = mean(max_download, na.rm = TRUE),
    mean_avg_upload = mean(avg_upload),
    mean_max_upload = mean(max_upload, na.rm = TRUE),
    connections = n(),
    .groups = "drop"
  )
print(broadband_postcode_summary)

write_csv(broadband_postcode_summary,".\\Coursework\\cleanedData\\broadbandSpeed_performance.csv")
