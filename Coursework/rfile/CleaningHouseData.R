library(tidyverse)
houseColsNames = c(
  "transaction_id",
  "price",
  "transfer_date",
  "postcode",
  "property_type",
  "new_build",
  "tenure",
  "paon",
  "saon",
  "street",
  "locality",
  "town",
  "district",
  "county",
  "ppd_category",
  "record_status"
)
housePrices2022 = read_csv(
  ".\\Coursework\\ObtainedData\\housePrice-2022.csv",
  col_names = houseColsNames
)
housePrices2023 = read_csv(
  ".\\Coursework\\ObtainedData\\housePrice2023.csv",
  col_names = houseColsNames
)
housePrices2024 = read_csv(
  ".\\Coursework\\ObtainedData\\housePrice2024.csv",
  col_names = houseColsNames
)


standardise_place_names = function(df) {
  df %>%
    mutate(
      town = str_to_title(town),
      district = str_to_title(district),
      county = str_to_title(county)
    )
}

housePrices2022 <- standardise_place_names(housePrices2022)
housePrices2023 <- standardise_place_names(housePrices2023)
housePrices2024 <- standardise_place_names(housePrices2024)

filter_county = function(df){
  df %>% 
    filter(
      county %in%c(
        "Cheshire",
        "Cheshire East",
        "Cheshire West And Chester",
        "Cumberland"
      )
    )
}

housePrices2022 <- filter_county(housePrices2022)
housePrices2023 <- filter_county(housePrices2023)
housePrices2024 <- filter_county(housePrices2024)

select_house_price_columns = function(df) {
  df %>%
    select(
      transaction_id,
      price,
      transfer_date,
      town,
      district,
      county,
      postcode
    )
}

housePrices2022 <- select_house_price_columns(housePrices2022)
housePrices2023 <- select_house_price_columns(housePrices2023)
housePrices2024 <- select_house_price_columns(housePrices2024)

clean_postcode <- function(x) {
  x %>%
    str_to_upper() %>%          # make uppercase
    str_replace_all(" ", "") %>%# remove spaces
    na_if("")                   # convert empty strings to NA
}

housePrices2022 <- housePrices2022 %>%
  mutate(postcode = clean_postcode(postcode))

housePrices2023 <- housePrices2023 %>%
  mutate(postcode = clean_postcode(postcode))

housePrices2024 <- housePrices2024 %>%
  mutate(postcode = clean_postcode(postcode))


remove_invalid_house_prices <- function(df) {
  df %>%
    filter(!is.na(postcode)) %>%        # postcode required for joins
    filter(!is.na(price), price > 0) %>%
    filter(!is.na(transfer_date))
}

housePrices2022 <- remove_invalid_house_prices(housePrices2022)
housePrices2023 <- remove_invalid_house_prices(housePrices2023)
housePrices2024 <- remove_invalid_house_prices(housePrices2024)

write_csv(
  housePrices2022,
  ".\\Coursework\\cleanedData\\cleaned_house_prices_2022.csv"
)

write_csv(
  housePrices2023,
  ".\\Coursework\\cleanedData\\cleaned_house_prices_2023.csv"
)

write_csv(
  housePrices2024,
  ".\\Coursework\\cleanedData\\cleaned_house_prices_2024.csv"
)


