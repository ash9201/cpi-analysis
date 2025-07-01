library(stringr)
library(dplyr)

cpi_raw <- read.csv("All_India_Index_Upto_Feb24.csv", stringsAsFactors = FALSE)

# Fix typo
cpi_raw[45, 3] = "March"

# Replace dots with spaces in all column names
cpi <- cpi_raw %>%
  rename_with(~ str_replace_all(., "\\.", " "))

cpi <- cpi %>%
  mutate(Date = as.Date(paste(Year, Month, "1"), format = "%Y %B %d")) %>%
  select(Date, everything(), -Year, -Month)

rural_df <- cpi %>%
  filter(Sector == "Rural") %>%
  select(-Sector)

urban_df <- cpi %>%
  filter(Sector == "Urban") %>%
  select(-Sector)

combined_df <- cpi %>%
  filter(Sector == "Rural+Urban") %>%
  select(-Sector)

missing_row <- tibble(
  Date = as.Date("2019-04-01"),
  !!!setNames(rep(list(NA), ncol(combined_df) - 1), names(combined_df)[-1])
)

# Append it and re‐order by Date
combined_df <- combined_df %>%
  bind_rows(missing_row) %>%
  arrange(Date)
rural_df <- rural_df %>%
  bind_rows(missing_row) %>%
  arrange(Date)
urban_df <- urban_df %>%
  bind_rows(missing_row) %>%
  arrange(Date)

# Linear Interpolation
combined_df[ , -1] <- apply(combined_df[ , -1], 2, function(x) na.approx(x, na.rm = FALSE))
urban_df[ , -1] <- apply(urban_df[ , -1], 2, function(x) na.approx(x, na.rm = FALSE))
rural_df <- rural_df %>% select(-Housing)
rural_df[ , -1] <- apply(rural_df[ , -1], 2, function(x) na.approx(x, na.rm = FALSE))

save(combined_df, urban_df, rural_df, file = "cpi_data_cleaned.RData")

