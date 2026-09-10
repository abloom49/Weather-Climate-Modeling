setwd("/Users/ameliabloom/Desktop/Econ108/Project")

library(dplyr)

# List of city names
cities <- c("NewYork", "LosAngeles", "Chicago", "Houston", "Phoenix","Philadelphia", "SanAntonio", "SanDiego", "Dallas", "Jacksonville", "Austin", "SanJose", "Charlotte", "Indianapolis", "SanFrancisco", "Seattle", "Denver", "OklahomaCity")

# Initialize an empty data frame to store the combined data
precipitation_temperature_df <- data.frame()

# Loop through each city in the list
for (city in cities) {
  # Construct the file path (assuming the file names follow a consistent naming convention)
  file_path <- paste0("Weather Data/", city, ".csv")
  
  # Read the CSV file
  df <- read.csv(file_path)
  
  # Select only specific columns
  df <- select(df, PRCP, TMAX, TMIN, DATE)
  
  # Add a new column indicating the location
  df <- mutate(df, Location = city)
  
  df$Year <- as.integer(substr(df$DATE, 1, 4))
  df$Month <- as.integer(substr(df$DATE, 6, 7))
  df$Day <- as.integer(substr(df$DATE, 9, 10))
  
  df <- subset(df, Year >= 1999)
  df <- subset(df, Year <= 2024)
  
  # Combine the data frames
  precipitation_temperature_df <- bind_rows(precipitation_temperature_df, df)
}

# Write the data frame to a CSV file, overwriting any existing file
write.csv(precipitation_temperature_df, "Cleaned Data/precipitation_temperature.csv", row.names = FALSE)

precipitation_temperature_df$TimeTrend <- precipitation_temperature_df$Year - min(precipitation_temperature_df$Year)
precipitation_temperature_df$DailySpread <- precipitation_temperature_df$TMAX - precipitation_temperature_df$TMIN


# converting dates into weeks of the year
library(lubridate)
precipitation_temperature_df$day_of_year_number <- yday(precipitation_temperature_df$DATE)
precipitation_temperature_df$week_number <- factor(ceiling(precipitation_temperature_df$day_of_year_number / 7))

# Run generalized linear model for Maximum Temperature with city fixed effects and a time trend
max_temp_glm <- glm(TMAX ~ TimeTrend + factor(Location) + week_number, data = precipitation_temperature_df, family = gaussian())
summary(max_temp_glm)
min_temp_glm <- glm(TMIN ~ TimeTrend + factor(Location) + week_number, data = precipitation_temperature_df, family = gaussian())
summary(min_temp_glm)
spread_temp_glm <- glm(DailySpread ~ TimeTrend + factor(Location) + week_number, data = precipitation_temperature_df, family = gaussian())
summary(spread_temp_glm)
precipitation_glm <- glm(PRCP ~ TimeTrend + factor(Location) + week_number, data = precipitation_temperature_df, family = gaussian())
summary(precipitation_glm)

library(dplyr)

variance_df <- precipitation_temperature_df %>%
  group_by(Year, Month, Location) %>%
  summarise(
    Var_TMAX = var(TMAX),  # Variance of max temperature, removing NA values if any
    Var_TMIN = var(TMIN), 
    Var_PRCP = var(PRCP)# Variance of min temperature, removing NA values if any
  ) %>%
  ungroup()

yearly_var_df <- precipitation_temperature_df %>%
  group_by(Year, Location) %>%
  summarise(
    Var_year_TMAX = var(TMAX),  # Variance of max temperature, removing NA values if any
    Var_year_TMIN = var(TMIN), 
    Var_year_PRCP = var(PRCP)# Variance of min temperature, removing NA values if any
  ) %>%
  ungroup()

precipitation_temperature_df <- left_join(precipitation_temperature_df, variance_df, by = c("Year", "Month", "Location"))
precipitation_temperature_df <- left_join(precipitation_temperature_df, yearly_var_df, by = c("Year", "Location"))


var_max_temp_glm <- glm(Var_TMAX ~ TimeTrend + factor(Location) + week_number, data = precipitation_temperature_df, family = gaussian())
summary(var_max_temp_glm)
var_min_temp_glm <- glm(Var_TMIN ~ TimeTrend + factor(Location) + week_number, data = precipitation_temperature_df, family = gaussian())
summary(var_min_temp_glm)
var_PRCP_glm <- glm(Var_PRCP ~ TimeTrend + factor(Location) + week_number, data = precipitation_temperature_df, family = gaussian())
summary(var_PRCP_glm)
var_year_max_temp_glm <- glm(Var_year_TMAX ~ TimeTrend + factor(Location) + week_number, data = precipitation_temperature_df, family = gaussian())
summary(var_year_max_temp_glm)
var_year_min_temp_glm <- glm(Var_year_TMIN ~ TimeTrend + factor(Location) + week_number, data = precipitation_temperature_df, family = gaussian())
summary(var_year_min_temp_glm)
var_year_PRCP_glm <- glm(Var_year_PRCP ~ TimeTrend + factor(Location) + week_number, data = precipitation_temperature_df, family = gaussian())
summary(var_year_PRCP_glm)

base_numbers <- c("23", "24")
parts <- 1:5

# Initialize an empty vector to store the results
tornado_csvs <- vector("character", length = length(base_numbers) * length(parts))

# Index for storing in the identifiers vector
index <- 1

# Generate the tornado_csvs
for (number in base_numbers) {
  for (part in parts) {
    tornado_csvs[index] <- paste(number, paste("part", part, sep=""), sep="")
    index <- index + 1
  }
}
# Initialize an empty data frame to store the combined data
tornado_df <- data.frame()

# Loop through each city in the list
for (tornado_csv in tornado_csvs) {
  # Construct the file path (assuming the file names follow a consistent naming convention)
  file_path <- paste0("Weather Data/", tornado_csv, ".csv")
  
  # Read the CSV file
  df <- read.csv(file_path)
  
  # Select only specific columns
  df <- select(df, BEGIN_DATE, DEATHS_DIRECT, INJURIES_DIRECT,	DAMAGE_PROPERTY_NUM, STATE_ABBR,BEGIN_LAT,	BEGIN_LON,	END_LAT,	END_LON )
  
  
  df$Year <- as.integer(substr(df$BEGIN_DATE, 6, 7))
  df$Month <- as.integer(substr(df$BEGIN_DATE, 3, 4))
  df$Day <- as.integer(substr(df$BEGIN_DATE, 1, 1))

  
  # Combine the data frames
  tornado_df <- bind_rows(tornado_df, df)
}

# Write the data frame to a CSV file, overwriting any existing file
write.csv(tornado_df, "Cleaned Data/tornado_df.csv", row.names = FALSE)



