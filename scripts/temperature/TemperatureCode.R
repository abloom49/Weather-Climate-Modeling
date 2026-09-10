setwd("/Users/ameliabloom/Desktop/Econ108/Project")


library(dplyr)

# cities studied: 20 with largest population
# note: Fort Worth has the 12th largest population but is 30 minutes away from 
# Dallas (9th largest) so the weather is not distinct. I did not included Fort
# Worth in my data to avoid having two  observations for the same area. 
cities <- c("NewYork", "LosAngeles", "Chicago", "Houston", "Phoenix","Philadelphia", "SanAntonio", "SanDiego", "Dallas", "Jacksonville", "Austin", "SanJose", "Charlotte", "Indianapolis", "SanFrancisco", "Seattle", "Denver", "OklahomaCity")

# will store the data in this
precipitation_temperature_df <- data.frame()

# Add in each city's data to the combined dataframe
for (city in cities) {
  file_path <- paste0("Weather Data/", city, ".csv")
  df <- read.csv(file_path)
  df <- select(df, PRCP, TMAX, TMIN, DATE)
  df <- mutate(df, Location = city)
  print(df$DATE)
  # get date components
  df$Year <- as.integer(substr(df$DATE, 1, 4))
  df$Month <- as.integer(substr(df$DATE, 6, 7))
  df$Day <- as.integer(substr(df$DATE, 9, 10))
  
  # these are the years that all cities have data for so constraining to these
  df <- subset(df, Year >= 1999)
  df <- subset(df, Year <= 2024)
  
  precipitation_temperature_df <- bind_rows(precipitation_temperature_df, df)
}


# creating a time trend
precipitation_temperature_df$TimeTrend <- precipitation_temperature_df$Year - min(precipitation_temperature_df$Year)




precipitation_temperature_df$Month <- factor(precipitation_temperature_df$Month)

# Temperature and Precipitation Changes section

# Linear regression for maximum temperature with city fixed effects, a time trend, and controlling for month
max_temp_glm <- glm(TMAX ~ TimeTrend + factor(Location) + Month, data = precipitation_temperature_df, family = gaussian())
summary(max_temp_glm)
# regressions for minimum temperature and precipitation
min_temp_glm <- glm(TMIN ~ TimeTrend + factor(Location) + Month, data = precipitation_temperature_df, family = gaussian())
summary(min_temp_glm)
precipitation_glm <- glm(PRCP ~ TimeTrend + factor(Location) + Month, data = precipitation_temperature_df, family = gaussian())
summary(precipitation_glm)

#creating a factor variable for each week of the year
library(lubridate)
precipitation_temperature_df$day_of_year_number <- yday(precipitation_temperature_df$DATE)
# precipitation_temperature_df$week_number <- factor(ceiling(precipitation_temperature_df$day_of_year_number / 7))


library(dplyr)

precipitation_temperature_df$daily_range = precipitation_temperature_df$TMAX - precipitation_temperature_df$TMIN


# creating a measure variance each year
yearly_var_df <- precipitation_temperature_df %>%
  group_by(Year, Location) %>%
  summarise(
    Var_year_TMAX = var(TMAX),
    Var_year_TMIN = var(TMIN), 
    Var_year_PRCP = var(PRCP)
  ) %>%
  ungroup()

# adding these values back into the original dataframe
precipitation_temperature_df <- left_join(precipitation_temperature_df, yearly_var_df, by = c("Year", "Location"))

# to know how many NAs I am dropping
na_count_PRCP <- sum(is.na(precipitation_temperature_df$PRCP))
na_count_TMAX <- sum(is.na(precipitation_temperature_df$TMAX))
na_count_TMIN <- sum(is.na(precipitation_temperature_df$TMIN))
prin(na_count_PRCP + na_count_TMAX + na_count_TMIN)

# dropping them
precipitation_temperature_df <- na.omit(precipitation_temperature_df, cols = c("PRCP", "TMAX", "TMIN"))

# 
# # Linear regressions for daily spread with city fixed effects, a time trend, and controlling for week and month
spread_temp_glm <- glm(daily_range ~ TimeTrend + factor(Location) + Month, data = precipitation_temperature_df, family = gaussian())
summary(spread_temp_glm)

# # Linear regressions for yearly variances with city fixed effects, a time trend, and controlling for month

var_year_max_temp_glm <- glm(Var_year_TMAX ~ TimeTrend + factor(Location) + Month, data = precipitation_temperature_df, family = gaussian())
summary(var_year_max_temp_glm)
var_year_min_temp_glm <- glm(Var_year_TMIN ~ TimeTrend + factor(Location) + Month, data = precipitation_temperature_df, family = gaussian())
summary(var_year_min_temp_glm)
var_year_PRCP_glm <- glm(Var_year_PRCP ~ TimeTrend + factor(Location) + Month, data = precipitation_temperature_df, family = gaussian())
summary(var_year_PRCP_glm)


data_1999 <- precipitation_temperature_df %>%
  filter(Year == "1999")


# settins Extreme Values
extreme_values <- data_1999 %>%
  group_by(Location,) %>%
  summarise(
    tmax_95 = quantile(TMAX, probs = 0.95, na.rm = TRUE),
    tmin_05 = quantile(TMIN, probs = 0.05, na.rm = TRUE)
  )

precipitation_temperature_df <- left_join(precipitation_temperature_df, extreme_values, by = c("Location"))


precipitation_temperature_df <- precipitation_temperature_df %>%
  mutate(
    extreme_temp = as.integer((TMAX > tmax_95) | (TMIN < tmin_05)),
    extreme_high = as.integer((TMAX > tmax_95)),
    extreme_low = as.integer( (TMIN < tmin_05))
  )

write.csv(precipitation_temperature_df, "Cleaned Data/precipitation_temperature.csv", row.names = FALSE)

# regressions for Extreme Values
precipitation_temperature_df_2000  <- subset(precipitation_temperature_df, Year >= 2007)
extreme_temp_glm <- glm(extreme_temp ~ TimeTrend + factor(Location) + Month, data = precipitation_temperature_df_2000, family = "binomial")
summary(extreme_temp_glm)
extreme_low_glm <- glm(extreme_low ~ TimeTrend + factor(Location) + Month, data = precipitation_temperature_df_2000, family = "binomial")
summary(extreme_low_glm)
extreme_high_glm <- glm(extreme_high ~ TimeTrend + factor(Location) + Month, data = precipitation_temperature_df_2000, family = "binomial")
summary(extreme_high_glm)



# squared time trend regression
precipitation_temperature_df$SqTimeTrend = (precipitation_temperature_df$TimeTrend)**2
max_temp_glm_additionalterm <- glm(TMAX ~ TimeTrend + SqTimeTrend + factor(Location) + Month, data = precipitation_temperature_df, family = gaussian())
summary(max_temp_glm_additionalterm)

min_temp_glm_additionalterm <- glm(TMIN ~ TimeTrend + SqTimeTrend + factor(Location) + Month, data = precipitation_temperature_df, family = gaussian())
summary(min_temp_glm_additionalterm)
PRCP_glm_additionalterm <- glm(PRCP ~ TimeTrend + SqTimeTrend + factor(Location) + Month, data = precipitation_temperature_df, family = gaussian())
summary(PRCP_glm_additionalterm)
