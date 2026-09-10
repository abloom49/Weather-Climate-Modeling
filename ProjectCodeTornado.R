setwd("/Users/ameliabloom/Desktop/Econ108/Project")
# base_numbers <- c("15", "16", "17","18", "19", "20", "21", "22", "23", "24")

# Generate a sequence from 2011 to 2024
years <- seq(18, 24)

# Convert the sequence of numbers to character strings
base_numbers <- as.character(years)

# Print the result to check
print(base_numbers)

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
library(dplyr)
# Loop through each city in the list
for (tornado_csv in tornado_csvs) {
  # Construct the file path (assuming the file names follow a consistent naming convention)
  file_path <- paste0("Weather Data/", tornado_csv, ".csv")
  
  # Read the CSV file
  df <- read.csv(file_path)
  
  # Select only specific columns
  df <- select(df, BEGIN_DATE, DEATHS_DIRECT, INJURIES_DIRECT,	DAMAGE_PROPERTY_NUM, STATE_ABBR,BEGIN_LAT,	BEGIN_LON,	END_LAT,	END_LON, EPISODE_ID )
  
  df$Year <- as.integer(substr(df$BEGIN_DATE, 7, 10))
  df$Day <- as.integer(substr(df$BEGIN_DATE, 4, 5))
  df$Month <- as.integer(substr(df$BEGIN_DATE, 1, 2))
  
  df$Tornado <- 1
  
  
  # Combine the data frames
  tornado_df <- bind_rows(tornado_df, df)
}

# Write the data frame to a CSV file, overwriting any existing file
library(dplyr)

# Assuming your data frame is named df and it has columns Year, Month, Day
# and each row represents one tornado occurrence

# I have 19 different places that had tornados on 3/11 with the code 196826

# df_tornado_unique <- tornado_df %>%
#   group_by(EPISODE_ID, Year, Month, Day) %>%
#   slice(1) %>%  # Keeps the first occurrence based on the grouping
#   ungroup()

df_tornado_unique <- tornado_df %>%
  group_by(Year, Month, Day, STATE_ABBR) %>%
  slice(1) %>%  # Keeps the first occurrence based on the grouping
  ungroup()
df_tornado_graph <- df_tornado_unique

# df_tornado_unique should now have only one per state per day

# Summarize the data to get one record per day with the total number of tornadoes
df_tornado_unique <- df_tornado_unique %>%
  group_by(Year, Month, Day) %>%
  summarise(
    Total_Tornadoes = n()  # Count the number of rows in each group, which equals the number of tornadoes
  ) %>%
  ungroup()  # Remove the grouping structure

# df_tornado_unique should now have only one per state per day and should have a total count on that day

head(df_tornado_unique)




# View the resulting data frame
df_tornado <- left_join(tornado_df, df_tornado_unique, by = c("Year", "Month", "Day"))

# next steps replace places with no tornados with 0
# look into whether there are any tornados rather than number of them
df_tornado_unique$TimeTrend <- df_tornado_unique$Year - min(df_tornado_unique$Year)
num_episodes_glm <- glm(Total_Tornadoes ~ TimeTrend, data = df_tornado_unique, family = gaussian())
summary(num_episodes_glm)

# Load necessary libraries
library(dplyr)
library(lubridate)

# Define all U.S. state abbreviations
states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL",
            "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT",
            "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
            "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

# Generate all dates for a leap year (e.g., 2024)
dates <- seq(as.Date("2018-01-01"), as.Date("2024-12-31"), by = "day")

# Create a data frame with all combinations of states and dates
full_df <- expand.grid(STATE_ABBR = states, Date = dates)

# Add Year, Month, and Day columns
full_df <- full_df %>%
  mutate(Year = year(Date),
         Month = month(Date),
         Day = day(Date))

df_tornado <- left_join(full_df, df_tornado, by = c("Year", "Month", "Day", "STATE_ABBR"))
df_tornado$Tornado[is.na(df_tornado$Tornado)] <- 0
df_tornado$Total_Tornadoes[is.na(df_tornado$Total_Tornadoes)] <- 0


write.csv(df_tornado, "Cleaned Data/tornado_df.csv", row.names = FALSE)
write.csv(df_tornado_graph, "Cleaned Data/tornado_df_graph.csv", row.names = FALSE)


# next steps replace places with no tornados with 0
# look into whether there are any tornados rather than number of them
df_tornado$TimeTrend <- df_tornado$Year - min(df_tornado$Year)
num_episodes_glm <- glm(Total_Tornadoes ~ TimeTrend, data = df_tornado, family = gaussian())
summary(num_episodes_glm)

df_tornado$Month = factor(df_tornado$Month)
df_tornado$Day = factor(df_tornado$Day)
library(lubridate)
# df_tornado$lubriate_date <- as.Date(paste(df_tornado$Year, df_tornado$Month, df_tornado$Day, sep="-"))

# Get the day of the year
# df_tornado$day_of_year_number <- yday(df_tornado$Date)
# 
# df_tornado$week_number <- factor(ceiling(df_tornado$day_of_year_number / 7))
# 
# 
# 
# tornado_logistic_reg <- glm(Tornado~TimeTrend + df_tornado$week_number + STATE_ABBR, data=df_tornado, family='binomial')
# summary(tornado_logistic_reg)
library(ggplot2)
library(maps)
library(ggmap)

us_map <- map_data("state")

ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "white", colour = "black") +
  geom_point(data =  df_tornado_graph, aes(x = BEGIN_LON, y = BEGIN_LAT), color = "red", size = .1) +
  ggtitle("Hurricanes on US Map") +
  theme_minimal()
