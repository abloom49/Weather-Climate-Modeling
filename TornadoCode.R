setwd("/Users/ameliabloom/Desktop/Econ108/Project")

# list of years we cover (2006 to 2024)
years <- seq(6, 24)
base_numbers <- as.character(years)

# creating a vector of all the files I am combining to get all the tornado data
# (I was only able to download 1/6 of each year at once so every year is downloade
# seperately and in 6 parts)
parts <- 1:6
tornado_csvs <- vector("character", length = length(base_numbers) * length(parts))
index <- 1
for (number in base_numbers) {
  for (part in parts) {
    tornado_csvs[index] <- paste(number, paste("part", part, sep=""), sep="")
    index <- index + 1
  }
}

# creating a df to store the data
tornado_df <- data.frame()
library(dplyr)
# add each section of each year to the data
for (tornado_csv in tornado_csvs) {
  file_path <- paste0("Weather Data/", tornado_csv, ".csv")
  df <- read.csv(file_path)

  # Select only the columns I want
  df <- select(df, BEGIN_DATE, DEATHS_DIRECT, INJURIES_DIRECT,	DAMAGE_PROPERTY_NUM, STATE_ABBR,BEGIN_LAT,	BEGIN_LON,	END_LAT,	END_LON, EPISODE_ID )
  
  # pull date components out
  df$Year <- as.integer(substr(df$BEGIN_DATE, 7, 10))
  df$Day <- as.integer(substr(df$BEGIN_DATE, 4, 5))
  df$Month <- as.integer(substr(df$BEGIN_DATE, 1, 2))
  
  # mark that there was a tornado on this day (dataset includes only data on
  # tornados that occured so if a day-state combination is an observation in 
  # the dataset then there was a tornado in that state on that day)
  df$Tornado <- 1
  
  # Add the new data into the combined data
  tornado_df <- bind_rows(tornado_df, df)
}

library(dplyr)

# there are sometimes multiple different tornados per state per day
# My analysis looks into predicting whether there was a tornado in a state in a day
# so second, third, etc tornados are not relevent for this analysis
# I chose this method as from visual inspection it appears a given tornado was 
# added a varying number of times (same tornado marked down 1 time or 18 times for example)
# and I didn't want this to bias my data

# keeps only one per state per day
df_tornado_unique <- tornado_df %>%
  group_by(Year, Month, Day, STATE_ABBR) %>%
  slice(1) %>%  
  ungroup()

# used as we want to graph only the states with tornados each day so saving this before we add in
# all states and days to df_tornado_unique in a few steps
df_tornado_graph <- df_tornado_unique


# Summarize the data to get one number per day with the total number of states with tornadoes
df_tornado_unique <- df_tornado_unique %>%
  group_by(Year, Month, Day) %>%
  summarise(
    Total_Tornadoes = n()
  ) %>%
  ungroup()


# # adds in the totals on each day to the original dataset
# tornado_df <- left_join(tornado_df, df_tornado_unique, by = c("Year", "Month", "Day"))

library(dplyr)
library(lubridate)

# creates an empty dataframe with every day in every state as our original df only has 
# places and times where there were tornados
states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL",
            "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT",
            "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
            "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

dates <- seq(as.Date("2006-01-01"), as.Date("2024-12-31"), by = "day")
full_df <- expand.grid(STATE_ABBR = states, Date = dates)

# Adds year, month, and day
full_df <- full_df %>%
  mutate(Year = year(Date),
         Month = month(Date),
         Day = day(Date))

# cuts back to just one per state again but now has total counts
tornado_df <- tornado_df %>%
  group_by(Year, Month, Day, STATE_ABBR) %>%
  slice(1) %>%  
  ungroup()

# adds the number of tornados each day and where they are for every day when there was a tornado 
tornado_df <- left_join(full_df, tornado_df, by = c("Year", "Month", "Day", "STATE_ABBR"))
tornado_df$Tornado[is.na(tornado_df$Tornado)] <- 0

# adds in the totals on each day to the original dataset
tornado_df <- left_join(tornado_df, df_tornado_unique, by = c("Year", "Month", "Day"))

# creates a time trend
tornado_df$TimeTrend <- tornado_df$Year - min(tornado_df$Year)

tornado_df$Month = factor(tornado_df$Month)

library(lubridate)
tornado_df$lubriate_date <- as.Date(paste(tornado_df$Year, tornado_df$Month, tornado_df$Day, sep="-"))

tornado_df$day_of_year_number <- yday(tornado_df$Date)

tornado_df$week_number <- factor(ceiling(tornado_df$day_of_year_number / 7))

write.csv(tornado_df, "Cleaned Data/tornado_df.csv", row.names = FALSE)
write.csv(df_tornado_graph, "Cleaned Data/tornado_df_graph.csv", row.names = FALSE)

# regressions for tornado frequency over time section

tornado_logistic_reg <- glm(Tornado~TimeTrend + tornado_df$Month + STATE_ABBR, data=tornado_df, family='binomial')
summary(tornado_logistic_reg)

tornado_logistic_reg <- glm(Total_Tornadoes~TimeTrend + tornado_df$Month, data=tornado_df, family='gaussian')
summary(tornado_logistic_reg)

#runs tornado location section

library(ggplot2)
library(maps)
library(ggmap)

us_map <- map_data("state")

ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "white", colour = "black") +
  geom_point(data =  df_tornado_graph, aes(x = BEGIN_LON, y = BEGIN_LAT), color = "red", size = .1) +
  ggtitle("Hurricanes on US Map") +
  theme_minimal()


yearly_data_2006 <- filter(df_tornado_graph, Year == 2006)
yearly_data_2012 <- filter(df_tornado_graph, Year == 2012)
yearly_data_2018 <- filter(df_tornado_graph, Year == 2018)
yearly_data_2024 <- filter(df_tornado_graph, Year == 2024)


lon_range <- c(-130, -60)  # Adjust these values based on your specific data
lat_range <- c(20, 50)

ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "white", colour = "black") +
  geom_point(data =  yearly_data_2006, aes(x = BEGIN_LON, y = BEGIN_LAT), color = "red", size = 1) +
  ggtitle("2006 Tornados") +
  theme_minimal() +
  xlim(lon_range) +  # Set longitude limits
  ylim(lat_range)

ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "white", colour = "black") +
  geom_point(data =  yearly_data_2012, aes(x = BEGIN_LON, y = BEGIN_LAT), color = "red", size = 1) +
  ggtitle("2012 Tornados") +
  theme_minimal()+
  xlim(lon_range) +  # Set longitude limits
  ylim(lat_range)

ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "white", colour = "black") +
  geom_point(data =  yearly_data_2018, aes(x = BEGIN_LON, y = BEGIN_LAT), color = "red", size = 1) +
  ggtitle("2018 Tornados") +
  theme_minimal()+
  xlim(lon_range) +  # Set longitude limits
  ylim(lat_range)

ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "white", colour = "black") +
  geom_point(data =  yearly_data_2024, aes(x = BEGIN_LON, y = BEGIN_LAT), color = "red", size = 1) +
  ggtitle("2024 Tornados") +
  theme_minimal()+
  xlim(lon_range) +  # Set longitude limits
  ylim(lat_range)


# has longitude and latitude of each state
df_lat <- read.csv("Weather Data/US_GeoCode.csv")
tornado_df_lat <- left_join(tornado_df, df_lat, by = c("STATE_ABBR"))


# used to create the graphs of the clustered variables 
yearly_data_all <- tornado_df_lat

# 2006 - 2010
yearly_data_one <- tornado_df_lat %>%
  filter(Year <= 2010 ) 

# 2011 - 2015
yearly_data_two <- tornado_df_lat %>%
  filter( Year > 2010, Year <= 2015) 

# 2016 - 2020
yearly_data_three <- tornado_df_lat %>%
  filter( Year > 2015, Year <= 2020) 

# 2021 - 2024
yearly_data_four <- tornado_df_lat %>%
  filter(Year > 2020) 

write.csv(tornado_df_lat, "Cleaned Data/tornado_df_lat.csv", row.names = FALSE)

# to make each of the figures, I replaced the yearly_data_four on row 222 and 
# 233 with the dataset I was using and changed the title accordingly


state_summary <- yearly_data_all %>%
  group_by(STATE_ABBR) %>%
  summarize(TornadoFrequency = mean(Tornado), .groups = 'drop')

# Perform k-means clustering on the aggregated data
set.seed(123)
kmeans_result <- kmeans(state_summary[2], centers = 2)

# Add cluster assignment back to the aggregated data
state_summary$Cluster <- kmeans_result$cluster

df_kmeans <- left_join(yearly_data_all, state_summary, by = "STATE_ABBR")



df_kmeans_unique <- df_kmeans %>%
  group_by(STATE_ABBR) %>%
  slice(1) %>%
  ungroup()

df_kmeans_unique <- df_kmeans_unique %>%
  select(STATE_ABBR, Cluster, BEGIN_LON_NEW, BEGIN_LAT_NEW)

df_kmeans_unique <- df_kmeans_unique %>%
  filter(BEGIN_LON_NEW > -140 ) 

ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  geom_point(data = df_kmeans_unique, aes(x = BEGIN_LON_NEW, y = BEGIN_LAT_NEW, color = factor(Cluster)), size = 5) +
  scale_color_manual(values = c("blue", "red")) +
  labs(title = "States Clustered by Total Number of Tornados",
       x = "Longitude",
       y = "Latitude",
       color = "Cluster") +
  theme_minimal() +
  theme(legend.position = "right")
