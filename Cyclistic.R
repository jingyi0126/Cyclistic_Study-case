library(tidyverse)
library(ggplot2)
library(skimr)
library(patchwork)
library(readr)
library(dplyr)
library(janitor)
library(lubridate)
library(scales)
# Load the datasets from April to September 2020
apr_20 <- read.csv("202004-divvy-tripdata.csv", header = TRUE, sep = ",")
may_20 <- read.csv("202005-divvy-tripdata.csv", header = TRUE, sep = ",")
jun_20 <- read.csv("202006-divvy-tripdata.csv", header = TRUE, sep = ",")
jul_20 <- read.csv("202007-divvy-tripdata.csv", header = TRUE, sep = ",")
aug_20 <- read.csv("202008-divvy-tripdata.csv", header = TRUE, sep = ",")
sep_20 <- read.csv("202009-divvy-tripdata.csv", header = TRUE, sep = ",")

# Check the class of each column in the April dataset
sapply(apr_20, class)

# Combine all monthly datasets into one
bikes <- rbind(apr_20, may_20, jun_20, jul_20, aug_20, sep_20)

# Convert station ID columns to character type
bikes <- bikes %>% 
  mutate(
    start_station_id = as.character(start_station_id),
    end_station_id = as.character(end_station_id)
  )

# Summary of the combined dataset to check for NA values
summary(bikes) 
str(bikes) 

# Remove empty columns and rows just in case
bikes <- janitor::remove_empty(bikes, which = c("cols")) # Remove empty columns
bikes <- janitor::remove_empty(bikes, which = c("rows")) # Remove empty rows

# Check for missing values in start and end station names
bikes %>% filter(start_station_name == "") %>% count()  # Count missing start station names
bikes %>% filter(end_station_name == "") %>% count()    # Count missing end station names

# Remove rows where start or end station names are missing
bikes <- subset(bikes, start_station_name != "")
bikes <- subset(bikes, end_station_name != "")

# Convert start and end times to date-time format
bikes <- bikes %>% mutate(started_at = ymd_hms(started_at))
bikes <- bikes %>% mutate(ended_at = ymd_hms(ended_at))

# Calculate ride duration
bikes <- mutate(bikes, ride_duration = ended_at - started_at)

# Find the minimum and maximum ride durations
bikes %>% summarise(min(ride_duration), max(ride_duration))

# Remove rows where ride duration is not positive
bikes_clean <- subset(bikes, ride_duration > 0)

# Arrange the cleaned dataset by ride duration
bikes_clean %>% arrange(ride_duration)

# Create an executive summary table grouped by user type (member or casual)
executive_sum <- bikes_clean %>%
  group_by(member_casual) %>%
  summarize(n = n()) %>%
  mutate(percentage = n * 100 / sum(n))

# Plot user count comparison: member vs casual
ggplot(data = executive_sum, mapping = aes(x = member_casual, y = n, fill = member_casual)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "User Count: Member vs Casual", x = "User Type", y = "Number of Rides") +
  scale_y_continuous(labels = scales::comma_format()) +  # Format y-axis with commas
  theme_minimal()

# Extract date components (date, day, month, year) from the start time
bikes_clean <- mutate(bikes_clean,
                      date_of_trip = date(started_at), day = date(date_of_trip),
                      month = month(date_of_trip), year = year(date_of_trip))

# Extract weekday from the start date
bikes_clean <- mutate(bikes_clean, weekday = weekdays(date_of_trip))

# Convert weekdays to a factor to control order in plots
bikes_clean$weekday <- factor(bikes_clean$weekday, 
                              c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                "Friday", "Saturday", "Sunday"))

# Plot number of rides by weekday, comparing member and casual users
ggplot(data = bikes_clean) +
  geom_bar(mapping = aes(x = weekday, fill = member_casual), position = "dodge") +
  labs(
    title = "Member vs Casual: Ride amount by Weekday",
    subtitle = "Data from April 2020 to September 2020",
    x = "Day of the week",
    y = "Number of rides"
  ) +
  theme(axis.text.x = element_text(angle = 35, size = 8)) + # Rotate x-axis labels
  scale_y_continuous(labels = comma)

# Group by rideable type and user type, calculate number of rides and average duration
bikes_clean %>%
  group_by(rideable_type, member_casual) %>%
  summarise(number_of_rides = n() / 1000, 
            avg_duration = mean(ride_duration)) %>%
  drop_na() %>% # Remove any remaining missing values
  ggplot(aes(x = rideable_type, y = number_of_rides, fill = member_casual)) +
  geom_bar(stat = 'identity') +
  labs(title = 'The number of rides by rider type', x = 'Rideable Type', y = 'Number of Rides') +
  geom_col(position = "dodge") # Plot with dodged bars