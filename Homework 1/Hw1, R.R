library(dplyr)
library(lubridate)


# Part 1: Data Cleaning and Exploration
# Load the dataset
df <- read.csv("crime_data.csv")
print(head(df, 5))

# Identify columns with missing values and their respective counts. Drop columns where more than
# 50% of the data is missing
missing_counts <- colSums(is.na(df))
missing_counts[missing_counts > 0]

threshold <- nrow(df) * 0.5
new_df <- df[, colSums(is.na(df)) <= threshold]

# Convert the DATE OCC column to a datetime format. Extract the year, month, and day into separate
# columns. Create a new column for the hour using the TIME OCC column.
new_df$DATE.OCC <- as.Date(df$DATE.OCC, format="%m/%d/%Y")

new_df$Year <- year(new_df$DATE.OCC)
new_df$Month <- month(new_df$DATE.OCC)
new_df$Day <- day(new_df$DATE.OCC)

new_df$Hour <- as.numeric(substr(sprintf("%04d", new_df$TIME.OCC), 1, 2))

# Filter the dataset for crimes that occurred in 2023. Further filter crimes with the description
# BURGLARY in the Crm Cd Desc column
new_df_2023 <- new_df %>% filter(Year == 2023)

new_df_burglary_2023 <- new_df_2023 %>% filter(toupper(Crm.Cd.Desc) == "BURGLARY")

# Group the data by AREA NAME and calculate the total number of crimes and the average victim age.
# Sort the results by total crimes in descending order
crime_stats <- df %>%
  group_by(AREA.NAME) %>%
  summarise(
    Total_Crimes = n(),
    Avg_Victim_Age = mean(Vict.Age, na.rm = TRUE)
  ) %>%
  arrange(desc(Total_Crimes))

# Part 3: Further Exploration 

# Group the data by Month and count the number of crimes
monthly_crime_counts <- new_df %>%
  group_by(Month) %>%
  summarise(Total_Crimes = n())

# Count the number of crimes where a weapon was used (Weapon Used Cd is not null).
count_crimes_with_weapon <- sum(!is.na(df$Weapon.Used.Cd))

# Group the data by Premis Desc and count the number of crimes.
premis_crime_counts <- df %>%
  group_by(Premis.Desc) %>%
  summarise(Total_Crimes = n())

# Part 4: Advanced Analysis
new_df <- df %>%
  mutate(Severity.Score = case_when(
    !is.na(Weapon.Used.Cd) ~ 5, 
    Crm.Cd.Desc == "BURGLARY" ~ 3,
    TRUE ~ 1
  ))

area_severity_scores <- new_df %>%
  group_by(AREA.NAME) %>%
  summarise(Total_Severity_Score = sum(Severity.Score))

# Bonus Part:
# Use the LAT and LON columns to identify crimes that occurred within a specific latitude-longitude bounding
# box (e.g., downtown area).


# Latitude and Longitude values for the bounding box
lat_min <- 34.0
lat_max <- 34.1
lon_min <- -118.3
lon_max <- -118.2

downtown_crimes <- new_df %>%
  filter(LAT >= lat_min & LAT <= lat_max & LON >= lon_min & LON <= lon_max)