# Load required packages
library(dplyr)


# Read in Airline Reviews Dataset
reviews <- read.csv("Airline_review.csv")

# Explore data
head(reviews)
summary(reviews)
str(reviews)


# List of airlines to keep
airlines <- c("American Airlines", "Delta Air Lines", "United Airlines", "LATAM Airlines", 
                      "British Airways", "Lufthansa", "Emirates", "ANA All Nippon Airways", 
                      "China Southern Airlines", "Singapore Airlines")


# Filter the data set to keep only reviews of specified airlines
reviews_filtered <- subset(reviews, Airline.Name %in% airlines)


# Explore filtered data
head(reviews_filtered)
summary(reviews_filtered)
str(reviews_filtered)


################################### CLEANING & PREPARING THE DATA ###################################

# Remove any possible duplicates
duplicated(reviews_filtered)
## No duplicates found


# Remove unnecessary columns
reviews_filtered_cleaned <- subset(reviews_filtered, select = -c(X))


# Rename columns for consistency
colnames(reviews_filtered_cleaned) <- c("Airline_Name", "Overall_Rating", 
                                        "Review_Title", "Review_Date", 
                                        "Verified", "Review", 
                                        "Aircraft", "Type_Of_Traveller", 
                                        "Seat_Type", "Route", 
                                        "Date_Flown", "Seat_Comfort", 
                                        "Cabin_Staff_Service", 
                                        "Food_Beverages", "Ground_Service", 
                                        "Inflight_Entertainment", "Wi-Fi", 
                                        "Value_For_Money", "Recommended")


# Remove extra spaces from Route column
reviews_filtered_cleaned$Route <- trimws(reviews_filtered_cleaned$Route, "left")


# Create departure and destination city columns
# Split the "Route" column into Departure_City and Destination_City
route_split <- strsplit(as.character(reviews_filtered_cleaned$Route), " to ")

# Extract Departure_City and Destination_City
reviews_filtered_cleaned$Departure_City <- sapply(route_split, function(x) x[1])
reviews_filtered_cleaned$Destination_City <- sapply(route_split, function(x) x[2])


# Create departure country column
# Load dataset containing cities
cities <- read.csv("worldcities.csv")

# Remove smaller cities with the same names
cities_unique <- cities[!duplicated(cities$city_ascii), ]

# Merge datasets based on departure city
merged_data <- merge(reviews_filtered_cleaned, cities_unique, by.x = "Departure_City", by.y = "city_ascii", all.x = TRUE)


# Export airlines data
airlines_names <- c("American_Airlines", "Delta_Air_Lines", "United_Airlines", 
                    "LATAM_Airlines", "British_Airways", "Lufthansa", "Emirates", 
                    "ANA_All_Nippon_Airways", "China_Southern_Airlines", "Singapore_Airlines")

# Create separate datasets for each airline
for (airline in airlines_names) {
  # Filter data for current airline
  airline_data <- merged_data[merged_data$Airline_Name == airline, ]
  
  # Write airline-specific dataset to a CSV file
  write.csv(airline_data, file = paste0(airline, "_data.csv"), row.names = FALSE)
}


############################################# ANALYSIS #############################################


###################### AVERAGE RATINGS FOR EACH AIRLINE ######################

# Convert columns to numeric (if they're not already)
numeric_columns <- c("Overall_Rating", "Cabin_Staff_Service", "Inflight_Entertainment", "Food_Beverages", "Ground_Service", "Seat_Comfort", "Value_For_Money")
reviews_filtered_cleaned[, numeric_columns] <- lapply(reviews_filtered_cleaned[, numeric_columns], function(x) as.numeric(as.character(x)))

# Calculate the average rating for each metric and filter the results
average_ratings_by_metric <- lapply(numeric_columns, function(metric) {
  average_ratings <- aggregate(get(metric) ~ Airline_Name, data = reviews_filtered_cleaned, FUN = mean, na.rm = TRUE)
  average_ratings <- average_ratings[average_ratings$Airline_Name %in% airlines, ]
  return(average_ratings)
})

# Print the average ratings for each metric
names(average_ratings_by_metric) <- numeric_columns
for (metric in numeric_columns) {
  cat("Average ratings for", metric, ":\n")
  print(average_ratings_by_metric[[metric]])
  cat("\n")
}


######################### AVERAGE RATING BY AIRCRAFT ########################

# Calculate the average rating for each aircraft
average_ratings_by_aircraft <- aggregate(Overall_Rating ~ Aircraft, data = reviews_filtered_cleaned, FUN = mean, na.rm = TRUE)

# Sort the average ratings by descending order
average_ratings_by_aircraft <- average_ratings_by_aircraft[order(average_ratings_by_aircraft$Overall_Rating, decreasing = TRUE), ]

# Create the bar chart
barplot(average_ratings_by_aircraft$Overall_Rating, names.arg = average_ratings_by_aircraft$Aircraft, 
        main = "Average Ratings by Aircraft", xlab = "Aircraft", ylab = "Average Rating",
        col = "skyblue", border = "black", las = 2)

