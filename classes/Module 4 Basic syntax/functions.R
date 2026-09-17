library(dplyr)

storms

hurricanes <- storms[storms$status == "hurricane", ]
hurricanes <- hurricanes[, c("name", "year", "month", 
                             "day", "category",
                             "pressure","wind")]



classify_wind <- function(wind) { 
  if (wind < 80) {
    "Low"
  } else if (wind < 110) {
    "Moderate"
  } else {
    "High"
  }
}


hurricanes$wind_class <- sapply(
  hurricanes$wind,
  classify_wind
)


table(hurricanes$wind_class)



# Create a function called "classify_wind"
# The function takes "wind" as its input
classify_wind <- function(wind) {
  
  # If wind speed is less than 80 knots, return "Low"
  if (wind < 80) {
    "Low"
    
    # Otherwise, if wind speed is less than 110 knots, return "Moderate"
  } else if (wind < 110) {
    "Moderate"
    
    # If neither condition is true, return "High"
    # This means wind speed is 110 knots or greater
  } else {
    "High"
  }
}


# Function ----------------------------------------------------------------
is_major <- function(category) {
  if (category >= 3) {
    return("Yes")
  } else {
    return("No")
  }
}


hurricanes$major <- sapply(
  hurricanes$category,
  is_major
)

head(hurricanes)
tail(hurricanes)
table(hurricanes$major)
