# File to Clean the hotel_bookings data



##################################################################################
# Removing Outliers
# https://www.r-bloggers.com/2020/01/how-to-remove-outliers-in-r/

boxplot(hotel_bookings$adr)

boxplot(hotel_bookings$adr, plot=FALSE)$out 
outliers <- boxplot(hotel_bookings$adr, plot=FALSE)$out # saves all the plots outside of the boxplot region
hotel_bookings <- hotel_bookings[-which(hotel_bookings$adr %in% outliers),] #saves the hotel_bookings data that are not In the outliers list

boxplot(hotel_bookings$adr)



##################################################################################
# Data Omitting
# market_segment Complementary and Undefined values

hotel_bookings <- subset(hotel_bookings, hotel_bookings$market_segment != "Complementary") # Remove Complementary Values from dataset
hotel_bookings <- subset(hotel_bookings, hotel_bookings$market_segment != "Undefined") # Remove Undefined Values from dataset

unique(hotel_bookings$market_segment) # no Complementary or Undefined remaining

##################################################################################
# New Variable: season
# Description Seasons based on (https://www.calendardate.com/) in format (MM-DD-YYYY)
##################################################################################

# 2015 - Winter (12-21-2014) - (03-20-2015)"2007-06-22"
winter2015Start <- as.Date("2014-12-21")
winter2015End   <- as.Date("2015-03-20")

# 2015 - Spring (03-20-2015) - (06-21-2015)
spring2015Start <- as.Date("2015-03-20")
spring2015End   <- as.Date("2015-06-21")

# 2015 - Summer (06-21-2015) - (09-23-2015)
summer2015Start <- as.Date("2015-06-21")
summer2015End   <- as.Date("2015-09-23")

# 2015 - Fall   (09-23-2015) - (12-22-2015)
fall2015Start <- as.Date("2015-09-23")
fall2015End   <- as.Date("2015-12-22")

# 2016 - Winter (12-22-2015) - (03-20-2016)
winter2016Start <- as.Date("2015-12-22")
winter2016End   <- as.Date("2016-03-20")

# 2016 - Spring (03-20-2016) - (06-20-2016)
spring2016Start <- as.Date("2016-03-20")
spring2016End   <- as.Date("2016-06-20")

# 2016 - Summer (06-20-2016) - (09-22-2016)
summer2016Start <- as.Date("2016-06-20")
summer2016End  <- as.Date("2016-09-22")

# 2016 - Fall   (09-22-2016) - (12-21-2016)
fall2016Start <- as.Date("2016-09-22")
fall2016End  <- as.Date("2016-12-21")

# 2017 - Winter (12-21-2016) - (03-20-2017)
winter2017Start <- as.Date("2016-12-21")
winter2017End   <- as.Date("2017-03-20")

# 2017 - Spring (03-20-2017) - (06-21-2017)
spring2017Start <- as.Date("2017-03-20")
spring2017End   <- as.Date("2017-06-21")

# 2017 - Summer (06-21-2017) - (09-22-2017)
summer2017Start <- as.Date("2017-06-21")
summer2017End   <- as.Date("2017-09-22")

# 2017 - Fall   (09-22-2017) - (12-21-2017)
fall2017Start <- as.Date("2017-09-22")
fall2017End   <- as.Date("2017-12-21")

# convertDate(Date) returns the Season in which the Date falls falls based on the above variables
convertDateToSeason <- function(providedDate) {
  if((providedDate >= winter2015Start & providedDate <= winter2015End) | (providedDate >= winter2016Start & providedDate <= winter2016End) | (providedDate >= winter2017Start & providedDate <= winter2017End) ) {
    result <- 'winter'
  }
  else if((providedDate >= spring2015Start & providedDate <= spring2015End) | (providedDate >= spring2016Start & providedDate <= spring2016End) | (providedDate >= spring2017Start & providedDate <= spring2017End)  ){
    result <- 'spring'
  }
  else if((providedDate >= summer2015Start & providedDate <= summer2015End) | (providedDate >= summer2016Start & providedDate <= summer2016End) | (providedDate >= summer2017Start & providedDate <= summer2017End)) {
    result <- 'summer'
  }
  else if((providedDate >= fall2015Start & providedDate <= fall2015End) | (providedDate >= fall2016Start & providedDate <= fall2016End) | (providedDate >= fall2017Start & providedDate <= fall2017End) ) {
    result <- 'fall'
  }
  else {
    result <- 'UNDEFINED'
  }
  return(result)
}

##################################################################################
# New Variable: season
# Represents the season in which the arrival date lands on
##################################################################################

# Step 1 - Convert Values (arrival_date_month, arrival_date_day_of_month, arrival_date_year) to DATE FORMAT
MonthDay <- paste(hotel_bookings$arrival_date_month, hotel_bookings$arrival_date_day_of_month, sep=" ", collapse = NULL) # concat ("Month Day")
MonthDayYear <- paste(MonthDay, hotel_bookings$arrival_date_year, sep=", ", collapse=NULL) # concat ("Month Day, Year")
convertedDate <- as.Date(MonthDayYear,format='%B %d, %Y') # Convert to date format

# Step 2 - create new column on hotel_bookings called 'Season' and input 'UNDEFINED' for the value
hotel_bookings$season <- "UNDEFINED"
hotel_bookings$season

# Step 3 - loop through each Element and Assign the hotel
x <- 1
for(i in convertedDate) {
  hotel_bookings$season[[x]] <- convertDateToSeason(i)
  x <- x + 1
}

hotel_bookings$season

##################################################################################
# New Variable: country_of_origin_Portugal
# Represents if the traveler's country of Origin is in Portugal (1) or outside of Portugal (0)
##################################################################################
# Step 1 - Create new column on hotel_bookings called 'CountryOfOriginInPortugal' and input 'UNDEFINED' for the value
hotel_bookings$country_of_origin_Portugal <- 0

# Step 2 - Conditional to set the value of 1 to Travelers whose country of Origin is Portugal and 0 if they are outside of Portugal
hotel_bookings$country_of_origin_Portugal <- ifelse(hotel_bookings$country == 'PRT', 1, 0)
hotel_bookings$country_of_origin_Portugal


##################################################################################
# New Variable: reserved_matches_assigned_room
# Reprents if the Reserved Room and the AssigendRoom fields contain the same value
##################################################################################
# Step 1 - Create new column on hotel_bookings called 'CountryOfOriginInPortugal' and input 'UNDEFINED' for the value
hotel_bookings$reserved_matches_assigned_room <- 0

# Step 2 - Conditional to set the value of 1 to Travelers whose country of Origin is Portugal and 0 if they are outside of Portugal
hotel_bookings$reserved_matches_assigned_room <- ifelse(hotel_bookings$reserved_room_type == hotel_bookings$assigned_room_type, 1, 0)

hotel_bookings$reserved_matches_assigned_room


################################################################################
# Clean Existing Variable Meals
# Combine SC and UNDEFINED as they are refer to the same value
################################################################################
hotel_bookings$meal <- ifelse(hotel_bookings$meal == 'UNDEFINED', 'SC', hotel_bookings$meal)

# Validating the update
#for(i in hotel_bookings$meal ) {
#  if(i == 'UNDEFINED') {
#    print(i)
#  }
#}

hotel_bookings$meal

##################################################################################
# Clean Existing Variable Children
# update NA values to be 0

hotel_bookings$children[is.na(hotel_bookings$children)] = 0



##################################################################################
# Create new Variable: total_number_of_guests
# Sums the total number of children, adults, babies in a room

hotel_bookings$total_number_of_guests <- NA

x <- 1
for(i in hotel_bookings$total_number_of_guests) {
  hotel_bookings$total_number_of_guests[[x]] <- sum(hotel_bookings$children[[x]], hotel_bookings$adults[[x]], hotel_bookings$babies[[x]] )
  x <- x + 1
}

hotel_bookings$total_number_of_guests


##################################################################################
# Clean Variables in Environment

rm(winter2015Start, winter2015End, spring2015Start, spring2015End, summer2015Start, summer2015End, fall2015Start, fall2015End, 
   winter2016Start, winter2016End, spring2016Start, spring2016End, summer2016Start, summer2016End, fall2016Start, fall2016End, 
   winter2017Start, winter2017End, spring2017Start, spring2017End, summer2017Start, summer2017End, fall2017Start, fall2017End
)
rm(outliers, i, x, MonthDay, MonthDayYear, convertedDate)




