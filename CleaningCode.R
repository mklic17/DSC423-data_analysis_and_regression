setwd('/Users/hieunguyenphi/Documents/DEPAUL UNI/DSC 423 - Data analysis and Regression/Project Group')
hotel_bookings <- read.csv('hotel_bookings.csv', header = T)
library(tidyverse)
dfCopy <- hotel_bookings

# Remove PreviousCancellations, PreviousBookingsNotCanceled, ReservationStatus, ReservationStatusDate
# Remove DistributionChannel, Agent, Company
dfCopy <- dfCopy %>%
  select(-previous_cancellations, -previous_bookings_not_canceled, -reservation_status, - reservation_status_date) %>%
  select(-distribution_channel, -agent, -company)

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

dfCopy <- dfCopy %>%
  mutate(arrival_date = as.Date(paste0(arrival_date_day_of_month,'-',arrival_date_month,
                                       '-',arrival_date_year), '%d-%B-%Y')) %>%
  mutate(season = case_when(arrival_date >= spring2015Start & arrival_date <= spring2015End ~ 'Spring',
                             arrival_date >= summer2015Start & arrival_date <= summer2015End ~ 'Summer',
                             arrival_date >= fall2015Start & arrival_date <= fall2015End ~ 'Fall',
                             arrival_date >= winter2015Start & arrival_date <= winter2015End ~ 'Winter',
                             arrival_date >= spring2016Start & arrival_date <= spring2016End ~ 'Spring',
                             arrival_date >= summer2016Start & arrival_date <= summer2016End ~ 'Summer',
                             arrival_date >= fall2016Start & arrival_date <= fall2016End ~ 'Fall',
                             arrival_date >= winter2016Start & arrival_date <= winter2016End ~ 'Winter',
                             arrival_date >= spring2017Start & arrival_date <= spring2017End ~ 'Spring',
                             arrival_date >= summer2017Start & arrival_date <= summer2017End ~ 'Summer',
                             arrival_date >= fall2017Start & arrival_date <= fall2017End ~ 'Fall',
                             arrival_date >= winter2017Start & arrival_date <= winter2017End ~ 'Winter',
                             TRUE ~ 'UNDEFINED'
                             ))

##################################################################################
# New Variable: country_of_origin_Portugal
# Represents if the traveler's country of Origin is in Portugal (1) or outside of Portugal (0)
##################################################################################

##################################################################################
# New Variable: reserved_matches_assigned_room
# Reprents if the Reserved Room and the AssigendRoom fields contain the same value
##################################################################################
################################################################################
# Clean Existing Variable Meals
# Combine SC and UNDEFINED as they are refer to the same value
################################################################################
##################################################################################
# Clean Existing Variable Children
# update NA values to be 0


dfCopy <- dfCopy %>%
  mutate(country_of_origin_Portugal = ifelse(country == 'PRT',1,0),
         reserved_matches_assigned_room = ifelse(reserved_room_type == assigned_room_type, 1,0),
         meal = ifelse(meal == 'UNDEFINED','SC',meal),
         children = ifelse(is.na(children),0,children),
         total_number_of_guests = adults + children + babies)

##################################################################################
# Data Omitting
# market_segment. Complementary and Undefined values

dfCopy <- dfCopy %>%
  filter(!market_segment %in% c('Complementary','Undefined')) %>%
  filter(adr != 0)

dfCopy <- dfCopy %>%
  select(-arrival_date_year, - arrival_date_day_of_month, -arrival_date_week_number, -arrival_date_year,
         -assigned_room_type, -country,-arrival_date, -arrival_date_month)
##################################################################################
# Clean Variables in Environment

rm(winter2015Start, winter2015End, spring2015Start, spring2015End, summer2015Start, summer2015End, fall2015Start, fall2015End, 
   winter2016Start, winter2016End, spring2016Start, spring2016End, summer2016Start, summer2016End, fall2016Start, fall2016End, 
   winter2017Start, winter2017End, spring2017Start, spring2017End, summer2017Start, summer2017End, fall2017Start, fall2017End
)

write.csv(dfCopy, 'dfCopy.csv')