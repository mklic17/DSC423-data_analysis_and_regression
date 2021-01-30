# Step 2
#File to show data visualization


# Separate blocks for easy reading and please ensure you comment your code

#1  hotel                           
#2  is_Cacelled
#3  lead_time
#4  arrival_date_year                       - REMOVE
#5  arrival_date_month                      - REMOVE
#6  arrival_date_week_number                - REMOVE
#7  arrival_date_day_of_month               - REMOVE
#8  stays_in_weekend_nights
#9  stays_in_week_nights
#10 adults
#11 children
#12 babies
#13 meal
#14 country                                 - REMOVE
#15 market_segment
#16 distribution_channel                    - REMOVE
#17 is_repeated_guest
#18 previous_cancellations                  - REMOVE
#19 previous_bookings_not_cancelled         - REMOVE
#20 reserved_room_type    
#21 assigned_room_type                      - REMOVE
#22 booking_changes
#23 deposit_type
#24 agent                                   - REMOVE
#25 company                                 - REMOVE
#26 days_in_waiting_list
#27 customer_type
#28 adr
#29 required_car_parking_spaces
#30 total_of_special_requests
#31 reservation_status                      - REMOVE
#32 reservation_status_date                 - REMOVE
#33 season
#34 country_of_origin_Portugal
#35 reserved_matches_assigned_room

###############################################################################
## removed the unused variables for calculations
data <-  hotel_bookings[,-c(4, 5, 6, 7, 14, 16, 18, 19, 21, 24, 25, 31, 32)] 
numericData <- hotel_bookings[,-c(1, 4, 5, 6, 7, 13, 14, 15, 16, 18, 19, 20, 21, 23, 24, 25, 27, 31, 32, 33)] 


cor(numericData)                    # Highest Numbers
# adults:                             0.230641216  *** more guests the higher the rate
# is_repeated_guest                  -0.134314447
# total_of_special_requests           0.17218526 
# country_of_origin_Portugal         -0.160481962
# reserved_matches_assigned_room      0.138132525


# get a random sample of 1000 entries from the data
numericDatasample <- numericData[sample(1:nrow(numericData), 1000, replace=FALSE),]
plot(numericDatasample)



