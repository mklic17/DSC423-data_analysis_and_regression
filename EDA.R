library(tidyverse)
library(hrbrthemes)
library(ggpubr)
library(reshape2)
library(corrplot)
str(dfCopy) #117366
# Total 22 variables: 10 numeric variables, 5 transformed categorical, 7 categorical

numDF <- dfCopy %>%
  select(lead_time,stays_in_weekend_nights,stays_in_week_nights, adults,
         children,babies, days_in_waiting_list, adr, total_of_special_requests,booking_changes)

transCatDF <- dfCopy %>%
  select(is_canceled, is_repeated_guest,
         country_of_origin_Portugal, reserved_matches_assigned_room)

catDF <- dfCopy %>%
  select(where(is.character))
#######################
# UNIVARIATE ANALYSIS #
#######################
p1 <- numDF %>%
  ggplot(aes(x=as.numeric(numDF[,1]))) +
  geom_histogram() +
  theme_ipsum() +
  labs(x = toupper(names(numDF)[1]))

p2 <- numDF %>%
  ggplot(aes(x=as.numeric(numDF[,2]))) +
  geom_histogram() +
  theme_ipsum() +
  labs(x = toupper(names(numDF)[2]))

p3 <- numDF %>%
  ggplot(aes(x=as.numeric(numDF[,3]))) +
  geom_histogram() +
  theme_ipsum() +
  labs(x = toupper(names(numDF)[3]))

p4 <- numDF %>%
  ggplot(aes(x=as.numeric(numDF[,4]))) +
  geom_histogram() +
  theme_ipsum() +
  labs(x = toupper(names(numDF)[4]))

p5 <- numDF %>%
  ggplot(aes(x=as.numeric(numDF[,5]))) +
  geom_histogram() +
  theme_ipsum() +
  labs(x = toupper(names(numDF)[5]))

p6 <- numDF %>%
  ggplot(aes(x=as.numeric(numDF[,6]))) +
  geom_histogram() +
  theme_ipsum() +
  labs(x = toupper(names(numDF)[6]))

p7 <- numDF %>%
  ggplot(aes(x=as.numeric(numDF[,7]))) +
  geom_histogram() +
  theme_ipsum() +
  labs(x = toupper(names(numDF)[7]))

p8 <- numDF %>%
  ggplot(aes(x=as.numeric(numDF[,8]))) +
  geom_histogram() +
  theme_ipsum() +
  labs(x = toupper(names(numDF)[8]))

p9 <- numDF %>%
  ggplot(aes(x=as.numeric(numDF[,9]))) +
  geom_histogram() +
  theme_ipsum() +
  labs(x = toupper(names(numDF)[9]))
p10 <- numDF %>%
  ggplot(aes(x=as.numeric(numDF[,10]))) +
  geom_histogram() +
  theme_ipsum() +
  labs(x = toupper(names(numDF)[10]))

ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10, ncol = 3)

temp <- melt(numDF)
levels(temp$variable) <- toupper(levels(temp$variable))
temp %>%
  ggplot(aes(x= variable, y = value)) + 
  geom_boxplot() +
  theme_ipsum() +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
  labs(y ="") +
  coord_flip()


# leadtime: skewed to right
a <-as.matrix(summary(numDF$lead_time))
temp <- data.frame(Variable = 'lead_time',
              Min = a[1],FQu. = a[2],Med = a[3],Mean = a[4],TQu. = a[5],Max = a[6])
for (i in 2:ncol(numDF)){
  a <-as.matrix(summary(numDF[,i]))
  temp <- rbind(temp,c(names(numDF)[i],a[1],a[2],a[3],a[4],a[5],a[5]))
}


outliers <- boxplot(dfCopy$adr, plot=FALSE)$out # saves all the plots outside of the boxplot region
dfCopy <- dfCopy %>% filter(!adr %in% outliers) #saves the hotel_bookings data that are not In the outliers list
dfCopy %>%
  ggplot(aes(adr)) +
  geom_histogram(aes(y=..density..),color = 'white', binwidth = 3, fill = 'darkgrey') +
  geom_density(aes(y=..density..), color = 'darkred') +
  stat_function(fun = dnorm, n = ncol(dfCopy), args = list(mean = mean(dfCopy$adr), 
                                                           sd = sd(dfCopy$adr)))+
  theme_ipsum() +
  labs(x = 'ADR')

dfCopy %>%
  ggplot(aes(x= adr)) + 
  geom_boxplot() +
  theme_ipsum() +
  #theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
  labs(y ="", x = 'ADR') +
  coord_flip()

p1 <- transCatDF %>%
  ggplot(aes(x = factor(is_canceled))) +
  geom_bar(color = 'white', fill = 'darkgrey') +
  theme_ipsum() +
  labs(x = 'IS_CANCELED')

p2 <- transCatDF %>%
  ggplot(aes(x = factor(is_repeated_guest))) +
  geom_bar(color = 'white', fill = 'darkgrey') +
  theme_ipsum() +
  labs(x = toupper('is_repeated_guest'))

p3 <- transCatDF %>%
  ggplot(aes(x = factor(required_car_parking_spaces))) +
  geom_bar(color = 'white', fill = 'darkgrey') +
  theme_ipsum() +
  labs(x = toupper('required_car_parking_spaces'))
summary(transCatDF$required_car_parking_spaces)
p4 <- transCatDF %>%
  ggplot(aes(x = factor(country_of_origin_Portugal))) +
  geom_bar(color = 'white', fill = 'darkgrey') +
  theme_ipsum() +
  labs(x = toupper('country_of_origin_Portugal'))

p5 <- transCatDF %>%
  ggplot(aes(x = factor(reserved_matches_assigned_room))) +
  geom_bar(color = 'white', fill = 'darkgrey') +
  theme_ipsum() +
  labs(x = toupper('reserved_matches_assigned_room'))
p6 <- catDF %>%
  ggplot(aes(x = factor(hotel))) +
  geom_bar(color = 'white', fill = 'darkgrey') +
  theme_ipsum() +
  labs(x = toupper('hotel'))

p7 <- catDF %>%
  ggplot(aes(x = factor(meal))) +
  geom_bar(color = 'white', fill = 'darkgrey') +
  theme_ipsum() +
  labs(x = toupper('meal'))

p8 <- catDF %>%
  ggplot(aes(x = factor(market_segment))) +
  geom_bar(color = 'white', fill = 'darkgrey') +
  theme_ipsum() +
  labs(x = toupper('market_segment'))

p9 <- catDF %>%
  ggplot(aes(x = factor(reserved_room_type))) +
  geom_bar(color = 'white', fill = 'darkgrey') +
  theme_ipsum() +
  labs(x = toupper('reserved_room_type'))

p10 <- catDF %>%
  ggplot(aes(x = factor(deposit_type))) +
  geom_bar(color = 'white', fill = 'darkgrey') +
  theme_ipsum() +
  labs(x = toupper('deposit_type'))

p10 <- catDF %>%
  ggplot(aes(x = factor(customer_type))) +
  geom_bar(color = 'white', fill = 'darkgrey') +
  theme_ipsum() +
  labs(x = toupper('customer_type'))

p11 <- catDF %>%
  ggplot(aes(x = factor(season))) +
  geom_bar(color = 'white', fill = 'darkgrey') +
  theme_ipsum() +
  labs(x = toupper('season'))

ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12, ncol =2, nrow = 6)

numDF <- dfCopy %>%
  select(lead_time,stays_in_weekend_nights,stays_in_week_nights, adults,
         children,babies, days_in_waiting_list, adr, total_of_special_requests,booking_changes)

corrplot(numDF, method="circle")
df <- cor(numDF)
df <- as.data.frame(df)
colnames(df) <- c('X1','X2','X3','X4','X5','X6','X7','X8','X9','X10')

numDF %>%
  ggplot(aes(y = sqrt(adr),x = (lead_time))) +
  geom_point(color = 'grey')
cor(numDF$adults + numDF$children + numDF$babies, numDF$adr)


p1 <- dfCopy %>%
  ggplot(aes(adr, group = is_canceled)) +
  geom_boxplot() +
  theme_ipsum() +
  labs(y = toupper('is_canceled'), x = 'ADR') +
  coord_flip()

p2 <- dfCopy %>%
  ggplot(aes(adr, group = is_repeated_guest)) +
  geom_boxplot() +
  theme_ipsum() +
  labs(y = toupper('is_repeated_guest'), x = 'ADR') +
  coord_flip()

p3 <- dfCopy %>%
  ggplot(aes(adr, group = country_of_origin_Portugal)) +
  geom_boxplot() +
  theme_ipsum() +
  labs(y = toupper('country_of_origin_Portugal'), x = 'ADR') +
  coord_flip()

p4 <- dfCopy %>%
  ggplot(aes(adr, group = reserved_matches_assigned_room)) +
  geom_boxplot() +
  theme_ipsum() +
  labs(y = toupper('reserved_matches_assigned_room'), x = 'ADR') +
  coord_flip()

p5 <- dfCopy %>%
  ggplot(aes(adr, group = hotel)) +
  geom_boxplot() +
  theme_ipsum() +
  labs(y = toupper('hotel'), x = 'ADR') +
  coord_flip()

p6 <- dfCopy %>%
  ggplot(aes(adr, group = meal)) +
  geom_boxplot() +
  theme_ipsum() +
  labs(y = toupper('meal'), x = 'ADR') +
  coord_flip()

p7 <- dfCopy %>%
  ggplot(aes(adr, group = market_segment)) +
  geom_boxplot() +
  theme_ipsum() +
  labs(y = toupper('market_segment'), x = 'ADR') +
  coord_flip()

p8 <- dfCopy %>%
  ggplot(aes(adr, group = reserved_room_type)) +
  geom_boxplot() +
  theme_ipsum() +
  labs(y = toupper('reserved_room_type'), x = 'ADR') +
  coord_flip()

p9 <- dfCopy %>%
  ggplot(aes(adr, group = deposit_type)) +
  geom_boxplot() +
  theme_ipsum() +
  labs(y = toupper('deposit_type'), x = 'ADR') +
  coord_flip()

p10 <- dfCopy %>%
  ggplot(aes(adr, group = customer_type)) +
  geom_boxplot() +
  theme_ipsum() +
  labs(y = toupper('customer_type'), x = 'ADR') +
  coord_flip()

p11 <- dfCopy %>%
  ggplot(aes(adr, group = season)) +
  geom_boxplot() +
  theme_ipsum() +
  labs(y = toupper('season'), x = 'ADR') +
  coord_flip()
catnam <- c(names(transCatDF), names(catDF))
res.aov <- summary(aov(adr ~ hotel, data = dfCopy))
res.aov <- res.aov[[1]]
anovaDf <- data.frame(Var = 'hotel' ,Fvalue = res.aov[1,4],PVal = res.aov[1,5])
for (i in catnam) {
  df <- data.frame(target = dfCopy$adr, catVar = dfCopy[i])
  colnames(df) <- c('target','catVar')
  res.aov <- summary(aov(target ~ catVar, data = df))
  res.aov <- res.aov[[1]]
  anovaDf <- rbind(anovaDf,data.frame(Var = i ,Fvalue = res.aov[1,4],PVal = res.aov[1,5]))
}

anovaDf <- anovaDf %>%
  distinct()

head(dfCopy[i])
res.aov <- summary(aov(adr ~ hotel, data = dfCopy))
res.aov <- res.aov[[1]]
res.aov[1,]
