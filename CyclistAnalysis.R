#combining all 13 csv files
#but it was a mistake

data_all <- list.files(path="C:/Users/ZEYNEP/datasci/case1/cyclistdata", pattern="*.csv", full.names=TRUE)%>%
  lapply(read_csv)%>%
  bind_rows
data_all

#initial data cleaning and manipulation

data_all <- data_all %>%
  select(-start_station_name, -start_station_id, -end_station_name, -end_station_id)
na_count_per_column <- colSums(is.na(data_all))
total_na_count <- sum(na_count_per_column)
##total 13700 cells empty
data_all <- na.omit(data_all)

#seperating date and time in the columns started_at and ended_at and formatting them
data_all <- data_all %>%
  separate(started_at, into = c("start_date", "start_time"), sep = " ") %>%
  separate(ended_at, into = c("end_date", "end_time"), sep = " ")
str(data_all)
data_all <- data_all %>%
  mutate(
    start_date=as.Date(start_date),
    end_date=as.Date(end_date)
  )

data_all <- data_all %>%
  mutate(
    start_time=as_hms(start_time),
    end_time=as_hms(end_time)
  )

#ride_duration column
#Turned to columns' data types into their appropriate formats.(Dates as YYMMMDD and time are hours, minutes and seconds.)
#Calculated the ride duration by substracting the end_time from start_time. Since not all rides started and ended at the same day some duration values came out as negative and incorrect.
#So i only calculated duration for the same day rides.
#The number of the rows i did not include corresponds to 0.4 percent of my data.

data_all <- data_all %>% 
  mutate(
    ride_duration= end_time-start_time, 
    ride_duration=as_hms(ride_duration)
  )
same_day_rides <- sum(data_all$start_date==data_all$end_date)
data_all$same_day_rides <- data_all$start_date==data_all$end_date

data_all <- data_all[data_all$same_day_rides==TRUE, ]
data_all <- data_all %>%
  select(-same_day_rides)

#day of the week column
data_all$day_of_the_week <- weekdays(data_all$start_date)

#saved the cleaned version as a csv file
write.csv(data_all, file="C:/Users/ZEYNEP/datasci/case1/data_all.csv", row.names=FALSE)

##exploratory data analysis

#annual-casual percentage pie
annual_members <- data_all %>%
  filter(!(year == 2022 & month == 6))%>%
  filter(member_casual == "member")
  
casual_riders <- data_all%>%
  filter(!(year == 2022 & month == 6))%>%
  filter(member_casual =="casual")
membership_counts <- c(n_distinct(casual_riders$ride_id),n_distinct(annual_members$ride_id))
membership_labels <- c("casual", "annual")
percentage<- round(100*membership_counts/sum(membership_counts),1)

p<-pie(membership_counts, labels = percentage, main = "Biker Membership Distribution",col = rainbow(length(membership_counts)))
legend("topright", c("casual","annual"), cex = 0.8,
       fill = rainbow(length(membership_counts)))
ggsave("membership_percentage.png", plot = p)

#monthly subscription trends

monthly_totals_annual <- annual_members %>%
  group_by(month) %>%
  summarise(TotalRides = n())
monthly_totals_annual$MonthName <- month.name[monthly_totals_annual$month]
monthly_totals_annual$MonthName <- factor(monthly_totals_annual$MonthName, levels = month.name, ordered = TRUE)

monthly_totals_casual <- casual_riders %>%
  group_by(month) %>%
  summarise(TotalRides = n())
monthly_totals_casual$MonthName <- month.name[monthly_totals_casual$month]
monthly_totals_casual$MonthName <- factor(monthly_totals_casual$MonthName, levels = month.name, ordered = TRUE)

ggplot( monthly_totals_annual, aes(x = MonthName, y = TotalRides)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Monthly Total Annual Membership Rides",
       x = "Month",
       y = "Total Rides") +
  scale_x_discrete(labels = monthly_totals_annual$MonthName) 
  theme_minimal() 

ggplot( monthly_totals_casual, aes(x = MonthName, y = TotalRides)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Monthly Total Casual Rides",
       x = "Month",
       y = "Total Rides") +
  scale_x_discrete(labels = monthly_totals_casual$MonthName) 
theme_minimal() 

#ride counts depending on the weekday
weekday_trends <- data_all %>%
  group_by(day_of_the_week, member_casual) %>%
  summarise(TotalRides = n_distinct(ride_id))%>%
  pivot_wider(names_from=member_casual, values_from = TotalRides, values_fill=0)
weekday_trends$day_of_the_week <- factor(weekday_trends$day_of_the_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), ordered = TRUE)
weekday_trends <- weekday_trends%>%
  gather(key = "type", value = "value", casual, member)
ggplot(weekday_trends, aes(x = day_of_the_week, y = value, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Day of the Week", y = "Count", title = "Casual vs Member Ridership by Day of the Week") +
  scale_fill_manual(values = c("casual" = "blue", "member" = "orange")) +
  theme_minimal()

#yearly_trends
yearly_trends<-data_all %>%
  group_by(year,member_casual)%>%
  summarise(TotalRides=n_distinct(ride_id))%>%
  pivot_wider(names_from=member_casual, values_from = TotalRides, values_fill=0)
yearly_trends <- yearly_trends%>%
  gather(key = "type", value = "value", casual, member)
ggplot(yearly_trends, aes(x = year, y = value, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", y = "Count", title = "Yearly Trends") +
  scale_fill_manual(values = c("casual" = "blue", "member" = "green")) +
  theme_minimal()

#annual_memberships bike type vs casual_riders bike type
bike_type <- data_all%>%
  group_by(rideable_type, member_casual)%>%
  summarise(TotalRides =n_distinct(ride_id))%>%
  pivot_wider(names_from=member_casual, values_from = TotalRides, values_fill=0)
bike_type <- bike_type%>%
  gather(key = "type", value = "value", casual, member)
ggplot(bike_type, aes(x = rideable_type, y = value, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Bike Type", y = "Count", title = "Rideable Preference") +
  scale_fill_manual(values = c("casual" = "blue", "member" = "green")) +
  theme_minimal()



