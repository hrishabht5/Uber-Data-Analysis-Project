install.packages("ggthemes")
install.packages("lubridate")
install.packages("tidyr")
install.packages("DT")
install.packages("scales")

library("ggplot2")      # ggplot2 is the most popular data visualization library that is most widely used for creating aesthetic visualization plots.
library("ggthemes")     # This is more of an add-on to our main ggplot2 library. With this, we can create better create extra themes and scales with the mainstream ggplot2 package.
library("lubridate")    # Our dataset involves various time-frames. In order to understand our data in separate time categories, we will make use of the lubridate package.
library("dplyr")        # This package is the lingua franca of data manipulation in R.
library("tidyverse")    # This package will help you to tidy your data.
library("DT")           # With the help of this package, we will be able to interface with the JavaScript Library called – Datatables.
library("scales")       # With the help of graphical scales, we can automatically map the data to the correct scales with well-placed axes and legends.

apr_data <- read.csv("C:/Users/hrish/OneDrive/Desktop/Uber Data Analysis Project/uber-raw-data-apr14.csv")
apr_data

may_data <- read.csv("C:/Users/hrish/OneDrive/Desktop/Uber Data Analysis Project/uber-raw-data-may14.csv")
may_data

jun_data <- read.csv("C:/Users/hrish/OneDrive/Desktop/Uber Data Analysis Project/uber-raw-data-jun14.csv")
jun_data

jul_data <- read.csv("C:/Users/hrish/OneDrive/Desktop/Uber Data Analysis Project/uber-raw-data-jul14.csv")
jul_data

aug_data <- read.csv("C:/Users/hrish/OneDrive/Desktop/Uber Data Analysis Project/uber-raw-data-aug14.csv")
aug_data

sep_data <- read.csv("C:/Users/hrish/OneDrive/Desktop/Uber Data Analysis Project/uber-raw-data-sep14.csv")
sep_data

data <- rbind(apr_data, may_data, jun_data, aug_data, sep_data)
data

#The data contains the columns Date.Time which is a factor, Latitude and Longitudes which are double and Base which is factor. we will format the datetime into a more readable format using the Date Time conversion function.

data$Date.Time <- as.POSIXct(data$Date.Time, format = "%m/%d/%Y %H:%M:%S")

data$time <- as.POSIXct(data$Date.Time, format = "%m/%d/%Y %H:%M:%S")

data$Date.Time <- ymd_hms(data$Date.Time)

# Create individual columns for month day and year

data$day <- factor(day(data$Date.Time))
data$month <- factor(month(data$Date.Time))
data$year <- factor(year(data$Date.Time))
data$dayofweek <- factor(wday(data$Date.Time, label = TRUE))

# Add Time variables as well 

data$second <- factor(second(data$time))
data$minute <-factor(minute(data$time))
data$hour <- factor(hour(data$time))

data

hourly_data <- data %>% 
  group_by(hour) %>% 
  dplyr::summarize(Total = n())

# Shows data in a searchable js table

datatable(hourly_data)

# Plot the data by hour

ggplot(hourly_data, aes(hour, Total)) +
  geom_bar(stat = "identity",
           fill = "steelblue",
           color = "red")  +
  ggtitle("Trips every hour", subtitle = "aggregate today") + 
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) + 
  scale_y_continuous(labels=comma)

# Aggregate the data by month and hour

monthly_data <- data %>% 
  group_by(month)  %>%
  dplyr::summarize(Total = n())

# Shows data in a searchable js table

datatable(monthly_data)

# Aggregate data by day of the month 


day_data <- data %>% group_by(day) %>% dplyr::summarize(Trips = n())
day_data

ggplot(day_data, aes(day, Trips)) + 
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Trips by day of the month") + 
  theme(legend.position = "none") + 
  scale_y_continuous(labels = comma)


# Collect data by day of the week and month

day_month_data <- data %>% group_by(dayofweek, month) %>% dplyr::summarize(Trips = n())
day_month_data

# Plot the above data
ggplot(day_month_data, aes(dayofweek, Trips, fill = month)) + 
  geom_bar(stat = "identity", aes(fill = month), position = "dodge") + 
  ggtitle("Trias by Day and Month") + 
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = colors)

# Number of Trips place during months in a year

month_data <- data %>% group_by(month) %>% dplyr::summarize(Total = n())
month_data

ggplot(month_data, aes(month, Total, fill = month)) + 
  geom_bar(stat = "Identity") + 
  ggtitle("Trips in a month") + 
  theme(legend.position = "none") + 
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = colors)
view(data)
save(data)
save.image(data)
